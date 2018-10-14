-module(alexander).

%% API exports
-export([parse_transform/2, check/0, enter/1, exit/0]).

%%====================================================================
%% API functions
%%====================================================================

parse_transform(AST, _Options) ->
    put(targets, [{gen_server, call, 2},
                  {gen_server, call, 3},
                  {gen_fsm, sync_send_event, 2},
                  {gen_fsm, sync_send_event, 3},
                  {gen_fsm, sync_send_all_state_event, 2},
                  {gen_fsm, sync_send_all_state_event, 3},
                  {gen_statem, call, 2},
                  {gen_statem, call, 3},
                  {sys, get_state, 1},
                  {sys, get_state, 2},
                  {sys, get_status, 1},
                  {sys, get_status, 2},
                  {gen_event, sync_notify, 2},
                  {gen_event, which_handlers, 1}
                 ]),
    walk_ast([], AST).

check() ->
    true.

enter({TargetMod, TargetFun, Module, Function, Line, Destination} = Signature) ->
    io:format("~p entering blocking call to ~s:~s in function ~s:~s line ~b with destination ~p~n", [self(), TargetMod, TargetFun, Module, Function, Line, Destination]),
    %% search for someone in the chain that is calling us
    check_loop(self(), Destination),
    erlang:put('__alexander', Signature),
    ok.

exit() ->
    io:format("~p exiting blocking call~n", [self()]),
    erlang:erase('__alexander'),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

walk_ast(Acc, []) ->
    lists:reverse(Acc);
walk_ast(Acc, [{attribute, _, module, Module}=H|T]) ->
    put(module, Module),
    walk_ast([H|Acc], T);
walk_ast(Acc, [{function, Line, Name, Arity, Clauses}|T]) ->
    put(function, Name),
    walk_ast([{function, Line, Name, Arity,
                walk_clauses([], Clauses)}|Acc], T);
walk_ast(Acc, [H|T]) ->
    walk_ast([H|Acc], T).

walk_clauses(Acc, []) ->
    lists:reverse(Acc);
walk_clauses(Acc, [{clause, Line, Arguments, Guards, Body}|T]) ->
    walk_clauses([{clause, Line, Arguments, Guards, walk_body([], Body)}|Acc], T).

walk_body(Acc, []) ->
    lists:reverse(lists:flatten(Acc));
walk_body(Acc, [H|T]) ->
    walk_body([transform_statement(H, get(targets))|Acc], T).

transform_statement({call, Line, {remote, _Line1, {atom, _Line2, Module},
                                  {atom, _Line3, Function}}, Arguments0} = Stmt,
                    Targets) ->
    Signature = {Module, Function, length(Arguments0)},
    case lists:member(Signature, Targets) of
        true ->
            {Destination, NewStmt} = extract(Signature, Line, Arguments0),
            io:format("detected signature on line ~p ~p~n", [Line, Arguments0]),
            %% use a try/after to install the metadata, run the code, return the actual value we expect, and remove the metadata once the call has completed
            {'try',Line,
                   [{call, Line, {remote, Line, {atom, Line, ?MODULE}, {atom, Line, enter}}, [{tuple, Line, [{atom, Line, Module}, {atom, Line, Function}, {atom, Line, get(module)}, {atom, Line, get(function)}, {integer, Line, Line}, Destination]}]}],
                   [{clause,Line,
                        [{var,Line,'_'}],
                        [],
                        [NewStmt]}],
                   [],
                   [{call, Line, {remote, Line, {atom, Line, ?MODULE}, {atom, Line, exit}},[]}]};
        false ->
            list_to_tuple(transform_statement(tuple_to_list(Stmt), Targets))
    end;
transform_statement(Stmt, Targets) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt), Targets));
transform_statement(Stmt, Targets) when is_list(Stmt) ->
    [transform_statement(S, Targets) || S <- Stmt];
transform_statement(Stmt, _Targets) ->
    Stmt.

extract({gen_server, call, _}, Line, [Destination, Msg|_]) ->
    {Destination, {call, Line, {remote, Line, {atom, Line, gen_server}, {atom, Line, call}}, [Destination, Msg, {atom, Line, infinity}]}};
extract({gen_fsm, sync_send_event, _}, Line, [Destination, Msg |_]) ->
    {Destination, {call, Line, {remote, Line, {atom, Line, gen_fsm}, {atom, Line, sync_send_event}}, [Destination, Msg, {atom, Line, infinity}]}};
extract({gen_fsm, sync_send_all_state_event, _}, Line, [Destination, Msg |_]) ->
    {Destination, {call, Line, {remote, Line, {atom, Line, gen_fsm}, {atom, Line, syns_send_all_state_event}}, [Destination, Msg, {atom, Line, infinity}]}};
extract({gen_statem, call, _}, Line, [Destination, Msg |_]) ->
    {Destination, {call, Line, {remote, Line, {atom, Line, gen_statem}, {atom, Line, call}}, [Destination, Msg, {atom, Line, infinity}]}};
extract({sys, get_state, _}, Line, [Destination |_]) ->
    {Destination, {call, Line, {remote, Line, {atom, Line, sys}, {atom, Line, get_state}}, [Destination, {atom, Line, infinity}]}};
extract({sys, get_status, _}, Line, [Destination |_]) ->
    {Destination, {call, Line, {remote, Line, {atom, Line, sys}, {atom, Line, get_status}}, [Destination, {atom, Line, infinity}]}};
extract({gen_event, sync_notify, 2}, Line, Arguments = [Destination, _]) ->
    {Destination, {call, Line, {remote, Line, {atom, Line, sys}, {atom, Line, get_status}}, Arguments}};
extract({gen_event, which_handlers, 1}, Line, Arguments = [Destination]) ->
    {Destination, {call, Line, {remote, Line, {atom, Line, sys}, {atom, Line, get_status}}, Arguments}}.


check_loop(Source, Destination) ->
    Pid = case Destination of
              D when is_pid(D) -> D;
              D when is_atom(D) -> whereis(D)
          end,
    case Pid == Source of
        true ->
            erlang:throw(call_loop_detected);
        false ->
            ok
    end,
    [{dictionary, Dict}] = erlang:process_info(Pid, [dictionary]),
    case lists:keyfind('__alexander', 1, Dict) of
        false ->
            ok;
        {'__alexander', {_TargetMod, _TargetFun, _Module, _Function, _Line, NewDestination}} ->
            check_loop(Source, NewDestination)
    end.

