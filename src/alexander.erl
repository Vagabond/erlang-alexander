-module(alexander).

%% API exports
-export([parse_transform/2, check/0, enter/1, exit/0]).

%%====================================================================
%% API functions
%%====================================================================

parse_transform(AST, _Options) ->
    put(targets, [{gen_server, call, 2},
                  {gen_server, call, 3}
                 ]),
    walk_ast([], AST).

check() ->
    true.

enter(_Signature) ->
    io:format("entering blocking call ~p~n", [_Signature]),
    ok.

exit() ->
    io:format("exiting blocking call~n"),
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
            io:format("detected signature on line ~p~n", [Line]),
            lists:reverse([{call, Line, {remote, Line, {atom, Line, ?MODULE}, {atom, Line, enter}},
              [{tuple, Line, [{atom, Line, Module}, {atom, Line, Function}, {atom, Line, get(module)}, {integer, Line, Line}]}]},
              Stmt,
            {call, Line, {remote, Line, {atom, Line, ?MODULE}, {atom, Line, exit}},[]}]);
        false ->
            list_to_tuple(transform_statement(tuple_to_list(Stmt), Targets))
    end;
transform_statement(Stmt, Targets) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt), Targets));
transform_statement(Stmt, Targets) when is_list(Stmt) ->
    [transform_statement(S, Targets) || S <- Stmt];
transform_statement(Stmt, _Targets) ->
    Stmt.

