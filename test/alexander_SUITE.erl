-module(alexander_SUITE).

-export([all/0]).
-export([non_loop/1, loop/1, recursive_call/1]).

all() ->
    [
     non_loop,
     loop,
     recursive_call
    ].

non_loop(_) ->
    {ok, Pid1} = gen_server_test:start_link(),
    {ok, Pid2} = gen_server_test:start_link(),
    {ok, Pid3} = gen_server_test:start_link(),
    {ok, Pid4} = gen_server_test:start_link(),
    no_loop = gen_server_test:blocking_call(Pid1, [{gen_server_test, Pid2},
                                                   {gen_server_test, Pid3},
                                                   {gen_server_test, Pid4},
                                                   no_loop]),
    [gen_server:stop(P) || P <- [Pid1, Pid2, Pid3, Pid4]].

loop(_) ->
    process_flag(trap_exit, true),
    {ok, Pid1} = gen_server_test:start_link(),
    {ok, Pid2} = gen_server_test:start_link(),
    {ok, Pid3} = gen_server_test:start_link(),
    {ok, Pid4} = gen_server_test:start_link(),
    try gen_server_test:blocking_call(Pid1, [{gen_server_test, Pid2},
                                                   {gen_server_test, Pid3},
                                                   {gen_server_test, Pid4},
                                                   {gen_server_test, Pid1},
                                                   loop])
    of
        _ ->
            ct:fail(did_not_crash)
    catch
        What:Why ->
            ct:pal("~p ~p", [What, Why])
    end,
    [gen_server:stop(P) || P <- [Pid1, Pid2, Pid3, Pid4], is_process_alive(P)].

recursive_call(_) ->
    process_flag(trap_exit, true),
    {ok, Pid} = gen_server_test:start_link(),
    try gen_server_test:recursive_call(Pid) of
        _ ->
            ct:fail(did_not_crash)
    catch
        What:Why ->
            ct:pal("~p ~p", [What, Why])
    end.
