-module(gen_server_test).
-compile([{parse_transform, alexander}]).

-behaviour(gen_server).

-export([start_link/0, blocking_call/1, blocking_call/0, blocking_infinite_call/1, blocking_timeout_call/1, recursive_call/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

blocking_call(Pid) ->
    gen_server:call(Pid, hello).

blocking_call() ->
    gen_server:call(?MODULE, hello).

blocking_infinite_call(Pid) ->
    gen_server:call(Pid, hello, infinity).

blocking_timeout_call(Pid) ->
    gen_server:call(Pid, hello, 500).

recursive_call(Pid) ->
    gen_server:call(Pid, recurse).

init([]) ->
    {ok, #state{}}.

handle_call(recurse, _From, State) ->
    {reply, recursive_call(self()), State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
