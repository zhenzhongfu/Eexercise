%% 0001. gen server的behaviour机制理解
%% http://www.cnblogs.com/me-sa/archive/2011/12/20/erlang0023.html
%% e.g.
%% 1> c(demo).
%% {ok,demo}
%% 2> c(gen_server_template).
%% {ok,gen_server_template}
%% 3> demo:start().
%% demo:init
%% <0.44.0>
%% 4> demo:call(inc).
%% handle_call <0.32.0> {state,0}
%% ok1
%% %5> demo:call(inc).
%% handle_call <0.32.0> {state,1}
%% ok1
%% 6> demo:cast(inc).
%% ok

-module(demo).

-export([start/0, init/0]).
-export([call/1, cast/1]).
-export([handle_call/3, handle_cast/2]).

-record(state, {
        account = 0
    }).

%% 公共接口部分
start() ->
    gen_server_template:start(demo).

call(Req) ->
    gen_server_template:call(demo, Req).

cast(Req) ->
    gen_server_template:cast(demo, Req).

%% 回调部分
init() ->
    io:format("~p:init~n", [?MODULE]),
    #state{}.

handle_call(inc, From, #state{account = Account} = State) ->
    io:format("handle_call ~p ~p~n", [From, State]),
    {ok1, State#state{account = Account + 1}}.

handle_cast(inc, #state{account = Account} = State) ->
    State#state{account = Account + 10}.
