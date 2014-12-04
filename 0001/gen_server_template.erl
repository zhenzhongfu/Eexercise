-module(gen_server_template).

-export([start/1, init/1]).
-export([call/2, cast/2]).

%% 内部机制
%% spawn -> register -> receive -> evaluate -> loop
%%                          ^                    |
%%                          |____________________|
start(Mod) ->
    spawn(gen_server_template, init, [Mod]).

init(Mod) ->
    register(Mod, self()),
    State = Mod:init(),
    loop(Mod, State).

loop(Mod, State) ->
    receive
        {call, From, Req} ->
            {Rsp, State2} = Mod:handle_call(Req, From, State),
            From ! {Mod, Rsp},
            loop(Mod, State2);
        {cast, Req} ->
            State2 = Mod:handle_cast(Req, State),
            loop(Mod, State2);
        stop ->
            stop
    end.

%% 外部接口
call(Name, Req) ->
    Name ! {call, self(), Req},
    receive 
        {Name, Rsp} ->
            Rsp
    after
        infinity ->
            ok
    end.

cast(Name, Req) ->
    Name ! {cast, Req},
    ok.
