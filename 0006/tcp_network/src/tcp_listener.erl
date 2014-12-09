-module(tcp_listener).

-behaviour(gen_server).

-include("tcp_network.hrl").

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock}).

start_link(Port, AcceptorCount, SocketOpts) ->
    gen_server:start_link(?MODULE, [Port, AcceptorCount, SocketOpts], []).

%%--------------------------------------------------------------------

init([Port, AcceptorCount, SocketOpts]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, SocketOpts) of
        {ok, LSock} ->
            % 在对应port下的acceptor_sup下创建AcceptorCount个acceptor
            lists:foreach(fun (_) ->
                                  {ok, _APid} = supervisor:start_child(
                                                  ?GET_ACCEPTOR_NAME(Port), [LSock])
                          end,
                          lists:duplicate(AcceptorCount, dummy)),
            {ok, #state{sock = LSock}};
        {error, Reason} ->
            {stop, {cannot_listen, Port, Reason}}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{sock=LSock}) ->
    gen_tcp:close(LSock).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
