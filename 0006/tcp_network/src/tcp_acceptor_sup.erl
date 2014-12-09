-module(tcp_acceptor_sup).

-behaviour(supervisor).

-include("tcp_network.hrl").

-export([start_link/2]).

-export([init/1]).

start_link(Port, Reader) ->
    supervisor:start_link({local,?GET_ACCEPTOR_NAME(Port)}, ?MODULE, [Reader]).

init([Reader]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{tcp_acceptor, {tcp_acceptor, start_link, [Reader]},
            transient, brutal_kill, worker, [tcp_acceptor]}]}}.
