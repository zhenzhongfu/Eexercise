-module(tcp_listener_sup).

-behaviour(supervisor).

-include("tcp_network.hrl").

-export([start_link/4]).

-export([init/1]).

start_link(Reader, Port, AcceptorCount, SocketOpts) ->
    % 每个端口用单独的supervisor管理
    supervisor:start_link(?MODULE, [Reader, Port, AcceptorCount, SocketOpts]).

init([Reader, Port, AcceptorCount, SocketOpts]) ->
    {ok, {{one_for_one, 10, 10},
            [
                % 按顺序启动依赖的mod,tcp_listener启动后,会创建acceptor
                % 1. tcp_accept_sup
                {tcp_acceptor_sup, 
                    {tcp_acceptor_sup, start_link, [Port, Reader]},
                    transient, 
                    infinity, 
                    supervisor, 
                    [tcp_acceptor_sup]
                },
                % 2. tcp_listener
                {tcp_listener, 
                    {tcp_listener, start_link, [Port, AcceptorCount, SocketOpts]},
                    transient,          % 异常退出则重启
                    16#ffffffff, 
                    worker, 
                    [tcp_listener]
                }
            ]
        }
    }.
