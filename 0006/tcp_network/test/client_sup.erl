-module(client_sup).

-include("tcp_network.hrl").

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_reader/1]).

start_link() ->
    {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    
    {ok, Sup}.

init(_Args) ->
    {ok, {{simple_one_for_one, 5, 10}, 
            [
                {client, 
                    {client, start_link, []}, 
                    temporary, 
                    16#ffffffff, 
                    worker, 
                    [client]
                }
            ]
        }
    }.

%% accept返回后会回调这个函数
start_reader(Sock) ->
    supervisor:start_child(?MODULE, [Sock]).
