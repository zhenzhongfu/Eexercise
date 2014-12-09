%%--------------------------------------
%% tcp network暴露给外部的接口
%% 其实是对rabbit network部分进行裁剪
%% acceptor_sup和listener_sup放在tcp_sup管理，但是client还是交由client_sup自己去管好点

-module(tcp_network).                                                            

-include("tcp_network.hrl").

-behaviour(application).                                                        

%% Application callbacks                                                        
-export([start/2, stop/1]).                                                     
-export([listen/4]).                                                     

%% ===================================================================          
%% Application callbacks                                                        
%% ===================================================================          

start(_StartType, _StartArgs) ->                                                
    tcp_sup:start_link().                                                    

stop(_State) ->                                                                 
    ok.                                                                         

%% 监听接口
%% Reader = atom(), 挂client的supervisor模块名称,这里accept后回调是Reader:start_reader
%% Port = integer(), listen的端口
%% AcceptorCount = integer(), acceptor进程的数量
%% _SockOpts = list(), socket option
listen(Reader, Port, AcceptorCount, _SockOpts) -> 
    tcp_sup:start_supervisor_child(
        ?GET_LISTENER_NAME(Port),                       % child id
        tcp_listener_sup,                               % mod
        [Reader, Port, AcceptorCount, ?TCP_OPTS]).
