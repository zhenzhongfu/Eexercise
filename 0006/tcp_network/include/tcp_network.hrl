-ifndef(TCP_NETWORK_H).
-define(TCP_NETWORK_H, true).

%% socket options
-define(TCP_OPTS,
    [binary,
    {active, false},   % 流量控制用once | N, 这个在client中重设为N,并用计时器去release passive mode
    {reuseaddr, true},
    {delay_send, true}, % erlang自己的queue up
    {nodelay, true},    % send immediately
    %{backlog, 1024},   % socket缓存队列长度
    {send_timeout, 5000}
    ]).

%% 流量控制，60s处理300个包，超过就利用passive mode阻塞
-define(SOCKET_DUMP_MSG_INTERVAL, 60 * 1000).
%-define(SOCKET_DUMP_MSG_COUNT, 300).     
-define(SOCKET_DUMP_MSG_COUNT, 3).     

%% 动态sup名字,支持listen多个端口
-define(GET_LISTENER_NAME(Port), list_to_atom("tcp_listener_sup_" ++ integer_to_list(Port))).
-define(GET_ACCEPTOR_NAME(Port), list_to_atom("tcp_acceptor_sup_" ++ integer_to_list(Port))).

-endif.

