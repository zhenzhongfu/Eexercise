Eexercise
=========

some erlang exercise

==

0001，gen_server的机制理解
http://www.cnblogs.com/me-sa/archive/2011/12/20/erlang0023.html
接口部分：spawn -> register -> receive -> evaluate -> loop  -> receive -> evaluate ...
回调部分：Mod:init    Mod:handle_call/Mod:handle_cast

==

0002，The Abstract Format
http://erlang.org/doc/apps/erts/absform.html
完美输出record的field和value
erlang提供了hook可以自己parse Mod:module_info里的attribute，通过
1)编译期间遍历并记录record的fieldname，记录到beam的metadata，
2)print时从beam的metadata中拿出来，再将record的各个field与value组合输出。
打印完整信息标准的解决方案使用shell中的rp方法格式化输出结果 比如 rp(os:getenv()).

==

0003，user_default
在游戏home目录编译user_default.beam是为了erl shell在home下启动能直接使用头文件定义的record.
%% e.g.                                   
%% 1> rl(). % 列出所有头文件中定义的recode 

==

0004，编译与reload单个文件
编译:compile:file，编译参数options注意hipe和native
reload:直接使用mochiweb的reloader.erl，用gen_server去定时扫描文件是否有更新，
code:load_file之前使用code:soft_purge清除old_version。
erlang执行的code version只维持current和old，再有版本更改就必须将old purge掉，
这时如有引用old的进程都会被干掉，soft_purge比较安全

==

0005，编译protobuf
rebar其实也有自带的pb compiler，%{proto_compiler, protobuffs},%{gpb_opts, []},
不过还没研究清楚，用自己写的可以控制生成beam还是erl

==

0006，tcp network的封装
用rebar制作app封装tcp网络层，从rabbitmq拿出来的。
修改了一下监控树结构：
application controller ——> tcp_sup ——> tcp_listener_sup ——> tcp_acceptor_sup ——> tcp_acceptor
                                             |
                                             |——> tcp_listener
client_sup被拿出来，在tcp_acceptor收到消息时回调client_sup的接口再去start client_sup 的child。
acceptor使用prim_inet:async_accept从队列里处理收到的新链接。

这里值得一说的就是socket的option，http://www.erlang.org/doc/man/inet.html#setopts-2
 %% socket options                                                                                                
-define(TCP_OPTS,                                                                                                
     [binary,                                                                                                     
     {packet, 2},                    % 包头长度2,不需要自己拆包                                                   
     {packet_size, 61440},           % 允许的单个包大小 60k                                                        
     {active, N},                    % 流量控制用once | N, 这个在client中重设为N,并用计时器去release passive mode 
     {reuseaddr, true},                                                                                           
     {delay_send, true},             % erlang自己的queue up                                                       
     {nodelay, true},                % send immediately                                                           
     {backlog, 1024},                % socket缓存队列长度                                                          
     {send_timeout, 5000}                                                                                         
     ]).      
accept返回后的socket是会继承listen定义的opts，将socket与client的pid绑定，
inet:setopts将socket修改为主动模式{active, N}，然后client这个gen_server等待数据通知。

{active,N}表示N个数据包之后切换为passive模式，不主动传递消息,被切换为passive后的消息会一直阻塞到缓存，
可以通过recv直接取或者active之后再次被通知得到，可以用定时器来做流量控制，
timeout的时间内收到N个包就开启被动模式不处理之后的数据，标记恶意行为，在timeout时再次切为主动N模式。

{packet,2}在recv和send在socket上传递时会自带消息长度（占2个字节0～65536）放在消息头，不需要自己计算数据包的大小，
超过了{packet_size, 61440}的数据包会返回错误{tcp_error, _Sock, emsgsize}，数据错了应该掐掉链接。
