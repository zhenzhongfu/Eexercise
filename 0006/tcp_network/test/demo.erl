-module(demo).

-include_lib("eunit/include/eunit.hrl").

tcp_test_() ->
    {setup, 
        fun() ->
                ok = application:start(tcp_network),
                {ok, _Pid} = client_sup:start_link(),
                ok = tcp_network:listen(client_sup, 10001, 8, []),
                ok = tcp_network:listen(client_sup, 10002, 8, [])
        end,
        fun(_) ->
                application:stop(tcp_network)
        end,
        {with, none,
            [
                fun tcp_case_10001/1,
                fun tcp_case_10002/1
            ]
        }
    }.

tcp_case(Port) ->
    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}], 5000),
    Msg = <<"hello world!">>,
    gen_tcp:send(Sock, Msg),
    ?assertEqual({ok, Msg}, gen_tcp:recv(Sock, 0, 5000)),
    gen_tcp:close(Sock).

tcp_case_10001(_) ->
    tcp_case(10001).

tcp_case_10002(_Args) ->
    tcp_case(10002).
