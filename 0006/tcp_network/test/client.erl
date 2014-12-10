
-module(client).

-include("tcp_network.hrl").

-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
-export([]).

% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% client的state
-record(cli_state, {
        sock = 0
    }).

start_link(Sock) ->
    gen_server:start_link(
        ?MODULE,        % module
        [Sock],         % args
        []              % options
    ).

start_release_timer(Sock) ->
    erlang:send_after(?SOCKET_DUMP_MSG_INTERVAL, self(), {release_passive, Sock}).

% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([Sock]) ->
    process_flag(trap_exit, true),
    % TODO init socket opt
    % 等下 验证一下这个socket和listen的socket options是否可以继承
    io:format(user, " ------- test socket opt:~p~n", [inet:getopts(Sock, [reuseaddr, delay_send, nodelay, send_timeout])]),
    case inet:setopts(Sock, [{delay_send, true},{active, ?SOCKET_DUMP_MSG_COUNT}]) of  
        ok ->
            % 定时release passive mode
            start_release_timer(Sock),
            {ok, #cli_state{sock = Sock}};
        {error, _Reason} ->
            gen_tcp:close(Sock),    % 这里其实链接已经建立了，出错要记得退出. ps:init成功才走到terminate? 这里close了，terminate还要close吗
            {stop, normal}
    end.

% --------------------------------------------------------------------
% Function: handle_call/3
% Description: Handling call messages
% Returns: {reply, Reply, State}          |
%          {reply, Reply, State, Timeout} |
%          {noreply, State}               |
%          {noreply, State, Timeout}      |
%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_info({release_passive, Sock}, State) ->
    % 切换主动模式
    inet:setopts(Sock, [{active, ?SOCKET_DUMP_MSG_COUNT}]),
    start_release_timer(Sock),
    {noreply, State};
handle_info({tcp_passive, _Sock}, State) ->
    % TODO 超出流量控制了，疑似恶意攻击?
    io:format("WARNING: dump msg arise. sock[~p]~n", [_Sock]),
    {noreply, State};
handle_info({tcp, Sock, Bin}, State) ->
    % 数据包已到
    io:format(user, "tcp recv ~p~n", [Bin]),
    Ret = gen_tcp:send(Sock, Bin),
    io:format(user, "send result:~p~n", [Ret]),
    {noreply, State};                 
handle_info({tcp_closed, _Sock}, State) ->
    io:format("recv tcp_closed. sock[~p]~n", [_Sock]),
    {stop, normal, State};
handle_info({tcp_error, _Sock, emsgsize}, State) ->
    % 超过定义的包长
    io:format("error: ~p emsgsize ~n", [_Sock]),
    {stop, normal, State};
handle_info({tcp_error, _Sock, _Reason}, State) ->
    io:format("error: ~p ~n", [_Reason]),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: terminate/2
% Description: Shutdown the server
% Returns: any (ignored by gen_server)
% --------------------------------------------------------------------
terminate(_Reason, State) ->
    io:format(user, "terminate reason:~p~n", [_Reason]),
    (catch gen_tcp:close(State#cli_state.sock)),
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
