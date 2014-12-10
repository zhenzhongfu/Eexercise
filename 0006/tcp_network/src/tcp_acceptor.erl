-module(tcp_acceptor).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {reader, sock, ref}).

%%--------------------------------------------------------------------

%% 这里两个参数，Reader是spec预定义的，LSock是gen_tcp:listen返回后传入
start_link(Reader, LSock) ->
    gen_server:start_link(?MODULE, {Reader, LSock}, []).

%%--------------------------------------------------------------------

init({Reader, LSock}) ->
    gen_server:cast(self(), accept),
    {ok, #state{reader=Reader, sock=LSock}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(accept, State) ->
    accept(State);

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, LSock, Ref, {ok, Sock}},
            State = #state{reader=Reader, sock=LSock, ref=Ref}) ->

    %% patch up the socket so it looks like one we got from
    %% gen_tcp:accept/1
    {ok, Mod} = inet_db:lookup_socket(LSock),
    inet_db:register_socket(Sock, Mod),

    %% handle new acceptor event
    case catch handle_client(Reader, Sock) of
        ok ->
            ok;
        failed ->
            gen_tcp:close(Sock) 
    end,

    %% accept more
    accept(State);

handle_info({inet_async, LSock, Ref, {error, Reason}},
            State=#state{sock=LSock, ref=Ref}) ->
    case Reason of
        closed       -> {stop, normal, State}; %% listening socket closed
        econnaborted -> accept(State); %% client sent RST before we accepted
        _            -> {stop, {accept_failed, Reason}, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------

accept(State = #state{sock=LSock}) ->
    % 异步accept是可以让多个acceptor从accept队列取的
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {noreply, State#state{ref=Ref}};
        Error     -> {stop, {cannot_accept, Error}, State}
    end.

handle_client(Reader, Sock) ->
    % Reader是向role_sup添加child,这里只需要将返回的pid和sock绑定下
    case catch Reader:start_reader(Sock) of                             
        {ok, Pid} ->                                                    
            % TODO 注意pid的异步行为
            case gen_tcp:controlling_process(Sock, Pid) of              
                ok ->                                                   
                    ok;                                                 
                {error, Error} ->                                       
                    io:format("controlling_process ~p~n", [Error]),    
                    failed                                              
            end;                                                        
        Other ->                                                        
            io:format("start_client ~p~n", [Other]),                   
            failed                                                      
    end.
