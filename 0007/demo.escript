#!/usr/bin/env escript
%% -*- erlang -*- 

%% 为节约协议长度，将protobuf的映射字符串转成id
%% package_id,message_id 长度分别是8,解析demo.proto应该生成如下项:
%%   demo=3
%%   demo.c2s_addressbook=769     % 3:8,1:8
%% 这样能支持255个package

-module(compile).

main([]) ->
    RootDir = root_dir(),
    ProtoFiles = filelib:wildcard(filename:join([RootDir, "*.proto"])),

    % txt
    FileName = "./mf_id.txt",
    {ok, TxtFd} = file:open(FileName, write),

    % erl
    FileName2 = "./mf_id.erl",
    {ok, ErlFd} = file:open(FileName2, write),
    write(ErlFd, "-module(mf_id).\n\n"),

    [begin 
                parse_proto(TxtFd, ErlFd, File),
                write(TxtFd, "\n"),
                write(ErlFd, "\n")
        end || File <- ProtoFiles],

    % txt
    file:close(TxtFd),

    % erl
    write(ErlFd, "id_msg_convert(_Arg) -> io:format(\"invalid arg:~p\", [_Arg]), error(invalid_is_msg_convert)."),
    file:close(ErlFd),

    ok.

root_dir() ->                                                  
    Path = escript:script_name(),                              
    filename:dirname(filename:dirname(filename:dirname(Path))).

write(Fd, Info) ->
    file:write(Fd, Info).

parse_proto(TxtFd, ErlFd, ProtoFile) ->
    % 取得注释中的id定义
    {ok, Fd} = file:open(ProtoFile, read),
    Desc = get_desc(Fd, []),
    ok = file:close(Fd),

    % 描述转term
    L = lists:foldl(
        fun(Str, Acc) ->
            Term = expr_to_term(Str),
            [Term | Acc]
        end, [], Desc),

    % TODO
    % 1. 按tuple读package和message，生成k=v这样的写入txt文件.这里还要校验下package的编号与其他文件的编号是否有冲突
    % 2. get(k) -> v. get(v) -> k. 生成erl代码写入文件

    % package
    case gen_package_prop(L) of
        {{TxtContext1, ErlContext1}, PackageName, PackageId, Rest} ->
            % message
            {TxtContext2, ErlContext2} = gen_message_prop(Rest, PackageId, PackageName),

            TxtContext = TxtContext1 ++ TxtContext2,
            write(TxtFd, TxtContext),

            ErlContext = ErlContext1 ++ ErlContext2,
            file:write(ErlFd, ErlContext),
            io:format("\n\033\[1;42mSUCCESS\033[0m: file:~p gen mf_id  success\n", [ProtoFile]),
            ok;
        false ->
            io:format("\n\033\[1;42mSUCCESS\033[0m: file:~p gen mf_id success\n", [ProtoFile]),
            error
    end,
    ok.

%% 按行解析文件,得到proto注释里的自定义id描述
get_desc(Fd, Acc) ->
    case io:get_line(Fd,"") of
        eof ->
            Acc;
        Line ->
            Head = 
            case catch string:sub_string(Line, 3, 10) of
                {'EXIT', _} -> 
                    "";
                Other ->
                    Other
            end,
            Acc2 = 
            case Head =:= "{package" orelse Head =:= "{message" of
                true ->
                    Line2 = string:sub_string(Line, 3),
                    Line3 = string:strip(Line2, right, $\n),
                    [Line3 | Acc];
                false ->
                    Acc
            end,
            get_desc(Fd, Acc2)
    end.

%% 生成package的配置项
%% 需要在message之前生成，message_id基于package_id
gen_package_prop(L) when is_list(L) ->
    case catch lists:keytake(package, 1, L) of
        {value, Tuple, Rest} ->
            {package, PropName, PropId} = Tuple,
            TxtNewLine = convert_txt_newline(PropName, PropId),
            ErlNewLine = convert_erl_newline(PropName, PropId),
            %io:format("******** TxtLine:~p ErlNewLine:~p Rest:~p~n", 
            %    [TxtNewLine, ErlNewLine, Rest]),
            {{TxtNewLine, ErlNewLine}, PropName, PropId, Rest};
        _Other ->
            false
    end.

%% 生成message的配置项,txt的文本项和erl的文本项
%% Return = {TxtContext, ErlContext}
%% TxtContext = string()
%% ErlContext = string()
gen_message_prop(L, BaseId, BaseName) when is_list(L) and is_integer(BaseId) ->
    lists:foldl(
        fun
            ({message, PropName, PropId}, {TxtAcc, ErlAcc}) ->
                PropId2 = convert_propid(PropId, BaseId),
                TxtNewLine = convert_txt_newline(PropName, PropId2, BaseName),
                ErlNewLine = convert_erl_newline(PropName, PropId2, BaseName),
                {TxtNewLine ++ TxtAcc, ErlNewLine ++ ErlAcc};
            (_Other, {TxtAcc, ErlAcc}) ->
                {TxtAcc, ErlAcc}
        end, {"", ""}, L).

%%--------------------------------------------------------
%%
%%--------------------------------------------------------

%% 根据package_id生成message_id
convert_propid(PropId, BaseId) ->
    <<PropId2:16>> = <<BaseId:8, PropId:8>>,
    PropId2.

%% package=id
convert_txt_newline(PropName, PropId) ->
    atom_to_list(PropName) ++ "=" ++ integer_to_list(PropId) ++ "\n".
%% package.message=id
convert_txt_newline(PropName, PropId, BaseName) ->
    atom_to_list(BaseName) ++ "." ++ atom_to_list(PropName) ++ "=" ++ integer_to_list(PropId) ++ "\n".

%% id_msg_convert(id) -> msg
%% id_msg_convert(msg) -> id 
convert_erl_newline(PropName, PropId) ->
    "id_msg_convert(" ++ atom_to_list(PropName) ++ ") -> " ++ integer_to_list(PropId) ++ ";\n"
    ++
    "id_msg_convert(" ++ integer_to_list(PropId) ++ ") -> " ++ atom_to_list(PropName) ++ ";\n".
%% id_msg_convert(id) -> package_message
%% id_msg_convert(package_message) -> id 
convert_erl_newline(PropName, PropId, BaseName) ->
    "id_msg_convert(" ++ atom_to_list(BaseName) ++ "_" ++ atom_to_list(PropName) ++ ") -> " ++ integer_to_list(PropId) ++ ";\n"
    ++
    "id_msg_convert(" ++ integer_to_list(PropId) ++ ") -> " ++ atom_to_list(BaseName) ++ "_" ++ atom_to_list(PropName) ++ ";\n".

%% 字符串表达式转term
%% http://www.cnblogs.com/me-sa/archive/2011/12/15/erlang0021.html
expr_to_term(String) ->
    {ok, Scaned, _} = erl_scan:string(String),
    {ok, Parsed}=erl_parse:parse_exprs(Scaned),
    {value, Term, []} = erl_eval:exprs(Parsed, []),
    Term.

%% term转字符表达式
%term_to_expr(Term) ->
%    lists:flatten(io_lib:write(Term)).

