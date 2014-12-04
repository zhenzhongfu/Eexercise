#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).

%% 编译user_default是为了erlang shell在GAME_HOME目录启动时,能直接使用user_default里定义的record
%% 这里的做法是user_default只include各个头文件
%% e.g. 
%% 1> rl(). % 列出所有user_default里定义的record

main([]) ->
    {ok, Root} = file:get_cwd(),
    IncludeDir = filename:join([Root, "include"]),
    OutCodeDir = filename:join([Root, "src"]),
    OutEbinDir = Root,
    io:format("IncludeDir:~p OutCodeDir:~p OutEbinDir:~p~n", [IncludeDir, OutCodeDir, OutEbinDir]),

    % 匹配文件名
    L = filelib:wildcard(filename:join([IncludeDir, "*.hrl"])),
    % 生成user_default.erl
    HeadStr = lists:foldl(
        fun(Name, Acc) ->
                BaseName = filename:basename(Name),  
                if 
                    % TODO filter
                    true ->
                        Acc
                        ++
                        io_lib:format("-include(\"~s\").\n", [BaseName])
                end
            end, 
        ["%% Warning: auto generate.\n-module(user_default).\n"], 
        L),
    FileName = filename:join([OutCodeDir, "user_default.erl"]),
    file:write_file(FileName, HeadStr),

    % 编译user_default.erl,编译选项加上debug_info
    case make:files([FileName], [debug_info, {i, IncludeDir}, {outdir, OutEbinDir}]) of
        error ->
            io:format("\nERROR:*** gen user_default.hrl make error\n");
        _ ->
            io:format("\nSUCCESS: gen user_default.hrl success\n")
    end.
