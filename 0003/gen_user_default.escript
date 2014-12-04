#!/usr/bin/env escript
%% -*- erlang -*-

%% 编译user_default是为了shell在GAME_HOME下启动能直接使用头文件定义的record
%% e.g. 
%% 1> rl(). % 列出所有user_default里incllude文件中定义的record

-mode(compile).

main([]) ->
    RootDir = root_dir(),
    IncludeDir = filename:join([RootDir, "include"]),
    OutCodeDir = filename:join([RootDir, "src"]),
    OutEbinDir = RootDir,
    io:format("IncludeDir:~p OutCodeDir:~p OutEbinDir:~p~n", [IncludeDir, OutCodeDir, OutEbinDir]),

    % 匹配文件名,排除proto目录,common.hrl,ecode.hrl
    L = filelib:wildcard(filename:join([IncludeDir, "*.hrl"])),
    % 生成user_default.erl
    HeadStr = lists:foldl(
        fun(Name, Acc) ->
                BaseName = filename:basename(Name),  
                if 
                    BaseName =:= "common.hrl" orelse BaseName =:= "ecode.hrl"->
                        Acc;
                    true ->
                        Acc
                        ++
                        io_lib:format("-include(\"~s\").\n", [BaseName])
                end
            end, 
        ["%% Warning: this file is auto-generated.\n-module(user_default).\n"], 
        L),
    FileName = filename:join([OutCodeDir, "user_default.erl"]),
    file:write_file(FileName, HeadStr),

    % 编译user_default.erl,编译选项加上debug_info
    case make:files([FileName], [debug_info, {i, IncludeDir}, {outdir, OutEbinDir}]) of
        error ->
            io:format("\n\033\[1;41mERROR\033[0m:*** gen user_default.hrl make error\n");
        _ ->
            io:format("\n\033\[1;42mSUCCESS\033[0m: gen user_default.hrl success\n")
    end.

root_dir() ->
    Path = escript:script_name(),
    filename:dirname(filename:dirname(filename:dirname(Path))).
