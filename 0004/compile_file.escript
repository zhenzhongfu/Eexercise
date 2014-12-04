#!/usr/bin/env escript
%% -*- erlang -*-
%% 编译单个文件

-mode(native).      % for hipe compile
-define(EXIT(Code), init:stop(Code)).

-define(COMPILE_OPT,
    [
    {i, "."},
    {i, "include"},
    {i, "include/proto"},
    {outdir, "ebin"},
    report, 
    warn_obsolete_guard,    % old type testing BIFs
    warn_shadow_vars,       % "fresh" vars
    warnings_as_errors,
    verbose,
    native,                 % for hipe compile
    {hipe, o3}
    ]).

main([Mods]) ->
    code:add_path(filename:join([root_dir(), "ebin"])),
    [begin
        case catch find_file(Lang, Mod) of
            none ->
               io:format("\033[1;41m[ERROR]\033[0m:Can not find the module:~s~n", [Mod]), 
               ?EXIT(1);
            {ok, FilePath} ->
               compile_file(FilePath)
           end
        end || Mod <- Mods],
    ok;
main([]) ->
    usage(),
    ?EXIT(0).

usage() ->
    io:format(
    "\033\[1;41m[Usage]\033[0m
./compile_file module_name\n").

find_file(_Lang, Mod) ->
    FileName = 
    case filename:extension(Mod) of
        "" ->
            Mod ++ ".erl";
        ".erl" ->
            Mod
    end,
    Root = root_dir(),
    [begin
        case filelib:wildcard(FindPath) of
            [] ->
                ok;
            [File] ->
                throw({ok, File})
        end
    end || FindPath <-
    [
    filename:join([Root, "src", FileName]),
    filename:join([Root, "src", "*", FileName]),
    filename:join([Root, "src", "*", "*", FileName]),
    filename:join([Root, "src", "*", "*", "*", FileName])
    ]],
    none.

compile_file(FilePath) ->
    case compile:file(FilePath, ?COMPILE_OPT) of
        {ok, _Data} ->
            io:format("\033[1;42m[OPERATE]\033[0m:success! ~p~n", [_Data]); 
        {ok, _, Warnings} ->
            io:format("\033[1;43m[WARNING]\033[0m:~p!~n", [Warnings]); 
        error ->
            io:format("\033[1;41m[ERROR]\033[0m:Compile failed!~n"); 
        {error, Errors, Warnings} ->
            io:format("\033[1;41m[ERROR]\033[0m:Compile failed!~p~n", [Errors]), 
            io:format("\033[1;43m[WARNING]\033[0m:~p!~n", [Warnings]),
            ?EXIT(1)
    end.

root_dir() ->
    Path = escript:script_name(),
    filename:dirname(filename:dirname(Path)).
