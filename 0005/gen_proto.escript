#!/usr/bin/env escript
%% -*- erlang -*-

%% 生成protobuf message对应的code/beam

-mode(compile).

-define(EXIT(Code), init:stop(Code)).

%% GenType = "code" | "beam"
main([GenType, DataDir, OutIncludeDir, OutEbinDir, OutSrcDir]) ->
    Dir = filename:join([root_dir(), "deps/erlang-protobuffs/ebin"]),
    true = code:add_path(Dir),

    ProtoFiles = filelib:wildcard(filename:join([DataDir, "proto", "*.proto"])),
    GenFun = fun (ProtoFile, Acc) ->
        Path = filename:absname(ProtoFile),
        Message = filename:basename(ProtoFile, ".proto"),
        if 
            % 生成源码
            GenType =:= "code" ->
                case catch(protobuffs_compile:generate_source(Path,
                                 [{imports_dir,
                                   [filename:join([DataDir, "proto"]),
                                    filename:join([DataDir, "proto", "import"])]},
                                 {output_include_dir, OutIncludeDir}, 
                                 {output_ebin_dir, OutEbinDir},
                                 {output_src_dir, OutSrcDir}
                                 ])) of
                ok ->
                    Acc andalso true;
                {error, _Reason} ->
                    false
                end;

            % 生成beam
            GenType =:= "beam" ->
                case catch(protobuffs_compile:scan_file(Path,
                                  [{imports_dir,
                                      [filename:join([DataDir, "proto"]),
                                      filename:join([DataDir, "proto", "import"])]},
                                  {output_include_dir, OutIncludeDir}, 
                                  {output_ebin_dir, OutEbinDir}])) of
                ok ->
                    Acc andalso true;
                {error, _Reason} ->
                    false
                end;
            true ->
                false
        end
    end,

    case lists:foldl(GenFun, true, ProtoFiles) of
      true -> 
        % 生成proto.hrl,include所有的*_pb.hrl
        L = filelib:wildcard(filename:join([OutIncludeDir, "*.hrl"])),
        HeadStr = lists:foldl(
        fun(Name, Acc) -> 
            BaseName = filename:basename(Name),  
            Acc 
            ++
            io_lib:format("-include(\"proto/~s\").\n", [BaseName])
        end, "%% Warning: this file is auto-generated.\n-ifndef(PROTO_PB_H).\n-define(PROTO_PB_H, true).\n", 
        L),
        HeadStr2 = HeadStr ++ "\n-endif.",
        FileName = filename:join([OutIncludeDir, "proto_pb.hrl"]),
        file:write_file(FileName, HeadStr2),
        io:format("\n\033\[1;42mSUCCESS\033[0m: gen proto success\n");
      _ -> 
        io:format("\n\033\[1;41mERROR\033[0m:*** One or more property test cases failed\n"),
        ?EXIT(1)
    end;
main(_) ->
    usage(),
    ?EXIT(0).

usage() ->
    io:format(
    "\033\[1;41m[Usage]\033[0m
./gen_proto proto_dir include_dir ebin_dir\n").

root_dir() ->
    Path = escript:script_name(),
    filename:dirname(filename:dirname(Path)).
