%% 格式化record的输出,从lager拆出来的
%% 在需要的erl文件加上 -compile([{parse_transform, parse_transform_record}]).
%% 或者rebar.config配置erl_opt加上{parse_transform, parse_transform_record}

-module(parse_transform_record).

-export([parse_transform/2, pr/2]).

%% 编译其他模块时的parse_transform钩子,module的元数据在调用钩子时就已经写入beam了
%% Options: 
%% [{parse_print_records_flag, true}]   
parse_transform(AST, Options) ->                                                           
    Enable = proplists:get_value(parse_print_records_flag, Options, true),  
    put(parse_print_records_flag, Enable),                                        
    erlang:put(records, []),                                                               
    walk_ast([], AST).                                                                     

walk_ast(Acc, []) ->                                                                       
    case get(parse_print_records_flag) of                                                        
        true ->                                                                            
            insert_record_attribute(Acc);                                                  
        false ->                                                                           
            lists:reverse(Acc)                                                             
    end;                                                                                   
walk_ast(Acc, [{attribute, _, record, {Name, Fields}}=H|T]) ->                 
    FieldNames = lists:map(
        fun({record_field, _, {atom, _, FieldName}}) ->     
                FieldName;                                                     
            ({record_field, _, {atom, _, FieldName}, _Default}) ->             
                FieldName                                                      
        end, Fields),                                                          
    stash_record({Name, FieldNames}),                                          
    walk_ast([H|Acc], T);                                                      
walk_ast(Acc, [H|T]) ->          
    walk_ast([H|Acc], T).        

stash_record(Record) ->                       
    Records = case erlang:get(records) of     
        undefined ->                          
            [];                               
        R ->                                  
            R                                 
    end,                                      
    erlang:put(records, [Record|Records]).    

insert_record_attribute(AST) ->                                                     
    lists:foldl(fun({attribute, Line, module, _}=E, Acc) ->                         
                [E, {attribute, Line, parse_transform_records, erlang:get(records)}|Acc];     
            (E, Acc) ->                                                             
                [E|Acc]                                                             
        end, [], AST).                                                              


%% @doc Print a record lager found during parse transform                          
%% 暂时只处理一层record嵌套
pr(Record, Module) when is_tuple(Record), is_atom(element(1, Record)) ->           
    try                                                                            
        case is_record_known(Record, Module) of                                    
            false ->                                                               
                Record;                                                            
            {RecordName, RecordFields} ->                                          
                {'$parse_transform_records', RecordName,                                      
                    zip(RecordFields, tl(tuple_to_list(Record)), Module, [])}      
        end                                                                        
    catch                                                                          
        error:undef ->                                                             
            Record                                                                 
    end;                                                                           
pr(Record, _) ->                                                                   
    Record.                                                                        

zip([FieldName|RecordFields], [FieldValue|Record], Module, ToReturn) ->            
    case   is_tuple(FieldValue) andalso                                            
        tuple_size(FieldValue) > 0 andalso                                      
        is_atom(element(1, FieldValue)) andalso                                 
        is_record_known(FieldValue, Module) of                                  
        false ->                                                                   
            zip(RecordFields, Record, Module, [{FieldName, FieldValue}|ToReturn]); 
        _Else ->                                                                   
            F = {FieldName, pr(FieldValue, Module)},                               
            zip(RecordFields, Record, Module, [F|ToReturn])                        
    end;                                                                           
zip([], [], _Module, ToReturn) ->                                                  
    lists:reverse(ToReturn).                                                       

is_record_known(Record, Module) ->                                               
    Name = element(1, Record),                                                   
    Attrs = Module:module_info(attributes),                                      
    case lists:keyfind(parse_transform_records, 1, Attrs) of                               
        false -> false;                                                          
        {parse_transform_records, Records} ->                                              
            case lists:keyfind(Name, 1, Records) of                              
                false -> false;                                                  
                {Name, RecordFields} ->                                          
                    case (tuple_size(Record) - 1) =:= length(RecordFields) of    
                        false -> false;                                          
                        true -> {Name, RecordFields}                             
                    end                                                          
            end                                                                  
    end.                                                                         
