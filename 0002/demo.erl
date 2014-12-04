%% 0002. 自定义record的打印,输出fieldname和fieldvalue,基本实现是从lager拆出来的
%% http://erlang.org/doc/apps/erts/absform.html
%% e.g.
%% 1> c(demo, [debug_info]).                  
%% 2> c(parse_transform_record, [debug_info]).
%% 3> demo:print().                           

-module(demo).

-export([print/0]).

-compile([{parse_transform, parse_transform_record}]).

-record(role, {
        id,
        name, 
        lvl,
        coin,
        pet
    }).
-record(pet, {
        id, 
        lvl
    }).

print() ->
    Pet = #pet{id = 10, lvl = 1},
    Pet2 = Pet#pet{id = 11},
    P = [Pet, Pet2],
    Role = #role{id = 1, name = <<"dd">>, lvl = 99, coin = 100, pet = P}, 
    io:format("~p~n", [Role]), 
    io:format("~p~n", [parse_transform_record:pr(Role, ?MODULE)]), 
    ok.
