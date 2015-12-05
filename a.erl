-module(a).
-export([start/0, test/0, set/0, ped/0]).

start() ->
    spawn(a, test, []).

test() ->
    receive 
        {Int1, Int2} -> ok
    end,
    io:fwrite("~p~p~n", [Int1, Int2]).

set() ->
    Dict = dict:store(sets:from_list([v1, v2]), v1v2, dict:new()),
    Val1 = dict:fetch(sets:from_list([v2, v1]), Dict),
    Val2 = dict:fetch(sets:from_list([v1, v2]), Dict),
    io:fwrite("~w~n~w~n", [Val1, Val2]).

ped() ->
    loader:start('', 'graph_test.txt'),
    map_server ! {next_edge, self(), halligan, braker},
    receive 
        {NextEdge, NextV, Weight} -> ok
    end,
    map_server ! {subscribe, NextEdge},
    map_server ! {subscribe, NextEdge},
    map_server ! {subscribe, NextEdge},
    map_server ! {subscribe, NextEdge},
    map_server ! {subscribe, NextEdge},
    map_server ! {subscribe, NextEdge},
    map_server ! {subscribe, NextEdge},
    map_server ! {subscribe, NextEdge}.
