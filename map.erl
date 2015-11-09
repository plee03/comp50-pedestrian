% Map Module

-module(map).
-export([start/1]).
-export([loop/1]).

start(Map) -> 
    spawn(map, loop, [Map]).

loop(Map) -> 
    receive
        {shortest_path, From, Current_Pos, Destination} -> 
        % map should be shared instead of passed
            spawn(map, shortest_path, [From, Current_Pos, Destination, Map])
    end,
    loop(Map).

shortest_path(_To, _Current_Pos, _Destination, _Map) -> ok.
