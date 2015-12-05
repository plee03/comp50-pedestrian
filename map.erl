% Map Module

-module(map).
-export([start/2]).
-export([loop/2, next_edge/4, init/2]).

start(Map, EdgeCaps) ->
    register(map_server, spawn(map, loop, [Map, EdgeCaps])).

init(Map, EdgeCaps) ->
    receive
        ets_tables -> 
            io:fwrite("Got tables!~n"),
            loop(Map, EdgeCaps)
    end.

loop(Map, EdgeCaps) ->
    receive
        {subscribe, Current_Pos} -> 
            io:fwrite("APPLES~n"),
            Vertices = ugraph:vertices(Map),
            io:fwrite("~p~n", [Vertices]),
            ugraph:add_edge(Map, tisch, granoff, 10),
            io:fwrite("KIWIS~n"),

            {E, V1, V2, Weight} = ugraph:edge(Map, Current_Pos),
            %V3 = ugraph:vertex(Map, V1),
            %V4 = ugraph:vertex(Map, V2),
            io:fwrite("V1: ~w V2: ~w~n", [V1, V2]),
            %io:fwrite("V3: ~w V4: ~w~n", [V3, V4]),
            ugraph:add_edge(Map, V1, V2, 10),
            %io:fwrite("V3: ~w V4: ~w~n", [V3, V4]),
            %ugraph:add_edge(Map, V3, V4, 10),
            io:fwrite("PEARS~n"),
            EdgeKey = sets:from_list([V1, V2]),
            NewEdgeCaps = update_num_people(EdgeKey, EdgeCaps, 1);
            %{Base, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps), 
            %io:fwrite('~w people~n', [NumPeople]),
            %update_weight(Map, V1, V2, NumPeople, Cap, Weight, s);
        {unsubscribe, Current_Pos} -> 
            {E, V1, V2, Weight} = ugraph:edge(Map, Current_Pos),
            EdgeKey = sets:from_list([V1, V2]),
            NewEdgeCaps = update_num_people(EdgeKey, EdgeCaps, -1),
            {Base, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps), 
            update_weight(Map, V1, V2, NumPeople, Cap, Weight, u);
        {next_edge, From, Current_Pos, Destination} -> 
        % map should be shared instead of passed
            NewEdgeCaps = EdgeCaps,
            spawn(map, next_edge, [From, Current_Pos, Destination, Map])
    end,
    loop(Map, NewEdgeCaps).

update_num_people(EdgeKey, EdgeCaps, Num) ->
    {Base, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps),
    dict:store(EdgeKey, {Base, Cap, NumPeople + Num}, EdgeCaps).

update_weight(G, V1, V2, NumPeople, Cap, Weight, s) when NumPeople > Cap ->
    ugraph:add_edge(G, V1, V2, Weight + 3);
    %ugraph:update_edge(G, E, Weight + 3);
update_weight(G, V1, V2, NumPeople, Cap, Weight, u) when NumPeople > Cap ->
    ugraph:add_edge(G, V1, V2, Weight - 3);
    %ugraph:update_edge(G, E, Weight - 3);
update_weight(_, _, _,  NumPeople, Cap, _, _) when NumPeople =< Cap ->
    ok.


next_edge(From, Current_Pos, Destination, Map) ->
    case ugraph:shortest_path(Map, Current_Pos, Destination) of
        [NextEdge | _Path] ->
            {_, _, V2, Weight} = ugraph:edge(Map, NextEdge),
            From ! {NextEdge, V2, Weight};
        [] -> 
            erlang:error(current_dest_same) 
    end.
