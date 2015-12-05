% Map Module

-module(map).
-export([start/1]).
-export([loop/2, next_edge/4, init/1]).

start(GraphTerms) ->
    register(map_server, spawn(map, init, [GraphTerms])).

init(GraphTerms) ->
    {Map, EdgeCaps} = loader:create_map(GraphTerms, ugraph:new(), dict:new()),
    loop(Map, EdgeCaps).
    

loop(Map, EdgeCaps) ->
    receive
        {subscribe, Current_Pos} -> 
            {E, V1, V2, Weight} = ugraph:edge(Map, Current_Pos),
            io:fwrite("V1: ~w V2: ~w~n", [V1, V2]),
            EdgeKey = sets:from_list([V1, V2]),
            NewEdgeCaps = update_num_people(EdgeKey, EdgeCaps, 1),
            {Base, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps), 
            io:fwrite('~w people~n', [NumPeople]),
            io:fwrite("weight: ~w~n", [Weight]),
            update_weight(Map, E, NumPeople, Cap, Weight, s);
        {unsubscribe, Current_Pos} -> 
            {E, V1, V2, Weight} = ugraph:edge(Map, Current_Pos),
            EdgeKey = sets:from_list([V1, V2]),
            NewEdgeCaps = update_num_people(EdgeKey, EdgeCaps, -1),
            {Base, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps), 
            update_weight(Map, E, NumPeople, Cap, Weight, u);
        {next_edge, From, Current_Pos, Destination} -> 
            NewEdgeCaps = EdgeCaps,
            spawn(map, next_edge, [From, Current_Pos, Destination, Map])
    end,
    loop(Map, NewEdgeCaps).

update_num_people(EdgeKey, EdgeCaps, Num) ->
    {Base, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps),
    dict:store(EdgeKey, {Base, Cap, NumPeople + Num}, EdgeCaps).

update_weight(G, E, NumPeople, Cap, Weight, s) when NumPeople > Cap ->
    {_, V1, V2, _} = ugraph:edge(G, E),
    ugraph:update_edge(G, E, V1, V2, Weight + 3);
update_weight(G, E, NumPeople, Cap, Weight, u) when NumPeople > Cap ->
    {_, V1, V2, _} = ugraph:edge(G, E),
    ugraph:update_edge(G, E, V1, V2, Weight - 3);
update_weight(_, _, NumPeople, Cap, _, _) when NumPeople =< Cap ->
    ok.


next_edge(From, Current_Pos, Destination, Map) ->
    case ugraph:shortest_path(Map, Current_Pos, Destination) of
        [NextEdge | _Path] ->
            {_, _, V2, Weight} = ugraph:edge(Map, NextEdge),
            From ! {NextEdge, V2, Weight};
        [] -> 
            erlang:error(current_dest_same) 
    end.
