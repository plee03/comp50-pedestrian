% Map Module

-module(map).
-export([start/1]).
-export([loop/2, next_edge/5, init/1]).

start(GraphTerms) ->
    register(map_server, spawn(map, init, [GraphTerms])).

init(GraphTerms) ->
    {Map, EdgeCaps} = create_map(GraphTerms, ugraph:new(), dict:new()),
    loop(Map, EdgeCaps).
    

loop(Map, EdgeCaps) ->
    receive
        {subscribe, Current_Pos} -> 
            {E, V1, V2, Weight} = ugraph:edge(Map, Current_Pos),
            %io:fwrite("V1: ~w V2: ~w~n", [V1, V2]),
            EdgeKey = make_edge_key(V1, V2),
            %io:format("UEDGEKEY: ~p\n", [EdgeKey]),
            NewEdgeCaps = update_num_people(EdgeKey, EdgeCaps, 1),
            {Base, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps), 
            %io:fwrite('~w people~n', [NumPeople]),
            %io:fwrite("weight: ~w~n", [Weight]),
            update_weight(Map, E, NumPeople, Cap, Weight, s);
        {path_weight, From, Edge} ->
            NewEdgeCaps = EdgeCaps,
            {_, _, _, Weight} = ugraph:edge(Map, Edge),
            From ! {weight, Weight};
        {unsubscribe, Current_Pos} -> 
            {E, V1, V2, Weight} = ugraph:edge(Map, Current_Pos),
            EdgeKey = make_edge_key(V1, V2),
            %io:format("UEDGEKEY: ~p\n", [EdgeKey]),
            %io:fwrite("weight: ~w~n", [Weight]),
            NewEdgeCaps = update_num_people(EdgeKey, EdgeCaps, -1),
            {Base, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps), 
            update_weight(Map, E, NumPeople, Cap, Weight, u);
        {next_edge, From, Current_Pos, Destination} -> 
            %io:fwrite("~w Going from ~w to ~w~n", [From, Current_Pos, Destination]),
            NewEdgeCaps = EdgeCaps,
            spawn(map, next_edge, [From, Current_Pos, Destination, Map, EdgeCaps])
    end,
    loop(Map, NewEdgeCaps).

update_num_people(EdgeKey, EdgeCaps, Num) ->
    {Base, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps),
    dict:store(EdgeKey, {Base, Cap, NumPeople + Num}, EdgeCaps).

update_weight(G, E, NumPeople, Cap, Weight, s) when NumPeople >= Cap ->
    {_, V1, V2, _} = ugraph:edge(G, E),
    ugraph:update_edge(G, E, V1, V2, Weight + 2);
update_weight(G, E, NumPeople, Cap, Weight, u) when NumPeople > Cap ->
    {_, V1, V2, _} = ugraph:edge(G, E),
    ugraph:update_edge(G, E, V1, V2, Weight - 2);
update_weight(_, _, NumPeople, Cap, _, _) when NumPeople =< Cap ->
    ok.


next_edge(From, Current_Pos, Destination, Map, EdgeCaps) ->
    case ugraph:shortest_path(Map, Current_Pos, Destination) of
        [NextEdge | _Path] ->
            {_, V1, V2, Weight} = ugraph:edge(Map, NextEdge),
            {Base, _, _} = dict:fetch(make_edge_key(V1, V2), EdgeCaps),
            From ! {NextEdge, V2, Weight};
        [] -> 
            erlang:error(current_dest_same) 
    end.

create_map([], G, EdgeCaps) -> {G, EdgeCaps};
create_map([{V1, V2, Weight, Capacity} | Terms], G, EdgeCaps) -> 
    ugraph:add_edge(G, V1, V2, Weight),
    EdgeKey = make_edge_key(V1, V2),
    UpdatedEdgeCaps = dict:store(EdgeKey, {Weight, Capacity,0}, EdgeCaps),
    create_map(Terms, G, UpdatedEdgeCaps);
create_map([V | Terms], G, EdgeCaps) -> 
    ugraph:add_vertex(G, V),
    create_map(Terms, G, EdgeCaps).

make_edge_key(V1, V2) when V1 > V2 -> {V1, V2};
make_edge_key(V1, V2) when V1 =< V2 -> {V2, V1}.
