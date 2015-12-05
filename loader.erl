% Loader module

-module(loader).
-export([start/2, create_map/3]).

start(Schedule, Graph) -> 
    case file:consult(Schedule) of
        {ok, Terms} -> Pids = spawn_people(Terms, []),
                       proxy:start(Pids);
        {error, Error} -> Error
    end,
    case file:consult(Graph) of
        {ok, GraphTerms} ->
            map:start(GraphTerms);
        {error, GraphError} -> GraphError
    end.

spawn_people([], Pids, _) -> Pids;
spawn_people([F|Files], Pids, Map_Pid) -> 
    case file:consult(F) of 
        {ok, Terms} -> 
            P = person:start(Terms, Map_Pid),
            spawn_people(Files, [P | Pids], Map_Pid);
        {error, Error} -> Error
    end.

spawn_map(Graph) ->
    Map = ugraph:new(),
    case file:consult(Graph) of
        {ok, Terms} -> 
            {G, EdgeCaps} = create_map(Terms, Map, dict:new()),
            map:start(G, EdgeCaps),
            ets:give_away(edges, whereis(map_server), none),
            ets:give_away(vertices, whereis(map_server), none),
            ets:give_away(neighbors, whereis(map_server), none),
            map_server ! ets_tables;
        {error, Error} -> Error
    end.
        
create_map([], G, EdgeCaps) -> {G, EdgeCaps};
create_map([{V1, V2, Weight, Capacity} | Terms], G, EdgeCaps) -> 
    ugraph:add_edge(G, V1, V2, Weight),
    UpdatedEdgeCaps = dict:store(sets:from_list([V1, V2]), {Weight, Capacity, 0}, EdgeCaps),
    create_map(Terms, G, UpdatedEdgeCaps);
create_map([V | Terms], G, EdgeCaps) -> 
    ugraph:add_vertex(G, V),
    create_map(Terms, G, EdgeCaps).

