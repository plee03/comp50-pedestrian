% Loader module

-module(loader).
-export([start/3, create_map/3]).

start(Schedule, Graph, Output_File) -> 
    case file:open(Output_File, write) of
        {ok, Open_File} -> {ok, Open_File};
        {error, Reason} -> Open_File = "",
                           exit(Reason)
    end, 
    case file:consult(Schedule) of
        {ok, Terms} -> Pids = spawn_people(Terms, []);
        {error, Error} -> Error,
                          Pids = []
    end,
    case file:consult(Graph) of
        {ok, GraphTerms} ->
            map:start(GraphTerms);
        {error, GraphError} -> GraphError
    end,
    proxy:start(Pids, Open_File).

spawn_people([], Pids) -> Pids;
spawn_people([F|Files], Pids) -> 
    case file:consult(F) of 
        {ok, Terms} -> 
            P = person:start(Terms),
            spawn_people(Files, [P | Pids]);
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

