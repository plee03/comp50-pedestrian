%
% graph.erl
% Undirected weighted graph module

-module(ugraph).
-export([new/0, add_edge/4, add_edge/5, add_vertex/3, edge/2,
         vertex/2, vertices/1, shortest_path/3]).

new() -> digraph:new().

add_vertex(G, V, Label) -> digraph:add_vertex(G, V, Label).

vertices(G) -> digraph:vertices(G).

vertex(G, V) -> digraph:vertex(G, V).

add_edge(G, V1, V2, Label) ->
    digraph:add_edge(G, V1, V2, Label),
    digraph:add_edge(G, V2, V1, Label).

add_edge(G, E, V1, V2, Label) ->
    digraph:add_edge(G, E, V1, V2, Label),
    digraph:add_edge(G, E, V2, V1, Label).


edge(G, E) -> digraph:edge(G, E).

shortest_path(G, V1, V2) -> 
    Unvisited = digraph:vertices(G),
    % Making a dictionary of (vertex, tentative-distance) pairs
    Distances = lists:foldl(fun(V, D) when V1 == V -> dict:store(V1, 0, D);
                               (V, D)  -> dict:store(V, -1, D) end, dict:new(), 
                                                                    Unvisited),
    shortest_path(G, V1, V2, Unvisited, Distances).

shortest_path(G, Current_V, V2, Unvisited, Distances) -> 
    Out_edges = digraph:out_edges(G, Current_V).
    lists:foldl(fun (Edge, Dist) -> , Out_edges).
    
         

     

