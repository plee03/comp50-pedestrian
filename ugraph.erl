% ugraph.erl
% By Peter Lee, Caitlin Klein
% December 10, 2015
%
% This ugraph module provides a simple interface for creating an undirected 
% graph.

-module(ugraph).
-export([new/0, add_edge/4, add_vertex/2, add_vertex/3, edge/2,
         vertex/2, vertices/1, shortest_path/3, update_edge/5]).

%% Returns a new undirected graph
new() -> digraph:new().

%% Given a graph and a vertex name, adds the vertex to the graph
add_vertex(G, V) -> digraph:add_vertex(G, V).

%% Given a graph, vertex name, and a label, adds the vertex to the graph
add_vertex(G, V, Label) -> digraph:add_vertex(G, V, Label).

%% Returns a list of the vertices in a given graph 
vertices(G) -> digraph:vertices(G).

%% Returns information for a vertex in the form {Vertex, Label}
%% Returns false if the vertex does not exist in the graph.
vertex(G, V) -> digraph:vertex(G, V).

%% Given a graph, two vertices, and a label, adds the corresponding edge
%% to the graph.
add_edge(G, V1, V2, Label) ->
    StrV1V2 = lists:flatten(io_lib:format("~w#~w", [V1, V2])),
    StrV2V1 = lists:flatten(io_lib:format("~w#~w", [V2, V1])),
    digraph:add_edge(G, StrV2V1, V2, V1, Label),
    digraph:add_edge(G, StrV1V2, V1, V2, Label).

%% Returns information for an edge, in the form of {E, V1, V2, Label}
%% where V1 and V2 are the connecting vertices and the label is the label.
%% Returns false if the edge does not exist in the graph.
edge(G, E) -> digraph:edge(G, E).

%% Updates a specified edge's label with the new specified label.
update_edge(G, E, V1, V2, Label) ->
    digraph:add_edge(G, E, V1, V2, Label),
    InEdges = digraph:in_edges(G, V1),
    E2 = find_edge(G, V2, V1, InEdges),
    digraph:add_edge(G, E2, V2, V1, Label).

%% Given a graph and two vertices, returns a list of the edges representing
%% the shortest path from V1 to V2.
%% Returns false if V2 cannot be reached from V1. 
shortest_path(G, V1, V2) -> 
    Unvisited = digraph:vertices(G),
    Distances = lists:foldl(fun(V, D) when V1 == V -> 
                                dict:store(V1, {start, 0}, D);
                               (V, D)  -> 
                                dict:store(V, {none, -1}, D) 
                            end, dict:new(), Unvisited),
    shortest_path(G, V1, V2, Unvisited, Distances).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                             Helper Functions                            %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       

% Given a graph and vertices (V1, V2), and a list of edges, finds an edge going
% from V1 to V2 in the list of edges. Returns false if no match is found.
find_edge(_, _, _, []) -> false;
find_edge(G, V1, V2, [E | Edges]) ->
    case edge(G, E) of 
        {_, V1, V2, _} -> E;
        {_, _, _, _} -> find_edge(G, V1, V2, Edges)
    end.



% Helper function for Dijkstra's shortest path, follows recorded previous 
% vertices and returns a list of edges representing the shortest path.
get_path(V, Distances, Path) ->
    case dict:fetch(V, Distances) of
        {start, _} -> Path;
        {none, _} -> false;
        {{E, V1}, _} -> get_path(V1, Distances, [E | Path])
    end.
    
% Takes a graph, current vertex, destination vertex, a list of unvisited
% vertices, and a dictionary of vertices and tentative distances. Returns a
% list of edges representing shortest path.
shortest_path(_G, DesV, DesV, _Unvisited, Distances) -> 
    get_path(DesV, Distances, []);
shortest_path(G, CurrentV, DesV, Unvisited, Distances) -> 
    NewUnvisited = lists:delete(CurrentV, Unvisited), 
    % For our current edge, get a list of all its edges leading to unvisited
    % vertices
    OutEdges = [edge(G, Edge) || Edge <- digraph:out_edges(G, CurrentV)],
    UnvisitedOutEdges = lists:filter(fun ({_E, _V1, V2, _Weight}) ->
                                          lists:member(V2, Unvisited)
                                     end, OutEdges),
    {_, CurrentVDistance} = dict:fetch(CurrentV, Distances),

    % Calculate new tentative distance for each neighboring edge, and replace
    % in distance dictionary if the distance is better
    NewDistances = lists:foldl(fun ({E, V1, V2, Weight}, DistDict) ->
                                    NewDistance = CurrentVDistance + Weight,
                                    case dict:fetch(V2, DistDict) of
                                        {_, -1} -> 
                                            dict:store(V2, 
                                                       {{E, V1}, NewDistance},
                                                       DistDict);
                                        {_, D} when D > NewDistance -> 
                                            dict:store(V2, 
                                                       {{E, V1}, NewDistance},
                                                       DistDict);
                                        {_, D} when D =< NewDistance -> 
                                            DistDict
                                    end
                               end, Distances, UnvisitedOutEdges),

    %% Choosing unvisited vertex with shortest tentative distance to traverse
    %% next.
    {NextV, _} = lists:foldl(fun (V2, {NextV, NextVDist}) ->
                            case dict:fetch(V2, NewDistances) of
                                {_, D} when NextVDist == -1 -> {V2, D};
                                {_, D} when D == -1 -> {NextV, NextVDist};
                                {_, D} when D < NextVDist -> {V2, D};
                                {_, D} when D >= NextVDist -> 
                                    {NextV, NextVDist}
                            end
                       end, {none, -1}, NewUnvisited),
    shortest_path(G, NextV, DesV, NewUnvisited, NewDistances).
