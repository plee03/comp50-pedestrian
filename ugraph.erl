%
% graph.erl
% Undirected weighted graph module

-module(ugraph).
-export([new/0, add_edge/4, add_vertex/2, add_vertex/3, edge/2,
         vertex/2, vertices/1, shortest_path/3, update_edge/5]).

new() -> digraph:new().

add_vertex(G, V) -> digraph:add_vertex(G, V).

add_vertex(G, V, Label) -> digraph:add_vertex(G, V, Label).

vertices(G) -> digraph:vertices(G).

vertex(G, V) -> digraph:vertex(G, V).

add_edge(G, V1, V2, Label) ->
    digraph:add_edge(G, V2, V1, Label),
    digraph:add_edge(G, V1, V2, Label).

update_edge(G, E, V1, V2, Label) ->
    digraph:add_edge(G, E, V1, V2, Label),
    InEdges = digraph:in_edges(G, V1),
    E2 = find_edge(G, V2, V1, InEdges),
    digraph:add_edge(G, E2, V2, V1, Label).

% Given a graph and vertices (V1, V2), and a list of edges, finds an edge going
% from V1 to V2 in the list of edges. Returns false if no match is found.
find_edge(_, _, _, []) -> false;
find_edge(G, V1, V2, [E | Edges]) ->
    case edge(G, E) of 
        {_, V1, V2, _} -> E;
        {_, _, _, _} -> find_edge(G, V1, V2, Edges)
    end.

edge(G, E) -> digraph:edge(G, E).

shortest_path(G, V1, V2) -> 
    Unvisited = digraph:vertices(G),
    % Making a dictionary of (vertex, (prev, tentative-distance)) pairs
    % where prev is the previous vertex, initialized to start for V1, and none for 
    % every other vertex.
    Distances = lists:foldl(fun(V, D) when V1 == V -> dict:store(V1, {start, 0}, D);
                               (V, D)  -> dict:store(V, {none, -1}, D) end, dict:new(), 
                                                                    Unvisited),
    shortest_path(G, V1, V2, Unvisited, Distances).

% Helper function for shortest path, follows previous vertices to find the
% shortest path.
get_path(V, Distances, Path) ->
    case dict:fetch(V, Distances) of
        {start, _} -> Path;
        {{E, V1}, _} -> get_path(V1, Distances, [E | Path])
    end.
    
relax({E, V1, V2, Weight}, DistDict) -> 
    {_, CurrentVDistance} = dict:fetch(V1, DistDict),
    NewDistance = CurrentVDistance + Weight,
    case dict:fetch(V2, DistDict) of
        {_, -1} -> 
            dict:store(V2, {{E, V1}, NewDistance}, DistDict);
        {_, D} when D > NewDistance -> 
            dict:store(V2, {{E, V1}, NewDistance}, DistDict);
        {_, D} when D =< NewDistance -> 
            DistDict
    end.
    
    

% Takes a graph, current vertex, destination vertex, a list of unvisited
% vertices, and a dictionary of vertices and tentative distances
shortest_path(_G, DesV, DesV, _Unvisited, Distances) -> get_path(DesV, Distances, []);
shortest_path(G, CurrentV, DesV, Unvisited, Distances) -> 
    NewUnvisited = lists:delete(CurrentV, Unvisited), 
    % For our current edge, get a list of all its neighbors
    Out_edges = [edge(G, Edge) || Edge <- digraph:out_edges(G, CurrentV)],
    UnvisitedOutEdges = lists:filter(fun ({_E, _V1, V2, _Weight}) ->
                                          lists:member(V2, Unvisited)
                                     end, Out_edges),
    {_, CurrentVDistance} = dict:fetch(CurrentV, Distances),

    % Calculate new tentative distance for each neighboring edge, and replace
    % in distance dictionary if the distance is better
    NewDistances = lists:foldl(fun ({E, V1, V2, Weight}, DistDict) ->
                                        NewDistance = CurrentVDistance + Weight,
                                        case dict:fetch(V2, DistDict) of
                                            {_, -1} -> 
                                                dict:store(V2, {{E, V1}, NewDistance}, DistDict);
                                            {_, D} when D > NewDistance -> 
                                                dict:store(V2, {{E, V1}, NewDistance}, DistDict);
                                            {_, D} when D =< NewDistance -> 
                                                DistDict
                                        end
                               end, Distances, UnvisitedOutEdges),

    {NextV, _} = lists:foldl(fun (V2, {NextV, NextVDist}) ->
                            io:fwrite("nextV: ~w, nextdist: ~w~n", [NextV, NextVDist]),
                            io:fwrite("V: ~w, dist: ~w~n", [V2, dict:fetch(V2,
                            NewDistances)]),
                            case dict:fetch(V2, NewDistances) of
                                {_, D} when NextVDist == -1 -> {V2, D};
                                {_, D} when D == -1 -> {NextV, NextVDist};
                                {_, D} when D < NextVDist -> {V2, D};
                                {_, D} when D >= NextVDist -> {NextV, NextVDist}
                            end
                       end, {none, -1}, NewUnvisited),
    shortest_path(G, NextV, DesV, NewUnvisited, NewDistances).
