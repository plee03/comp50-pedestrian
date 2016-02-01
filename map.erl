% map.erl
% By Peter Lee, Caitlin Klein
% December 10, 2015
% 
% This module implements a map server that receives and responds to messages
% that update or query a graph.

-module(map).
-export([start/1]).
-export([next_edge/4, init/1]).

%% Starts a new map server with the registered name map_server.
%% GraphTerms must be a list containing either atoms or {V1, V2, Weight,
%% Capacity} tuples where V1 and V2 are atoms and Weight and Capacity are
%% numbers.
start(GraphTerms) ->
    register(map_server, spawn(map, init, [GraphTerms])).

%% Initializes a graph according to GraphTerm, and starts the main server loop.
init(GraphTerms) ->
    {Map, EdgeCaps} = create_map(GraphTerms, ugraph:new(), dict:new()),
    loop(Map, EdgeCaps).
    
%% The main server loop for the map module.
%% Map is a graph, and EdgeCaps is a dictionary containing base weight,
%% capacity, and number of people for each edge.
%%
%% The server responds to the following messages:
%% subscribe - increments the number of people on an edge and updates the edge's
%%             weight in the graph if needed. 
%% unsubscribe - same as subscribe, but decrements.
%% path_weight - returns the current weight of a path to the message sender.
%% next_edge - returns the next edge, its weight, and the next vertex in the 
%%             shortest path from Current_Pos to Destination to the 
%%             message sender.
loop(Map, EdgeCaps) ->
    receive
        {subscribe, Current_Pos} -> 
            {E, V1, V2, Weight} = ugraph:edge(Map, Current_Pos),
            EdgeKey = make_edge_key(V1, V2),
            NewEdgeCaps = update_num_people(EdgeKey, EdgeCaps, 1),
            {_, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps), 
            update_weight(Map, E, NumPeople, Cap, Weight, s);
        {path_weight, From, Edge} ->
            NewEdgeCaps = EdgeCaps,
            {_, _, _, Weight} = ugraph:edge(Map, Edge),
            From ! {weight, Weight};
        {unsubscribe, Current_Pos} -> 
            {E, V1, V2, Weight} = ugraph:edge(Map, Current_Pos),
            EdgeKey = make_edge_key(V1, V2),
            NewEdgeCaps = update_num_people(EdgeKey, EdgeCaps, -1),
            {_, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps), 
            update_weight(Map, E, NumPeople, Cap, Weight, u);
        {next_edge, From, Current_Pos, Destination} -> 
            NewEdgeCaps = EdgeCaps,
            spawn(map, next_edge, [From, Current_Pos, Destination, Map])
    end,
    loop(Map, NewEdgeCaps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                             Helper Functions                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Updates the number of people on an edge specified by EdgeKey in the
%% dictionary EdgeCaps by Num.
update_num_people(EdgeKey, EdgeCaps, Num) ->
    {Base, Cap, NumPeople} = dict:fetch(EdgeKey, EdgeCaps),
    dict:store(EdgeKey, {Base, Cap, NumPeople + Num}, EdgeCaps).

%% If necessary, updates the weight of an edge in the graph depending on the
%% number of people NumPeople and the capacity Cap. 
%% The final argument represents whether a person has just subscribed or
%% unsubscribed, with the atoms s and u respectively.
update_weight(G, E, NumPeople, Cap, Weight, s) when NumPeople >= Cap ->
    {_, V1, V2, _} = ugraph:edge(G, E),
    ugraph:update_edge(G, E, V1, V2, Weight + 2);
update_weight(G, E, NumPeople, Cap, Weight, u) when NumPeople > Cap ->
    {_, V1, V2, _} = ugraph:edge(G, E),
    ugraph:update_edge(G, E, V1, V2, Weight - 2);
update_weight(_, _, NumPeople, Cap, _, _) when NumPeople =< Cap ->
    ok.

%% Sends to the PID From a tuple containing the next edge, next vertex, and the
%% edge's weight in the shortest path from Current_Pos to Destination.
%% If Current_Pos and Destination are the same vertex, raises an error.
next_edge(From, Current_Pos, Destination, Map) ->
    case ugraph:shortest_path(Map, Current_Pos, Destination) of
        [NextEdge | _Path] ->
            {_, _, V2, Weight} = ugraph:edge(Map, NextEdge),
            From ! {NextEdge, V2, Weight};
        [] -> 
            erlang:error(current_dest_same) 
    end.

%% Given a list of Erlang terms representing a graph, creates a returns a graph
%% and a dictionary containing edge information.
create_map([], G, EdgeCaps) -> {G, EdgeCaps};
create_map([{V1, V2, Weight, Capacity} | Terms], G, EdgeCaps) -> 
    ugraph:add_edge(G, V1, V2, Weight),
    EdgeKey = make_edge_key(V1, V2),
    UpdatedEdgeCaps = dict:store(EdgeKey, {Weight, Capacity,0}, EdgeCaps),
    create_map(Terms, G, UpdatedEdgeCaps);
create_map([V | Terms], G, EdgeCaps) -> 
    ugraph:add_vertex(G, V),
    create_map(Terms, G, EdgeCaps).

%% Returns a unique edge identifier for an edge given two vertices.
make_edge_key(V1, V2) when V1 > V2 -> {V1, V2};
make_edge_key(V1, V2) when V1 =< V2 -> {V2, V1}.
