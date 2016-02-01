% Loader module
% by: Peter Lee and Caitlin Klein
% date: December 10, 2015
% description: Used to load all of the information necessary for the 
%              pedestrian model. Spawns the map, proxy, and persons with 
%              the given information.

-module(loader).
-export([start/3]).

% starts up the pedestrian program given a file with a list of schedule files,
% a file containing the graph information, and a file to output the contents 
% of the program to the scedhule and graph files are of a specified format
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
    proxy:start(Pids, Open_File),
    ok.

% spawns individual persons with their associated schedule file
spawn_people([], Pids) -> Pids;
spawn_people([F|Files], Pids) -> 
    case file:consult(F) of 
        {ok, Terms} -> 
            P = person:start(Terms),
            spawn_people(Files, [P | Pids]);
        {error, Error} -> Error
    end.


