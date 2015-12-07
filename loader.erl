% Loader module

-module(loader).
-export([start/3]).

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


