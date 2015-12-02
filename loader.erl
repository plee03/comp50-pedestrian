% Loader module

-module(loader).
-export([start/1]).

start(Schedule) -> 
    case file:consult(Schedule) of
        {ok, Terms} -> spawn_people(Terms, []);
        {error, Error} -> Error
    end.

spawn_people([], Pids) -> Pids;
spawn_people([F|Files], Pids) -> 
    case file:consult(F) of 
        {ok, Terms} -> P = person:start(Terms),
            spawn_people(Files, [P | Pids]);
        {error, Error} -> Error
    end.
