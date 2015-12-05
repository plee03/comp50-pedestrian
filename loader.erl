% Loader module

-module(loader).
-export([start/1]).

start(Schedule) -> 
    Map_Pid = person:start_ex(),
    case file:consult(Schedule) of
        {ok, Terms} -> Pids = spawn_people(Terms, [], Map_Pid),
                       proxy:start(Pids);
        {error, Error} -> Error
    end.

spawn_people([], Pids, _) -> Pids;
spawn_people([F|Files], Pids, Map_Pid) -> 
    case file:consult(F) of 
        {ok, Terms} -> 
            P = person:start(Terms, Map_Pid),
            spawn_people(Files, [P | Pids], Map_Pid);
        {error, Error} -> Error
    end.
