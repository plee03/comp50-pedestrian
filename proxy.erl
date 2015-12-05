% Proxy Module

-module(proxy).
-export([start/1]).
-export([loop/2, update_loc/2]).

start(List) -> 
    spawn(proxy, loop, [0, List]).

% People - {Pid, Location}
loop(22, _People) -> ok;
loop(Time, People) -> 
    io:format("Time: ~p\n", [Time]),
    %New_People = rpc:pmap({proxy, update_loc}, [Time], People),
    rpc:pmap({proxy, update_loc}, [Time], People),
    loop(Time+1, People).

update_loc(Pid, Time) ->
    Pid ! {self(), Time},
    receive
        {Pid, NewLocation, Progress, Distance} -> 
            %{Pid, NewLocation, Progress, Distance},
            io:format("Pid: ~p, Loc: ~p, Progress: ~p / ~p\n", [Pid, NewLocation, Progress, Distance]) 
    end.
    
