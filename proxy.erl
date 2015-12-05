% Proxy Module

-module(proxy).
-export([start/1]).
-export([loop/2, update_loc/2]).

start(List) -> 
    register(proxy_server, spawn(proxy, loop, [0, List])).

% People - {Pid, Location}
loop(Time, People) -> 
    New_People = rpc:pmap({proxy, update_loc}, [Time], People),
    loop(Time+1, New_People).

update_loc({Pid, _Location}, Time) ->
    Pid ! {self(), Time},
    receive
        {Pid, NewLocation} -> {Pid, NewLocation}
    end.
    
