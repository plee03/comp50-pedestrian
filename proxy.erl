% Proxy Module

-module(proxy).
-export([start/2]).
-export([loop/3, update_loc/2]).

-define(STOP_TIME, 1440).

start(List, Output_File) -> 
    io:format(Output_File, "[", []),
    register(proxy_server, spawn(proxy, loop, [0, List, Output_File])).

% People - {Pid, Location}
loop(Time, _People, Output_File) when Time == ?STOP_TIME -> io:format(Output_File, "]\n", []);
loop(Time, People, Output_File)  when Time == ?STOP_TIME - 1 -> 
    %io:format("Time: ~p\n", [Time]),
    People_Data = rpc:pmap({proxy, update_loc}, [Time], People),
    print_step(Time, People_Data, Output_File),
    loop(Time+1, People, Output_File);
loop(Time, People, Output_File) -> 
    %io:format("Time: ~p\n", [Time]),
    People_Data = rpc:pmap({proxy, update_loc}, [Time], People),
    print_step(Time, People_Data, Output_File),
    io:format(Output_File, ",\n", []),
    loop(Time+1, People, Output_File).

update_loc(Pid, Time) ->
    Pid ! {self(), Time},
    receive
        {Pid, NewLocation, Progress, Distance} -> 
            [Pid, NewLocation, [Progress, Distance]]
            %io:format("Pid: ~p, Loc: ~p, Progress: ~p / ~p\n", [Pid, NewLocation, Progress, Distance]) 
    end.
    
print_step([], Output_File) -> 
    io:format(Output_File, "]}", []); 
print_step([ Person | []], Output_File) -> 
    io:format(Output_File, '{"Pid": "~w", "Location":"~s", "Progress":~w}', Person),
    print_step([], Output_File);
print_step([ Person | People_Data], Output_File) -> 
    io:format(Output_File, '{"Pid": "~w", "Location":"~s", "Progress":~w},', Person),
    print_step(People_Data, Output_File).

print_step(Time, People_Data, Output_File) -> 
    io:format(Output_File, '{"Time": "~w", "People": [', [Time]),
    io:format("TIME: ~p\nPEOPLE: ~p\n", [Time, People_Data]),
    print_step(People_Data, Output_File).
