% Proxy Module
% by: Peter Lee and Caitlin Klein
% date: December 10, 2015
% description: keeps track of time, sends the time to all of the persons and 
%              receives from them their current location, the progress they've
%              made on a path along with the distance of that path

-module(proxy).
-export([start/2, loop/3, update_loc/2]).

% determines the number of ticks the program runs for
-define(STOP_TIME, 1440).

% registers the proxy process
% prints an open bracket in compliance with JSON format
start(List, Output_File) -> 
    io:format(Output_File, "[", []),
    register(proxy_server, spawn(proxy, loop, [0, List, Output_File])).

% makes function calls to receive updated locations and print those to a file
% in JSON format and increases the time
loop(Time, _People, Output_File) when Time == ?STOP_TIME -> 
    io:format(Output_File, "]\n", []);
loop(Time, People, Output_File)  when Time == ?STOP_TIME - 1 -> 
    People_Data = rpc:pmap({proxy, update_loc}, [Time], People),
    print_step(Time, People_Data, Output_File),
    loop(Time+1, People, Output_File);
loop(Time, People, Output_File) -> 
    People_Data = rpc:pmap({proxy, update_loc}, [Time], People),
    print_step(Time, People_Data, Output_File),
    io:format(Output_File, ",\n", []),
    loop(Time+1, People, Output_File).

% sends a person the current time and receives back their location
update_loc(Pid, Time) ->
    Pid ! {self(), Time},
    receive
        {Pid, NewLocation, Progress, Distance} -> 
            [Pid, NewLocation, [Progress, Distance]]
    end.

% handles printng the data in JSON format
print_step([], Output_File) -> 
    io:format(Output_File, "]}", []); 
print_step([ Person | []], Output_File) -> 
    io:format(Output_File, "{Pid: ~w, Location:~w, Progress: ~w}", Person),
    print_step([], Output_File);
print_step([ Person | People_Data], Output_File) -> 
    io:format(Output_File, "{Pid: ~w, Location:~w, Progress: ~w},", Person),
    print_step(People_Data, Output_File).
print_step(Time, People_Data, Output_File) -> 
    io:format(Output_File, "{Time: ~w, People: [", [Time]),
    print_step(People_Data, Output_File).

