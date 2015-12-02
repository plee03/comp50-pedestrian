% Person Module

-module(person).
-export([start/1]).
-export([loop/1]).

start(Schedule) -> 
   spawn(person, loop, [Schedule]).

%loop(Schedule, Current_Loc, Next_Loc, Final_Loc, Progress, Distance) ->
loop(Schedule) ->
    io:format("~p: ~p\n", [self(), Schedule]).
    %receive
    %    {From, _Time} -> From ! {self(), {Location, Distance}}
    %end,
    %if lookup
    %    New_Final_Loc = Value,
    %    Map ! {next_path, self(), Current_Loc, New_Final_Loc},
    %    receive
    %        {next_loc, Next} -> New_Next_Loc = Next,
    %    end,
    %    Map ! {subscribe, self(), Current_Loc, New_Next_Loc


