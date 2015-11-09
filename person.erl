% Person Module

-module(person).
-export([start/2]).
-export([loop/2]).

start(Schedule, Init_Pos) -> 
   spawn(person, loop, [Schedule, Init_Pos]).

loop(Schedule, {Location, Distance}) ->
    receive
        {From, _Time} -> From ! {self(), {Location, Distance}}
    end,
    % find next dest
    % loop(NewSchedule, {path, Node, Dest, 1/weight
    loop(Schedule, {Location, Distance}).

