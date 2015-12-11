% Person Module

-module(person).
-export([start/1]).
-export([loop/7]).
-export([start_ex/0]).
-export([test/0]).
-export([p_test/0]).
-export([new_progress/3]).

p_test() ->
    Pid = person:start_ex(),
    io:format("Pid: ~p\n", [Pid]),
    Pid2 = person:start([{0, halligan}, {1, braker}, {9, pearson}], Pid),
    Pid3 = person:start([{0, halligan}, {4, braker}, {13, pearson}], Pid),
    proxy:start([Pid2, Pid3]).

start([{_Start, Start_Loc}|Schedule]) -> 
   spawn(person, loop, [departing, dict:from_list(Schedule), Start_Loc, [], Start_Loc, 0, 0]).

loop(arriving, Schedule, Current_Loc, Next_Loc, Final_Loc, Progress, Distance) ->
    %io:format("~p: ~p, ~p, ~p, ~p / ~p\n", [arriving, Current_Loc, Next_Loc, Final_Loc, Progress, Distance]),
    case  Progress >= Distance of 
        true ->
            map_server ! {unsubscribe, Current_Loc},
            loop(departing, Schedule, Next_Loc, Next_Loc, Final_Loc, 1, 0);
        false -> 
            get_time(Current_Loc, Progress, Distance),
            Weight = get_weight(Current_Loc),
            NewDistance = min(Weight, Distance),
            NewProgress = new_progress(Progress, Distance, NewDistance),
            loop(arriving, Schedule, Current_Loc, Next_Loc, Final_Loc, NewProgress + 1, NewDistance)
    end;

loop(departing, Schedule, Current_Loc, Next_Loc, Final_Loc, Progress, Distance) ->
    %io:format("~p: ~p, ~p, ~p, ~p / ~p\n", [departing, Current_Loc, Next_Loc, Final_Loc, Progress, Distance]),
    Time = get_time(Current_Loc, Progress, Distance),
    %io:format("Time: ~p\n", [Time]),
    case dict:is_key(Time, Schedule) of 
        true when Current_Loc == Final_Loc -> {ok, New_Final_Loc} = dict:find(Time, Schedule);
        _ -> New_Final_Loc = Final_Loc 
    end,
    case Current_Loc /= New_Final_Loc of
        true -> 
            map_server ! {next_edge, self(), Current_Loc, New_Final_Loc},
            receive 
                {New_Current_Loc, New_Next_Loc, New_Distance} -> {New_Current_Loc, New_Next_Loc, New_Distance}
            end,
            map_server ! {subscribe, New_Current_Loc},
            loop(arriving, Schedule, New_Current_Loc, New_Next_Loc, New_Final_Loc, 1, New_Distance);
        false -> 
            loop(departing, Schedule, Current_Loc, Next_Loc, Final_Loc, 1, 0)
    end.

new_progress(Progress, Distance, NewDistance) ->
    round(Progress / (float(Distance) / float(NewDistance))).



get_weight(Location) -> 
    map_server ! {path_weight, self(), Location},
    receive 
        {weight, Weight}-> Weight
    end.

get_time(Location, Progress, Distance) -> 
    receive
        {From, Time} -> From ! {self(), Location, Progress, Distance}
    end,
    Time.

start_ex() ->
    spawn(person, test, []).

test() ->
    receive
        {shortest_path, From, halligan, braker} -> From ! {brakerhalligan, braker, 4};
        {shortest_path, From, braker, pearson} -> From ! {brakerpearson, pearson, 5};
        {shortest_path, _From, Start, End} -> io:format("doesn't match: ~p, ~p\n", [Start, End]);
        {subscribe, _} -> ok;
        {unsubscribe, _} -> ok
    end,
    test().
