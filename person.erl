% Person Module

-module(person).
-export([start/2]).
-export([loop/8]).
-export([start_ex/0]).
-export([test/0]).
-export([p_test/0]).

p_test() ->
    Pid = person:start_ex(),
    io:format("Pid: ~p\n", [Pid]),
    Pid2 = person:start([{0, halligan}, {1, braker}, {9, pearson}], Pid),
    Pid3 = person:start([{0, halligan}, {4, braker}, {13, pearson}], Pid),
    proxy:start([Pid2, Pid3]).

start([{_Start, Start_Loc}|Schedule], Pid) -> 
   spawn(person, loop, [departing, dict:from_list(Schedule), Start_Loc, [], Start_Loc, 0, 0, Pid]).

loop(arriving, Schedule, Current_Loc, Next_Loc, Final_Loc, Progress, Distance, Pid) ->
    %io:format("~p: ~p, ~p, ~p, ~p / ~p\n", [arriving, Current_Loc, Next_Loc, Final_Loc, Progress, Distance]),
    get_time(Current_Loc, Progress, Distance),
    case  Progress >= Distance of 
        true ->
            Pid ! {unsubscribe, Current_Loc},
            loop(departing, Schedule, Next_Loc, Next_Loc, Final_Loc, 0, 0, Pid);
        false -> 
        loop(arriving, Schedule, Current_Loc, Next_Loc, Final_Loc, Progress + 1, Distance, Pid)
    end;

loop(departing, Schedule, Current_Loc, Next_Loc, Final_Loc, Progress, Distance, Pid) ->
    %io:format("~p: ~p, ~p, ~p, ~p / ~p\n", [departing, Current_Loc, Next_Loc, Final_Loc, Progress, Distance]),
    Time = get_time(Current_Loc, Progress, Distance),
    %io:format("Time: ~p\n", [Time]),
    case dict:is_key(Time, Schedule) of 
        true -> {ok, New_Final_Loc} = dict:find(Time, Schedule);
        false -> New_Final_Loc = Final_Loc 
    end,
    case Current_Loc /= New_Final_Loc of
        true -> 
            Pid ! {shortest_path, self(), Current_Loc, New_Final_Loc},
            receive 
                {New_Current_Loc, New_Next_Loc, New_Distance} -> {New_Current_Loc, New_Next_Loc, New_Distance}
            end,
            Pid ! {subscribe, New_Current_Loc},
            loop(arriving, Schedule, New_Current_Loc, New_Next_Loc, New_Final_Loc, 0, New_Distance, Pid);
        false -> 
            loop(departing, Schedule, Current_Loc, Next_Loc, Final_Loc, 0, 0, Pid)
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
