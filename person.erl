% Person Module
% by: Peter Lee and Caitlin Klein
% date: December 10, 2015
% description: models a person, each person has a schedule and if they have
%              to go somewhere at the specified time they ask a map server
%              what their next location is 

-module(person).
-export([start/1, loop/7, new_progress/3]).

% spawns a person with a dictionary containing their schedule
start([{_Start, Start_Loc}|Schedule]) -> 
   spawn(person, loop, [departing, dict:from_list(Schedule), Start_Loc, [], 
                        Start_Loc, 0, 0]).

% person travels to destinations based on their schedule
loop(arriving, Schedule, Curr_Loc, Next_Loc, Final_Loc, Progress, Distance) ->
    case  Progress >= Distance of 
        true ->
            map_server ! {unsubscribe, Curr_Loc},
            loop(departing, Schedule, Next_Loc, Next_Loc, Final_Loc, 1, 0);
        false -> 
            get_time(Curr_Loc, Progress, Distance),
            Weight = get_weight(Curr_Loc),
            NewDistance = min(Weight, Distance),
            NewProgress = new_progress(Progress, Distance, NewDistance),
            loop(arriving, Schedule, Curr_Loc, Next_Loc, Final_Loc, 
                 NewProgress + 1, NewDistance)
    end;
loop(departing, Schedule, Curr_Loc, Next_Loc, Final_Loc, Progress, Distance) ->
    Time = get_time(Curr_Loc, Progress, Distance),
    case dict:is_key(Time, Schedule) of 
        true when Curr_Loc == Final_Loc -> {ok, New_Final_Loc} = 
                                           dict:find(Time, Schedule);
        _                               -> New_Final_Loc = Final_Loc 
    end,
    case Curr_Loc /= New_Final_Loc of
        true -> 
            map_server ! {next_edge, self(), Curr_Loc, New_Final_Loc},
            receive 
                {New_Curr_Loc, New_Next_Loc, New_Distance} -> ok 
            end,
            map_server ! {subscribe, New_Curr_Loc},
            loop(arriving, Schedule, New_Curr_Loc, New_Next_Loc, New_Final_Loc,
                 1, New_Distance);
        false -> 
            loop(departing, Schedule, Curr_Loc, Next_Loc, Final_Loc, 1, 0)
    end.

% recives the time from the proxy server and returns current location
% along with progress and distance
get_time(Location, Progress, Distance) -> 
    receive
        {From, Time} -> From ! {self(), Location, Progress, Distance}
    end,
    Time.

% scales progress according to the new distance
% used when the weight / distance of a path has changed
new_progress(Progress, Distance, NewDistance) ->
    round(Progress / (float(Distance) / float(NewDistance))).

% receives the current weight of the path a person is on
get_weight(Location) -> 
    map_server ! {path_weight, self(), Location},
    receive 
        {weight, Weight}-> Weight
    end.

