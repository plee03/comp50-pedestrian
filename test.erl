-module(test).
-export([test/0]).

test() ->
    Pid1 = person:start([], halligan),
    Pid2 = person:start([], pearson),
    List = [{Pid1, halligan}, {Pid2, pearson}],
    proxy:start(List).
