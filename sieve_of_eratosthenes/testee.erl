-module('testee').
-export([test/1, rec/1]).


test(No) ->
    Pid = spawn(?MODULE, rec, [No + 1]),
    test_(Pid, No).

test_(_, 0) -> true;
test_(Pid, No) ->
    Pid ! {No},
    test_(Pid, No - 1).

rec(Previous_no) ->
    receive
        {No} ->
            if
                Previous_no =/= (No + 1) ->
                    io:fwrite(io_lib:format("~p", [No]) ++ "\n");
                true ->
                    true
            end,
            rec(No)
    end.

