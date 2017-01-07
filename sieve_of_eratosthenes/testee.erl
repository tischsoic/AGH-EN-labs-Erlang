-module('testee').
-export([test/1, rec/1, set_bit_v1/2, set_bit_v2/2, set_bit_v3/2]).


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

set_bit_v1(N, B) ->
    fun (<<L:N/bits, _:1, R/bits>>) ->
        <<L/bits, 1:1, R/bits>>
    end(B).


set_bit_v2(N, Bin) ->
     a(list_to_binary([<<N:8>>, Bin])).

a(<<N:8, L:N/bits, _:1, R/bits >>)  ->
    << L/bits, 1:1, R/bits >>.

set_bit_v3(N, Bin) ->
    <<L:N/bits, _:1, R/bits>> = Bin,
    <<L/bits, 1:1, R/bits>>.
