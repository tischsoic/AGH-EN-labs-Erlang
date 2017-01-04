-module('soe_ets').
-export([get_primes/1]).


get_primes(Max_numb) ->
    ets:new(numbers, [ordered_set, named_table]),
    set_numbers(Max_numb),
    sieve(Max_numb),
    ets:insert(numbers, {2}),
    Primes = ets:match(numbers, '$1'),
    ets:delete(numbers),
    lists:flatten(Primes).

set_numbers(Max_numb) ->
    ets:insert(numbers, {5}),
    set_numbers_2(Max_numb, 5).

set_numbers_2(Max_numb, Base) ->
    Next_numb = Base + 2,
    if
        Next_numb > Max_numb ->
            true;
        true ->
            ets:insert(numbers, {Next_numb}),
            set_numbers_4(Max_numb, Next_numb)
    end.

set_numbers_4(Max_numb, Base) ->
    Next_numb = Base + 4,
    if
        Next_numb > Max_numb ->
            true;
        true ->
            ets:insert(numbers, {Next_numb}),
            set_numbers_2(Max_numb, Next_numb)
    end.

sieve_for_number(Max_numb, Number, Base) ->
    if
        Number =< Max_numb ->
            ets:delete(numbers, Number),
            sieve_for_number(Max_numb, Number + Base, Base);
        true -> true
    end.

sieve(Max_numb) ->
    First = ets:first(numbers),
    sieve_(Max_numb, First).

sieve_(Max_numb, Current) ->
    Current_square = Current * Current,
    if
        Current_square > Max_numb -> true;
        true ->
            sieve_for_number(Max_numb, Current_square, Current),
            case ets:next(numbers, Current) of
                '$end_of_table' -> true;
                Next_number ->
                    sieve_(Max_numb, Next_number)
            end
    end.
