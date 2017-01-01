-module('soe_map').
-export([get_primes/1]).
% -include_lib("eunit/include/eunit.hrl").

get_primes(Max_numb) ->
    Numbers = get_numbers(Max_numb),
    Numbers_balanced = gb_trees:balance(Numbers),
    Sieved_numbers = sieve(Max_numb, Numbers_balanced),
    Sieved_numbers_with_3 = gb_trees:insert(3, 3, Sieved_numbers),
    Primes = gb_trees:insert(2, 2, Sieved_numbers_with_3).

get_numbers(Max_numb) ->
    Numbers = gb_trees:empty(),
    Numbers_with_5 = gb_trees:insert(5, 5, Numbers),
    Numb_23 = get_numbers_2(Numbers_with_5, Max_numb, 5).

get_numbers_2(Numbers, Max_numb, Base) ->
    Next_numb = Base + 2,
    if
        Next_numb > Max_numb ->
            Numbers;
        true ->
            Numbers_updated = gb_trees:insert(Next_numb, Next_numb, Numbers),
            get_numbers_4(Numbers_updated, Max_numb, Next_numb)
    end.

get_numbers_4(Numbers, Max_numb, Base) ->
    Next_numb = Base + 4,
    if
        Next_numb > Max_numb ->
            Numbers;
        true ->
            Numbers_updated = gb_trees:insert(Next_numb, Next_numb, Numbers),
            get_numbers_2(Numbers_updated, Max_numb, Next_numb)
    end.


sieve(_, []) ->
    [];
sieve(Max_numb, [Prime | T]) ->
    if
        Prime * Prime > Max_numb -> [Prime | T];
        true -> Prime_2 = Prime * Prime,
                Sieved_list = [X || X <- T, X rem Prime =/= 0],
                % Sieved_list = [X || X <- T, ((X < Prime_2) or (X rem Prime =/= 0))],
                [Prime | sieve(Max_numb, Sieved_list)]
    end.
