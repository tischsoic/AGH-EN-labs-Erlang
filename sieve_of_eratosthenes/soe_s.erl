-module('soe_s').
-export([get_primes/1]).
% -include_lib("eunit/include/eunit.hrl").

get_primes(Max_numb) ->
    Numbers = lists:seq(3, Max_numb, 2),
    [2 | sieve(Max_numb, Numbers)].

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
