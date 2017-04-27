-module(tests_1).
-compile(export_all).
%-export([f_1/1, f_2/1, map_parallel/2, map_element_thread/3, delete_odd_numbers/1]).

f_1(List) ->
    F = fun(L) ->
            try lists:foldl(fun(El, Sum) -> El + Sum end, 0, L) of
                Ret -> Ret
            catch
                _:_ -> io:format("Can't sum this!")
            end
        end,
    F(List).

f_2(List) ->
    F = fun(L) ->
            case lists:foldl(
                  fun(El, Sum) ->
                          if
                              Sum == error -> error;
                              true ->
                                  try El + Sum of
                                      Ret -> Ret
                                  catch
                                      E:R -> error
                                  end
                              end
                  end, 0, L) of
                error -> io:format("Cant sum this!");
                Ret -> Ret
            end
        end,
    F(List).

map_parallel(Fun, List) ->
    Pids = map_elements(Fun, List),
    Results = gather_elements(Pids).

gather_elements([]) -> [];
gather_elements([Pid | Rest_pids]) ->
    Result = receive
        {Pid, ok, Res} -> Res;
        {Pid, error} -> error
    end,
    Rest_result =  gather_elements(Rest_pids),
    if
        (Result == error) or (Rest_result == error) ->
            error;
        true -> [Result | Rest_result]
    end.


map_element_thread(Parent_pid, Fun, El) ->
    try Fun(El) of
        Result -> Parent_pid ! {self(), ok, Result}
    catch
        _:_ -> Parent_pid ! {self(), error}
    end.

map_elements(_, []) -> [];
map_elements(Fun, [H | T]) ->
    Pid = spawn(?MODULE, map_element_thread, [self(), Fun, H]),
    Rest_pids = map_elements(Fun, T),
    [Pid | Rest_pids].


delete_odd_numbers([]) -> [];
delete_odd_numbers([H | T]) when is_integer(H) ->
    Rest = delete_odd_numbers(T),
    if
        (H rem 2) == 0 -> [H | Rest];
        true -> Rest
    end;
delete_odd_numbers([H | T]) ->
    [H | delete_odd_numbers(T)].

sum_el(List) ->
    try lists:sum(List) of
        Sum -> Sum
    catch
        _:_ -> error
    end.

sum_el_2([]) -> 0;
sum_el_2([H | T]) when is_number(H) ->
    Sum_of_tail = sum_el_2(T),
    case Sum_of_tail of
        error -> error;
        Sum -> H + Sum
    end;
sum_el_2(_) -> error.


forkBomb() ->
    create_N_threads(10).

manage_thread() ->
    Pids = create_N_threads(10),
    start_infinity_sending(Pids, Pids).

create_N_threads(0) -> [];
create_N_threads(N) ->
    Pid = spawn(?MODULE, manage_thread, []),
    Pids_rest = create_N_threads(N - 1),
    [Pid | Pids_rest].

start_infinity_sending(All_pids, []) -> start_infinity_sending(All_pids, All_pids);
start_infinity_sending(All_pids, [Pid | Pids_rest]) ->
    Pid ! {message},
    start_infinity_sending(All_pids, Pids_rest).


fib_without_recursion(N) ->
    Fi = (1 + math:sqrt(5)) / 2,
    ((math:pow(Fi, N) - math:pow(1 - Fi, N)) / math:sqrt(5)).



skroc2([], L) -> L;
skroc2([H],L) -> L ++ [H];
skroc2([H1|[H2|T]],L) -> skroc2(T, L ++ [(H1+H2)/2]).


short(X) -> short2(X, []).
short2([], Res) -> lists:reverse(Res);
short2([A], Res) -> short2([], [A | Res]);
short2([A,B|T], Res) -> 
      Md = (A+B)/2,
      short2(T, [Md|Res]).


my_lamport_clock(T) ->
    receive
        {msg, Timestamp} -> my_lamport_clock(max(T, Timestamp) + 1)
    end.




filter([], N) -> [];
filter([H|T], N) when H =< N ->
   [H | filter(T, N)];
filter([_ | T], N) ->
    filter(T, N).


reverse(List) ->
    reverse(List, []).

reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.



minL([H|T]) -> minL(T, H).

minL([], Min) -> Min;
minL([H|T], Min) when H < Min -> minL(T, H);
minL([_|T], Min) -> minL(T, Min).
