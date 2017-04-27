-module('soe_p').
-export([get_primes/2, sieve_thread_main/3, sieve_thread/2, set_numbers/1]).

-type list_of_numbers() :: list(integer()).

-spec get_primes(Max_numb::integer(), Threads_no::integer()) -> list_of_numbers().
get_primes(2, _) -> [2];
get_primes(3, _) -> [2, 3];
get_primes(Max_numb, Threads_no) ->
    Numbers = set_numbers(Max_numb),
    Primes = sieve(Max_numb, Numbers, Threads_no),
    Primes_with_2 = [2, 3 | Primes],
    Primes_with_2.

-spec set_numbers(Max_numb::Integer) -> list(Integer).
set_numbers(Max_numb) ->
    [5 | set_numbers_2(Max_numb, 5)].

-spec set_numbers_2(Max_numb::Integer, Base::Integer) -> list(Integer).
set_numbers_2(Max_numb, Base) ->
    Next_numb = Base + 2,
    if
        Next_numb > Max_numb ->
            [];
        true ->
            [Next_numb | set_numbers_4(Max_numb, Next_numb)]
    end.

-spec set_numbers_4(Max_numb::Integer, Base::Integer) -> list(Integer).
set_numbers_4(Max_numb, Base) ->
    Next_numb = Base + 4,
    if
        Next_numb > Max_numb ->
            [];
        true ->
            [Next_numb | set_numbers_2(Max_numb, Next_numb)]
    end.

-spec sieve_thread_main(Numbers::list_of_numbers(), Max_numb::integer(), Parent_pid::pid()) -> true.
sieve_thread_main(Numbers, Max_numb, Parent_pid) ->
    Primes = sieve_thread_main_(Numbers, Max_numb, Parent_pid),
    Parent_pid ! {end_of_main},
    Parent_pid ! {primes, self(), Primes},
    true.

-spec sieve_thread_main_(list_of_numbers(), Max_numb::integer(), Parent_pid::pid()) -> list_of_numbers().
sieve_thread_main_([], _, _) -> [];
sieve_thread_main_([Prime | T], Max_numb, Parent_pid) ->
    if
        Prime * Prime > Max_numb ->
            Parent_pid ! {sieving_definite_end},
            [Prime | T];
        true ->
            Parent_pid ! {prime, Prime},
            Sieved_list = [X || X <- T, X rem Prime =/= 0],
            [Prime | sieve_thread_main_(Sieved_list, Max_numb, Parent_pid)]
    end.

-spec sieve_thread(Numbers::list_of_numbers(), Parent_pid::pid()) -> true.
sieve_thread(Numbers, Parent_pid) ->
    Sieved_list = sieve_thread_(Numbers),
    Parent_pid ! {primes, self(), Sieved_list},
    true.

-spec sieve_thread_(Numbers::list_of_numbers()) -> list_of_numbers().
sieve_thread_(Numbers) ->
    receive
        {end_of_sieving} ->
            Numbers;
        {become_main, Max_numb, Parent_pid} ->
            sieve_thread_main_(Numbers, Max_numb, Parent_pid);
        {prime, Prime} ->
            Sieved_list = [X || X <- Numbers, X rem Prime =/= 0],
            sieve_thread_(Sieved_list)
    end.

-spec split_into(List::list_of_numbers(), Sublists_no::integer()) -> list(list_of_numbers()).
split_into(List, Sublists_no) ->
    Sublist_length = length(List),
    Chunks_length = Sublist_length div Sublists_no,
    Chunks = split_into_(List, Chunks_length + 1, Sublists_no),
    Chunks.

-spec split_into_(List::list_of_numbers(), Chunks_length::integer(), Sublists_no::integer()) -> list(list_of_numbers()).
split_into_(List, _, 1) -> [List];
split_into_(List, Chunks_length, Sublists_no) ->
    {Chunk, Rest} = lists:split(Chunks_length, List),
    [Chunk | split_into_(Rest, Chunks_length, Sublists_no - 1)].

-spec start_threads(list(list_of_numbers()), Max_numb::integer(), Threads_no::integer()) -> list(pid()).
start_threads([Main_chunk | Rest_chunks], Max_numb, Threads_no) ->
    Parent_pid = self(),
    Main_pid = start_thread_main(Main_chunk, Max_numb, Parent_pid),
    Rest_pids = start_threads_rest(Rest_chunks, Parent_pid),
    [Main_pid | Rest_pids].

-spec start_thread_main(list_of_numbers(), Max_numb::integer(), Parent_pid::pid()) -> pid().
start_thread_main(Main_chunk, Max_numb, Parent_pid) ->
    Pid = spawn(?MODULE, sieve_thread_main, [Main_chunk, Max_numb, Parent_pid]),
    Pid.

-spec start_threads_rest(list(list_of_numbers()), Parent_pid::pid()) -> list(pid()).
start_threads_rest([], _) -> [];
start_threads_rest([Chunk | Rest_chunks], Parent_pid) ->
    Pid = spawn(?MODULE, sieve_thread, [Chunk, Parent_pid]),
    [Pid | start_threads_rest(Rest_chunks, Parent_pid)].

-spec send_message_to_threads(list(pid()), {atom()} | {prime, integer()} | {become_main, integer(), pid()}) -> true.
send_message_to_threads([], _) -> true;
send_message_to_threads([Pid | Pids_rest], Message) ->
    Pid ! Message,
    send_message_to_threads(Pids_rest, Message).

-spec gather_all_primes(list(pid())) -> list_of_numbers() | end_of_list.
gather_all_primes([]) -> end_of_list;
gather_all_primes([Pid | Pids_rest]) ->
    receive
        {primes, Pid, Primes} ->
            case gather_all_primes(Pids_rest) of
                end_of_list -> Primes;
                Primes_from_rest ->
                    Primes ++ Primes_from_rest
            end
    end.

-spec threads_communication(list(pid()), integer()) -> ok.
threads_communication([Main_pid | Rest_pids], Max_numb) ->
    receive
        {end_of_main} ->
            case Rest_pids of
                [] -> true;
                [Next_main_pid | _] ->
                    send_message_to_threads([Next_main_pid], {become_main, Max_numb, self()}),
                    threads_communication(Rest_pids, Max_numb)
            end;
        {sieving_definite_end} ->
            send_message_to_threads(Rest_pids, {end_of_sieving}),
            receive
                {end_of_main} -> true
            end;
        {prime, Prime} ->
            send_message_to_threads(Rest_pids, {prime, Prime}),
            threads_communication([Main_pid | Rest_pids], Max_numb)
    end,
    ok.

-spec sieve(integer(), list_of_numbers(), integer()) -> list_of_numbers().
sieve(Max_numb, Numbers, Threads_no) ->
    Numbers_length = length(Numbers),
    Threads_max = Numbers_length div 5,
    Threads_max_proper = if Threads_max == 0 -> 1; true -> Threads_max end,
    Threads_no_final = if Threads_no < Threads_max_proper -> Threads_no; true -> Threads_max_proper end,
    Numbers_chunks = split_into(Numbers, Threads_no_final),
    [Main_pid | Rest_pids] = start_threads(Numbers_chunks, Max_numb, Threads_no_final),
    threads_communication([Main_pid | Rest_pids], Max_numb),
    Primes = gather_all_primes([Main_pid | Rest_pids]),
    Primes.
