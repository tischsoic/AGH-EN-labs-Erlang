-module('soe_p').
-export([get_primes/2, sieve_thread_main/2, sieve_thread_rest/2]).

- spec get_primes(Max_numb::Integer, Threads_no::Integer) -> list(Integer).
get_primes(2, _) -> [2];
get_primes(3, _) -> [2, 3];
get_primes(Max_numb, Threads_no) ->
    Numbers = set_numbers(Max_numb),
    Primes = sieve(Max_numb, Numbers, Threads_no),
    Primes_with_2 = [2, 3 | Primes].

set_numbers(Max_numb) ->
    [5 | set_numbers_2(Max_numb, 5)].

set_numbers_2(Max_numb, Base) ->
    Next_numb = Base + 2,
    if
        Next_numb > Max_numb ->
            [];
        true ->
            [Next_numb | set_numbers_4(Max_numb, Next_numb)]
    end.

set_numbers_4(Max_numb, Base) ->
    Next_numb = Base + 4,
    if
        Next_numb > Max_numb ->
            [];
        true ->
            [Next_numb | set_numbers_2(Max_numb, Next_numb)]
    end.

sieve_thread_main(Numbers, Parent_pid) ->
    %io:fwrite("tutaj 2"),
    Primes = sieve_thread_main_(Numbers, Parent_pid),
    %io:fwrite("tutaj 1"),
    Parent_pid ! {end_of_main, Primes}.

-spec sieve_thread_main_(list(Integer), pid()) -> list(Integer).
sieve_thread_main_([], _) -> [];
sieve_thread_main_([Prime | T], Parent_pid) ->
    Parent_pid ! {prime, Prime},
    Sieved_list = [X || X <- T, X rem Prime =/= 0],
    [Prime | sieve_thread_main_(Sieved_list, Parent_pid)].

sieve_thread_rest(Numbers, Parent_pid) ->
    Sieved_list = sieve_thread_rest_(Numbers),
    Parent_pid ! {self(), Sieved_list}.

-spec sieve_thread_rest_(list(Integer)) -> list(Integer).
sieve_thread_rest_(Numbers) ->
    receive
        {end_of_sieving} ->
            Numbers;
        {prime, Prime} ->
            Sieved_list = Sieved_list = [X || X <- Numbers, X rem Prime =/= 0],
            sieve_thread_rest_(Sieved_list)
    end.

-spec split_into(list(Integer), Integer) -> list(list(Integer)).
split_into(List, Sublists_no) ->
    Sublist_length = length(List),
    %io:fwrite(io_lib:format("~p", [Sublist_length])),
    %io:fwrite(io_lib:format("~p", [Sublists_no])),
    Chunks_length = Sublist_length div Sublists_no,
    Chunks = split_into_(List, Chunks_length + 1, Sublists_no).

-spec split_into_(list(Integer), Integer, Integer) -> list(list(Integer)).
split_into_(List, _, 1) -> [List];
split_into_(List, Chunks_length, Sublists_no) ->
    {Chunk, Rest} = lists:split(Chunks_length, List),
    [Chunk | split_into_(Rest, Chunks_length, Sublists_no - 1)].

start_threads([Main_chunk | Rest_chunks], Threads_no) ->
    Parent_pid = self(),
    Main_pid = start_thread_main(Main_chunk, Parent_pid),
    Rest_pids = start_threads_rest(Rest_chunks, Parent_pid),
    [Main_pid | Rest_pids].

start_thread_main(Main_chunk, Parent_pid) ->
    Pid = spawn(?MODULE, sieve_thread_main, [Main_chunk, Parent_pid]).

start_threads_rest([], _) -> [];
start_threads_rest([Chunk | Rest_chunks], Parent_pid) ->
    Pid = spawn(?MODULE, sieve_thread_rest, [Chunk, Parent_pid]),
    [Pid | start_threads_rest(Rest_chunks, Parent_pid)].

send_message_to_threads([], _) -> true;
send_message_to_threads([Pid | Pids_rest], Message) ->
    Pid ! Message,
    send_message_to_threads(Pids_rest, Message).

gather_all_numbers([]) -> end_of_list;
gather_all_numbers([Pid | Pids_rest]) ->
    receive
        {Pid, Numbers} ->
            case gather_all_numbers(Pids_rest) of
                end_of_list -> [Numbers];
                Numbers_from_rest ->
                    [Numbers | Numbers_from_rest]
            end
    end.

threads_communication([Main_pid | Rest_pids]) ->
    receive
        {end_of_main, Primes} ->
            send_message_to_threads(Rest_pids, {end_of_sieving}),
            case gather_all_numbers(Rest_pids) of
                end_of_list -> [Primes];
                All_numbers ->
                    [Primes | All_numbers]
            end;
        {prime, Prime} ->
            send_message_to_threads(Rest_pids, {prime, Prime}),
            threads_communication([Main_pid | Rest_pids])
    end.

sieve(Max_numb, Numbers, Threads_no) ->
    Numbers_length = length(Numbers),
    Threads_max = Numbers_length div 1000,
    Threads_max_proper = if Threads_max == 0 -> 1; true -> Threads_max end,
    Threads_no_final = if Threads_no < Threads_max_proper -> Threads_no; true -> Threads_max_proper end,
    Numbers_chunks = split_into(Numbers, Threads_no_final),
    [Main_pid | Rest_pids] = start_threads(Numbers_chunks, Threads_no_final),
    [Primes | Numbers_rest] = threads_communication([Main_pid | Rest_pids]),
    case lists:flatten(Numbers_rest) of
        [] -> Primes;
        Numbers_to_sieve ->
            %io:fwrite(io_lib:format("~p", [Numbers_to_sieve]) ++ "\n"),
            Primes_rest = sieve(Max_numb, Numbers_to_sieve, Threads_no_final),
            All_primes = lists:merge(Primes, Primes_rest)
    end.
