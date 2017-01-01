-module('soe_p').
-export([get_primes/2, sieve_thread_main/3, sieve_thread/2]).

get_primes(2, _) -> [2];
get_primes(3, _) -> [2, 3];
get_primes(Max_numb, Threads_no) ->
    %c:flush(),
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

sieve_thread_main(Numbers, Max_numb, Parent_pid) ->
    %io:fwrite("tutaj 2"),
    Primes = sieve_thread_main_(Numbers, Max_numb, Parent_pid),
    %io:fwrite("tutaj 1"),
    Parent_pid ! {end_of_main},
    Parent_pid ! {primes, self(), Primes}.

sieve_thread_main_([], _, _) -> [];
sieve_thread_main_([Prime | T], Max_numb, Parent_pid) ->
    if
        Prime * Prime > Max_numb ->
            Parent_pid ! {sieving_definite_end},
            [Prime | T];
        true ->
            %io:fwrite("m_t: prime" ++ io_lib:format("~p", [Prime]) ++ "\n"),
            Parent_pid ! {prime, Prime},
            Sieved_list = [X || X <- T, X rem Prime =/= 0],
            [Prime | sieve_thread_main_(Sieved_list, Max_numb, Parent_pid)]
    end.

sieve_thread(Numbers, Parent_pid) ->
    Sieved_list = sieve_thread_(Numbers),
    Parent_pid ! {primes, self(), Sieved_list}.

sieve_thread_(Numbers) ->
    receive
        {end_of_sieving} ->
            %io:fwrite("s_t: end_of_sieving"),
            Numbers;
        {become_main, Max_numb, Parent_pid} ->
            sieve_thread_main_(Numbers, Max_numb, Parent_pid);
        {prime, Prime} ->
            %io:fwrite("s_t: prime" ++ io_lib:format("~p", [Prime]) ++ "\n"),
            Sieved_list = [X || X <- Numbers, X rem Prime =/= 0],
            sieve_thread_(Sieved_list)
    end.

split_into(List, Sublists_no) ->
    Sublist_length = length(List),
    %io:fwrite(io_lib:format("~p", [Sublist_length])),
    %io:fwrite(io_lib:format("~p", [Sublists_no])),
    Chunks_length = Sublist_length div Sublists_no,
    Chunks = split_into_(List, Chunks_length + 1, Sublists_no).

split_into_(List, _, 1) -> [List];
split_into_(List, Chunks_length, Sublists_no) ->
    {Chunk, Rest} = lists:split(Chunks_length, List),
    [Chunk | split_into_(Rest, Chunks_length, Sublists_no - 1)].

start_threads([Main_chunk | Rest_chunks], Max_numb, Threads_no) ->
    Parent_pid = self(),
    Main_pid = start_thread_main(Main_chunk, Max_numb, Parent_pid),
    Rest_pids = start_threads_rest(Rest_chunks, Parent_pid),
    [Main_pid | Rest_pids].

start_thread_main(Main_chunk, Max_numb, Parent_pid) ->
    Pid = spawn(?MODULE, sieve_thread_main, [Main_chunk, Max_numb, Parent_pid]).

start_threads_rest([], _) -> [];
start_threads_rest([Chunk | Rest_chunks], Parent_pid) ->
    Pid = spawn(?MODULE, sieve_thread, [Chunk, Parent_pid]),
    [Pid | start_threads_rest(Rest_chunks, Parent_pid)].

send_message_to_threads([], _) -> true;
send_message_to_threads([Pid | Pids_rest], Message) ->
    Pid ! Message,
    send_message_to_threads(Pids_rest, Message).

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

threads_communication([Main_pid | Rest_pids], Max_numb) ->
    receive
        {end_of_main} ->
            %io:fwrite("threads_communication: end_of_main\n"),
            case Rest_pids of
                [] -> true;
                [Next_main_pid | _] ->
                    send_message_to_threads([Next_main_pid], {become_main, Max_numb, self()}),
                    threads_communication(Rest_pids, Max_numb)
            end;
        {sieving_definite_end} ->
            %io:fwrite("threads_communication: sieving_definite_end\n"),
            send_message_to_threads(Rest_pids, {end_of_sieving}),
            receive
                {end_of_main} -> true
            end;
        {prime, Prime} ->
            send_message_to_threads(Rest_pids, {prime, Prime}),
            threads_communication([Main_pid | Rest_pids], Max_numb)
    end.

sieve(Max_numb, Numbers, Threads_no) ->
    Numbers_length = length(Numbers),
    Threads_max = Numbers_length div 5,
    Threads_max_proper = if Threads_max == 0 -> 1; true -> Threads_max end,
    Threads_no_final = if Threads_no < Threads_max_proper -> Threads_no; true -> Threads_max_proper end,
    Numbers_chunks = split_into(Numbers, Threads_no_final),
    [Main_pid | Rest_pids] = start_threads(Numbers_chunks, Max_numb, Threads_no_final),
    threads_communication([Main_pid | Rest_pids], Max_numb),
    Primes = gather_all_primes([Main_pid | Rest_pids]).
    %case lists:flatten(Numbers_rest) of
        %[] -> Primes;
        %Numbers_to_sieve ->
            %io:fwrite(io_lib:format("~p", [Numbers_to_sieve]) ++ "\n"),
            %Primes_rest = sieve(Max_numb, Numbers_to_sieve, Threads_no_final),
            %All_primes = lists:merge(Primes, Primes_rest)
    %end.
