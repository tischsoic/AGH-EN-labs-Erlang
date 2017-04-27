-module('soe_p_bits').
-export([get_primes/2, sieve_thread_main/3, sieve_thread/2, get_numbers_as_bits/1]).

get_primes(2, _) -> [2];
get_primes(3, _) -> [2, 3];
get_primes(Max_numb, Threads_no) ->
    Primes = sieve(Max_numb, Threads_no),
    [2, 3 | Primes].

set_bit(N, Bin) ->
    <<L:N/bits, _:1, R/bits>> = Bin,
    <<L/bits, 1:1, R/bits>>.

get_bit(N, Bin) ->
    <<_:N/bits, Bit:1, _/bits>> = Bin,
    Bit.

get_numbers_as_bits({From, To}) ->
    Bits_quantity = To - From + 1,
    <<0:Bits_quantity>>.

set_num_in_bits({From, _}, Number, Numbers_as_bits) ->
    set_bit(Number - From, Numbers_as_bits).

get_delete_start_base({From, _}, Step) ->
    Remainder = From rem Step,
    Start_base = if
        Remainder =/= 0 ->
            From - Remainder + Step;
        true ->
            From
    end,
    Step_square = Step * Step,
    if
        Step_square > Start_base -> Step_square;
        true -> Start_base
    end.

delete_multiples(Numbers_as_bits, Numbers_range, Step) ->
    Start_base = get_delete_start_base(Numbers_range, Step),
    delete_multiples_(Numbers_as_bits, Numbers_range, Start_base, Step).

delete_multiples_(Numbers_as_bits, Numbers_range, Base, Step) ->
    {_, To} = Numbers_range,
    if
        Base > To ->
            Numbers_as_bits;
        true ->
            Next_base = Base + Step,
            Numbers_as_bits_without_base = set_num_in_bits(Numbers_range, Base, Numbers_as_bits),
            delete_multiples_(Numbers_as_bits_without_base, Numbers_range, Next_base, Step)
    end.

delete_2_3_multiples(Numbers_range, Numbers_as_bits) ->
    Numbers_as_bits_without_2s = delete_multiples(Numbers_as_bits, Numbers_range, 2),
    delete_multiples(Numbers_as_bits_without_2s, Numbers_range, 3).

get_next_prime(Start_from, {_, To}, _) when Start_from > To -> 
    no_more_primes;
get_next_prime(Start_from, Numbers_range, Numbers_as_bits) ->
    {From, _} = Numbers_range,
    Nth_bit = get_bit(Start_from - From, Numbers_as_bits),
    if
        Nth_bit == 0 ->
            Start_from;
        true ->
            get_next_prime(Start_from + 1, Numbers_range, Numbers_as_bits)
    end.

get_all_primes_from_numbers_as_bits(Current_position, _, {From, To}) when Current_position > (To - From) -> [];
get_all_primes_from_numbers_as_bits(Current_position, Numbers_as_bits, Numbers_range) ->
    Nth_bit = get_bit(Current_position, Numbers_as_bits),
    {From, _} = Numbers_range,
    Next_primes = get_all_primes_from_numbers_as_bits(Current_position + 1, Numbers_as_bits, Numbers_range),
    if
        Nth_bit =/= 0 ->
            Next_primes;
        true ->
            [Current_position + From | Next_primes]
    end.

initialise_thread_data(Numbers_range) ->
    Numbers_as_bits = get_numbers_as_bits(Numbers_range),
    delete_2_3_multiples(Numbers_range, Numbers_as_bits).

sieve_thread_main(Numbers_range, Max_numb, Parent_pid) ->
    Numbers_as_bits = initialise_thread_data(Numbers_range),
    {Prime, _} = Numbers_range,
    Sieved_numbers = sieve_thread_main_(Numbers_as_bits, Numbers_range, Prime, Max_numb, Parent_pid),
    Primes = get_all_primes_from_numbers_as_bits(0, Sieved_numbers, Numbers_range),
    Parent_pid ! {end_of_main},
    Parent_pid ! {primes, self(), Primes}.

sieve_thread_main_(Numbers_as_bits, Numbers_range, Prime, Max_numb, Parent_pid) ->
    if
        Prime == no_more_primes ->
            Numbers_as_bits;
        Prime * Prime > Max_numb ->
            Parent_pid ! {sieving_definite_end},
            Numbers_as_bits;
        true ->
            Parent_pid ! {prime, Prime},
            Sieved_numbers_as_bits = delete_multiples(Numbers_as_bits, Numbers_range, Prime),
            Next_prime = get_next_prime(Prime + 1, Numbers_range, Sieved_numbers_as_bits),
            sieve_thread_main_(Sieved_numbers_as_bits, Numbers_range, Next_prime, Max_numb, Parent_pid)
    end.

sieve_thread(Numbers_range, Parent_pid) ->
    Numbers_as_bits = initialise_thread_data(Numbers_range),
    Sieved_numbers = sieve_thread_(Numbers_as_bits, Numbers_range),
    Primes = get_all_primes_from_numbers_as_bits(0, Sieved_numbers, Numbers_range),
    Parent_pid ! {primes, self(), Primes}.

sieve_thread_(Numbers_as_bits, Numbers_range) ->
    receive
        {end_of_sieving} ->
            Numbers_as_bits;
        {become_main, Max_numb, Parent_pid} ->
            Next_prime = get_next_prime(0, Numbers_range, Numbers_as_bits),
            sieve_thread_main_(Numbers_as_bits, Numbers_range, Next_prime, Max_numb, Parent_pid);
        {prime, Prime} ->
            Sieved_numbers_as_bits = delete_multiples(Numbers_as_bits, Numbers_range, Prime),
            sieve_thread_(Sieved_numbers_as_bits, Numbers_range)
    end.

get_threads_ranges(From, To, Threads_no) ->
    Diff = To - From,
    Step = Diff div Threads_no,
    get_threads_ranges_(From, Step, To, Threads_no).

get_threads_ranges_(From, _, To, 1) ->
    [{From, To}];
get_threads_ranges_(From, Step, To, Threads_no) ->
    To_local = From + Step,
    [{From, To_local} | get_threads_ranges_(To_local + 1, Step, To, Threads_no - 1)].

start_threads([Main_range | Rest_ranges], Max_numb, Threads_no) ->
    Parent_pid = self(),
    Main_pid = start_thread_main(Main_range, Max_numb, Parent_pid),
    Rest_pids = start_threads_rest(Rest_ranges, Parent_pid),
    [Main_pid | Rest_pids].

start_thread_main(Main_range, Max_numb, Parent_pid) ->
    Pid = spawn(?MODULE, sieve_thread_main, [Main_range, Max_numb, Parent_pid]).

start_threads_rest([], _) -> [];
start_threads_rest([Range | Rest_ranges], Parent_pid) ->
    Pid = spawn(?MODULE, sieve_thread, [Range, Parent_pid]),
    [Pid | start_threads_rest(Rest_ranges, Parent_pid)].

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
    end.

sieve(Max_numb, Threads_no) ->
    Threads_max = Max_numb div 10,
    Threads_max_proper = if Threads_max == 0 -> 1; true -> Threads_max end,
    Threads_no_final = if Threads_no < Threads_max_proper -> Threads_no; true -> Threads_max_proper end,
    Threads_number_ranges = get_threads_ranges(5, Max_numb, Threads_no_final),
    [Main_pid | Rest_pids] = start_threads(Threads_number_ranges, Max_numb, Threads_no_final),
    threads_communication([Main_pid | Rest_pids], Max_numb),
    Primes = gather_all_primes([Main_pid | Rest_pids]).
