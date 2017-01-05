-module('soe_p_tree').
-export([get_primes/2, sieve_thread_main/3, sieve_thread/2, set_numbers/3]).

get_primes(2, _) -> [2];
get_primes(3, _) -> [2, 3];
get_primes(Max_numb, Threads_no) ->
    Primes = sieve(Max_numb, Threads_no),
    [2, 3 | Primes].


get_base_number(Reference_number) ->
    if
        (Reference_number rem 4) == 0 ->
            Reference_number + 3;
        (Reference_number rem 2) == 0 ->
            Reference_number + 1;
        true ->
            Reference_number
    end.

set_numbers(From, To, Tree) ->
    Base = get_base_number(From),
    set_numbers_2(To, Base, Tree).

set_numbers_2(Max_numb, Base, Tree) ->
    Next_numb = Base + 2,
    if
        Base > Max_numb ->
            Tree;
        true ->
            Tree_with_next = gb_trees:insert(Base, Base, Tree),
            set_numbers_4(Max_numb, Next_numb, Tree_with_next)
    end.

set_numbers_4(Max_numb, Base, Tree) ->
    Next_numb = Base + 4,
    if
        Base > Max_numb ->
            Tree;
        true ->
            Tree_with_next = gb_trees:insert(Base, Base, Tree),
            set_numbers_2(Max_numb, Next_numb, Tree_with_next)
    end.

%set_numbers(Max_numb) ->
    %[5 | set_numbers_24(Max_numb, 5)].
%
%set_numbers_24(Max_numb, Base) ->
    %set_numbers_24_(Max_numb, Base, [2, 4]).
%
%set_numbers_24_(Max_numb, Base, Wheel) ->
    %[Incrementer_1, Incrementer_2] = Wheel,
    %Next_numb = Base + Incrementer_1,
    %if
        %Next_numb > Max_numb ->
            %[];
        %true ->
            %[Next_numb | set_numbers_24_(Max_numb, Next_numb, [Incrementer_2, Incrementer_1])]
    %end.

initialise_thread_tree({From, To}) ->
    Numbers_tree = gb_trees:empty(),
    set_numbers(From, To, Numbers_tree).

sieve_thread_main(Numbers_range, Max_numb, Parent_pid) ->
    %io:fwrite("tutaj 2"),
    Numbers_tree = initialise_thread_tree(Numbers_range),
    %io:fwrite("sieve_thread_main: tree " ++ io_lib:format("~p", [Numbers_range]) ++ "\n"),
    {Local_smallest, _} = gb_trees:smallest(Numbers_tree),
    {Local_largest, _} = gb_trees:largest(Numbers_tree),
    Sieved_tree = sieve_thread_main_(Numbers_tree, Local_smallest, Local_largest, Max_numb, Parent_pid),
    Primes = gb_trees:keys(Sieved_tree),
    %io:fwrite("tutaj 1"),
    Parent_pid ! {end_of_main},
    Parent_pid ! {primes, self(), Primes}.

get_next_prime(Numbers_tree, Previous_prime) ->
    Iterator_from = gb_trees:iterator_from(Previous_prime, Numbers_tree),
    Current = gb_trees:next(Iterator_from),
    {P1, _, Iterator_from_next_prime} = Current,
    Next = gb_trees:next(Iterator_from_next_prime),
    %{P2, _, _} = Next,
    %io:fwrite("get_next_prime: prime" ++ io_lib:format("~p", [P1]) ++ "\n"),
    %{12, 12} * as,
    case Next of
        none -> none;
        {Prime, _, _} -> Prime
    end.

delete_multiples(Numbers_tree, Base, Step, Max) ->
    Numbers_tree_sieved = delete_multiples_(Numbers_tree, Base, Step, Max),
    gb_trees:balance(Numbers_tree_sieved).

delete_multiples_(Numbers_tree, Base, Step, Max) ->
    if
        Base > Max ->
            Numbers_tree;
        true ->
            Next_base = Base + Step,
            Numbers_tree_without_base = gb_trees:delete_any(Base, Numbers_tree),
            delete_multiples_(Numbers_tree_without_base, Next_base, Step, Max)
    end.

sieve_thread_main_(Numbers_tree, Prime, Initial_local_largest, Max_numb, Parent_pid) ->
    % blad za pierwszym razem przekazujemy prime, a nie previous prime!!!!!!!!!!!!!!!
    if
        Prime == none ->
            Numbers_tree;
        Prime * Prime > Max_numb ->
            Parent_pid ! {sieving_definite_end},
            Numbers_tree;
        true ->
            %io:fwrite("m_t: prime" ++ io_lib:format("~p", [Prime]) ++ "\n"),
            Parent_pid ! {prime, Prime},
            Sieved_numbers_tree = delete_multiples(Numbers_tree, Prime * Prime, Prime, Initial_local_largest),
            Next_prime = get_next_prime(Sieved_numbers_tree, Prime),
            sieve_thread_main_(Sieved_numbers_tree, Next_prime, Initial_local_largest, Max_numb, Parent_pid)
    end.

sieve_thread(Numbers_range, Parent_pid) ->
    Numbers_tree = initialise_thread_tree(Numbers_range),
    {Local_largest, _} = gb_trees:largest(Numbers_tree),
    Sieved_tree = sieve_thread_(Numbers_tree, Local_largest),
    Primes = gb_trees:keys(Sieved_tree),
    Parent_pid ! {primes, self(), Primes}.

sieve_thread_(Numbers_tree, Initial_local_largest) ->
    {Local_smallest, _} = gb_trees:smallest(Numbers_tree),
    receive
        {end_of_sieving} ->
            %io:fwrite("s_t: end_of_sieving"),
            Numbers_tree;
        {become_main, Max_numb, Parent_pid} ->
            sieve_thread_main_(Numbers_tree, Local_smallest, Initial_local_largest, Max_numb, Parent_pid);
        {prime, Prime} ->
            %io:fwrite("s_t: prime" ++ io_lib:format("~p", [Prime]) ++ "\n"),
            Prime_square = Prime * Prime,
            Deletion_base = if
                                Local_smallest > Prime_square -> Local_smallest - (Local_smallest rem Prime);
                                true -> Prime_square
                            end,
            Sieved_numbers_tree = delete_multiples(Numbers_tree, Deletion_base, Prime, Initial_local_largest),
            sieve_thread_(Sieved_numbers_tree, Initial_local_largest)
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

sieve(Max_numb, Threads_no) ->
    Threads_max = Max_numb div 10,
    Threads_max_proper = if Threads_max == 0 -> 1; true -> Threads_max end,
    Threads_no_final = if Threads_no < Threads_max_proper -> Threads_no; true -> Threads_max_proper end,
    Threads_number_ranges = get_threads_ranges(5, Max_numb, Threads_no_final),
    [Main_pid | Rest_pids] = start_threads(Threads_number_ranges, Max_numb, Threads_no_final),
    threads_communication([Main_pid | Rest_pids], Max_numb),
    Primes = gather_all_primes([Main_pid | Rest_pids]).
    %case lists:flatten(Numbers_rest) of
        %[] -> Primes;
        %Numbers_to_sieve ->
            %io:fwrite(io_lib:format("~p", [Numbers_to_sieve]) ++ "\n"),
            %Primes_rest = sieve(Max_numb, Numbers_to_sieve, Threads_no_final),
            %All_primes = lists:merge(Primes, Primes_rest)
    %end.
