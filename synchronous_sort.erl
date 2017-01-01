-module(synchronous_sort).
-export([sort/2]).

merge([], L) -> L;
merge(L, []) -> L;
merge([H1 | T1], [H2 | T2]) ->
	if
		H1 < H2 -> [H1 | merge(T1, [H2 | T2])];
		true -> [H2 | merge([H1 | T1], T2)]
	end.


sort(Pid, [E]) -> Pid ! [E];
sort(Pid, [E1, E2]) ->
	if 
		E1 > E2 -> Pid ! [E2, E1];
		true -> Pid ! [E1, E2]
	end;
sort(Pid, V) ->
	V_length = length(V),
	{V_left, V_right} = lists:split(V_length div 2, V),
	spawn(?MODULE, sort, [self(), V_left]),
	spawn(?MODULE, sort, [self(), V_right]),
	V_left_sorted_1 = receive
		V_left_sorted -> V_left_sorted
	end,
	V_right_sorted = receive
		V_right_sorted -> V_right_sorted
	end,
	Pid ! merge(V_left_sorted_1, V_right_sorted).


