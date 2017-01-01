-module(map_async).
-export([map_async/2, safe_map/2]).

map_async(Fn, List) ->
	S = self(),
	Pids = lists:map(fun(El) -> spawn(fun() -> map_over_one(Fn, El, S) end) end, List),
	get_all(Pids).

safe_map(Fn, El) ->
    catch Fn(El).

map_over_one(Fn, El, Receiver_pid) ->
	S = self(),
    Mapped_value = safe_map(Fn, El),
    Receiver_pid ! {S, Mapped_value}.

get_all([]) -> [];
get_all([Pid | Pids]) ->
	receive
		{Pid, Value} -> [Value | get_all(Pids)]
	end.
