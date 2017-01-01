-module(my_server).
-export([start/1, stop/1, rpc/2, save_server_state/1, set_auto_state_saving/2]).
-import(my_memory_manager, [save_state/1, get_state/0]).

start(Handling_module) ->
    State = case get_state() of
                no_previous_state -> Handling_module:init();
                                     Previous_state -> Previous_state
            end,
    spawn(fun() -> loop(Handling_module, State) end).

stop(Server_pid) ->
    rpc(Server_pid, {stop_server}).

save_server_state(Server_pid) ->
    rpc(Server_pid, {save_state}).

set_auto_state_saving(Server_pid, Time) ->
    timer:send_interval(Time, Server_pid, {save_state}).

rpc(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Response} ->
            Response
    end.

loop(Handling_module, State) ->
    receive
        {save_state} ->
            save_state(State),
            loop(Handling_module, State);

        {From, Ref, {save_state}} ->
            save_state(State),
            From ! {Ref, "Server state saved."},
            loop(Handling_module, State);

        {From, Ref, {stop_server}} ->
            save_state(State),
            From ! {Ref, "Server stopped."};

        {From, Ref, {update_handling_module, New_handling_module}} ->
            From ! {Ref, "Module changed."},
            loop(New_handling_module, State);

        {From, Ref, Request} ->
            {New_state, Response} = Handling_module:handle(Request, State),
            From ! {Ref, Response},
            loop(Handling_module, New_state)
        end.

