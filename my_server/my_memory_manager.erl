-module(my_memory_manager).
-export([save_state/1, get_state/0]).

get_state_data_file_name() ->
    "state_data.txt".

save_state(State) ->
    State_data_file_name = get_state_data_file_name(),
    {ok, Device} = file:open(State_data_file_name, [write]),
    State_as_string = state_to_string(State),
    file:write(Device, State_as_string),
    file:close(Device).

get_state() ->
    State_data_file_name = get_state_data_file_name(),
    case filelib:is_regular(State_data_file_name) of
        false -> no_previous_state;
        true -> read_state_from_file()
    end.

read_state_from_file() ->
    State_data_file_name = get_state_data_file_name(),
    {ok, Device} = file:open(State_data_file_name, [read]),
    State_as_string = read_whole_file(Device),
    file:close(Device),
    State = state_from_string(State_as_string),
    State.

read_whole_file(Device) ->
    case io:get_line(Device, "") of
        eof -> [];
        Line -> Line ++ read_whole_file(Device)
    end.

state_from_string(State_as_string) ->
    Records_as_strings = string:tokens(State_as_string, "\n"),
    Records = records_from_strings(Records_as_strings),
    Records.

records_from_strings([]) -> [];
% records_from_strings([""]) -> [];
records_from_strings([Record_as_string | T]) ->
    [record_from_string(Record_as_string) | records_from_strings(T)].

record_from_string(Record_as_string) ->
    [Username, Password] = string:tokens(Record_as_string, "\t"),
    {Username, Password}.

state_to_string([]) -> "";
state_to_string([{Username, Password} | T]) ->
    One_record_as_string = Username ++ "\t" ++ Password ++ "\r\n", % \r\n because of Windows file system or encoding or something else
    All_records_as_string = One_record_as_string 
        ++ state_to_string(T),
    All_records_as_string.

