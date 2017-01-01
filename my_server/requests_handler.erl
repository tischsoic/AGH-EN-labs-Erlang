-module(requests_handler).
-export([init/0, handle/2]).

init() -> [].


handle({add_user, Data}, State) ->
    {Username, Password} = Data,
    New_state = [{Username, Password} | State],
    Response = {"user added"},
    {New_state, Response};

handle({remove_user, Data}, State) ->
    {Username, Password} = Data,
    {Response, New_state} = case remove_user(Username, Password, State) of
        no_such_user -> {"No such user", State};
        bad_password -> {"Bad password", State};
        New_list_of_users -> {"User removed", New_list_of_users}
    end,
    {New_state, Response};

handle({get_all_usernames}, State) ->
    All_usernames = get_all_usernames(State),
    New_state = State,
    {New_state, All_usernames};

handle(_, State) ->
    New_state = State,
    Response = "No such operation",
    {New_state, Response}.


get_user_data(username, {Username, _}) ->
    Username;
get_user_data(password, {_, Password}) ->
    Password.

get_all_usernames([]) -> [];
get_all_usernames([User_record | Users_records]) ->
    Username = get_user_data(username, User_record),
    [Username | get_all_usernames(Users_records)].


% handle({get_user_password, Data}, State) ->
    % {Username} = Data,
    % User_password = get_user_password(Username, State),
    % Response = {User_password},
    % New_state = State,
    % {New_state, Response}.


get_user_record(_, []) -> no_such_user;
get_user_record(Username, [User_record | Users_records]) ->
    Record_username = get_user_data(username, User_record),
    if
        Username == Record_username ->
            User_record;
        true -> get_user_data(Username, Users_records)
    end.


remove_user(_, _, []) -> no_such_user;
remove_user(Username, Password, [User_record | Users_records]) ->
    Record_username = get_user_data(username, User_record),
    Record_password = get_user_data(password, User_record),
    if
        Username == Record_username ->
            if
                Password == Record_password -> Users_records;
                true -> bad_password
            end;
        true ->
            User_removal = remove_user(Username, Password, Users_records),
            case User_removal of
                no_such_user -> no_such_user;
                bad_password -> bad_password;
                New_list_of_users -> [User_record | New_list_of_users]
            end
    end.



get_user_password(_, []) -> [];
get_user_password(Username, [H | T]) ->
    {Username_from_list, Password_from_list} = H,
    List_of_passwords = if
        Username == Username_from_list ->
            [Password_from_list | get_user_password(Username, T)];

        true -> get_user_password(Username, T)
    end,
    List_of_passwords.
    % [One_password | _] = List_of_passwords,
    % One_password.
