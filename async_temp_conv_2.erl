-module(async_temp_conv_2).
-export([conv_temp_async/2, fun_wrapper/3]).
-import(temp_conversion, [conv_temp/2]).


conv_temp_async(Temp, {FromUnit, ToUnit}) ->
	async_fun_call(fun temp_conversion:conv_temp/2, [Temp, {FromUnit, ToUnit}]).

fun_wrapper(Fun, Args, Pid) ->
	Result = apply(Fun, Args),
	Pid ! {self(), Result}.

async_fun_call(Fun, Args) ->
	ChildPid = spawn(?MODULE, fun_wrapper, [Fun, Args, self()]),
	receive
		{ChildPid, Result} ->
			Result
	end.


% conv_temp_adapter({Temp, {FromUnit, ToUnit}}) -> conv_temp(Temp, {FromUnit, ToUnit}).

