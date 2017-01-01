-module(asynchronous_temp_conversion).
-export([start/0, conv_temp_async/2, loop/0]).
-import(temp_conversion, [conv_temp/2]).


start() -> register(temp_conv_process, spawn(fun() -> loop() end)).

conv_temp_async(Temp, {FromUnit, ToUnit}) -> rpc({Temp, {FromUnit, ToUnit}}).

rpc(Q) ->
	temp_conv_process ! {self(), Q},
	receive
		{temp_conv_process, Reply} ->
			Reply
	end.

loop() ->
	receive
		{From, {Temp, {FromUnit, ToUnit}}} ->
			Conv_temp = conv_temp(Temp, {FromUnit, ToUnit}),
			From ! {temp_conv_process, Conv_temp},
			loop()
	end.

					 

% start() -> register(enss, spawn(fun() -> loop() end)).
% store(Key, Value) -> rpc([store, Key, Value]).
% lookup(Key) -> rpc({lookup, Key}).
% rpc(Q) ->
	% enss ! {self(), Q},
	% receive
		% [enss, Reply] -> 
			% Reply
	% end.
% 
% loop() ->
	% receive
		% {From, {store, Key, Value}} ->
			% put{Key, {ok, Value}},
			% From ! {enss, true},
			% loop();
		% {From, {lookup, Key}} ->
			% From ! {enss, get(Key}},
			% loop()
	% end.
% 
		% -- make_ref ???
