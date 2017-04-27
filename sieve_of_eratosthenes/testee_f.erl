-module(simple).
-compile(export_all).

foo(A) ->
   if
      length(A) == 0 ->
           io:format("zero");
      true ->
           null
   end;
foo([H | T]) -> io:format("Hello second").
