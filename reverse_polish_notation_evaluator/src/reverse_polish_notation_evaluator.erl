-module(reverse_polish_notation_evaluator).
-export([start/0]).


% evaluate_rpn(expression) ->


start() ->
   io:fwrite("Hello, world!~n"),
   Stack = stack:new(),
   stack:push(3, stack:push(2, stack:push(1, Stack))),
   io:fwrite("~p~n", stack:peek(Stack)),
   io:fwrite("~p~n", stack:pop(Stack)),
   io:fwrite("~p~n", stack:peek(Stack)),
   io:fwrite("~p~n", stack:pop(Stack)),
   io:fwrite("~p~n", stack:peek(Stack)),
   io:fwrite("~p~n", stack:pop(Stack)),
   io:fwrite("~p~n", stack:peek(Stack)),
   io:fwrite("~p~n", stack:is_empty(Stack)).
