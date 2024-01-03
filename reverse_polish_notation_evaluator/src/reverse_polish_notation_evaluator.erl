-module(reverse_polish_notation_evaluator).
-export([start/0]).


handle_token(Token, Stack, Position) ->
   case utils:try_parse_number(Token) of
      Value when is_number(Value) -> stack:push(Value, Stack);
      _ ->
         Right = stack:peek(Stack),
         StackAfterFirstPop = stack:pop(Stack),
         Left = stack:peek(StackAfterFirstPop),
         StackAfterSecondPop = stack:pop(StackAfterFirstPop),
         case Token of
            "+" -> stack:push(Left + Right, StackAfterSecondPop);
            "-" -> stack:push(Left - Right, StackAfterSecondPop);
            "*" -> stack:push(Left * Right, StackAfterSecondPop);
            "/" -> stack:push(Left / Right, StackAfterSecondPop);
            "//" -> 
               case is_integer(Left) and is_integer(Right) of
                  true -> stack:push(Left div Right, StackAfterSecondPop);
                  false -> throw({error, io_lib:format("Error at position ~w: both operands must be integers.", [Position])})
               end;
            "%" ->
               case is_integer(Left) and is_integer(Right) of
                  true -> stack:push(Left rem Right, StackAfterSecondPop);
                  false -> throw({error, io_lib:format("Error at position ~w: both operands must be integers.", [Position])})
               end;
            "**" -> stack:push(math:pow(Left, Right), StackAfterSecondPop)
         end
   end.


tokenize(Expression) ->
   Tokens = re:split(Expression, "(\\d+\\.?\\d*|[-+*/%//**])"),
   lists:filter(fun(X) -> X =/= "" end, Tokens).
   


%evaluate_rpn(Expression) ->



start() ->
   io:fwrite("~p~n", [tokenize(1 2 3 * + 4 -)]).
