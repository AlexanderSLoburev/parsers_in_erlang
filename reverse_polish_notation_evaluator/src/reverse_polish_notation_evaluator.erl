-module(reverse_polish_notation_evaluator).
-export([start/0]).


handle_token({Token, Position}, Stack) ->
   io:fwrite("Stack: ~p~n", [Stack]),
   case utils:try_parse_number(Token) of
      Value when is_number(Value) ->
         io:fwrite("Pushing ~p into stack~n", [Value]),
         stack:push(Value, Stack);
      _ ->
         Right = try
             stack:peek(Stack)
         catch
            _ -> throw({error, lists:flatten(io_lib:format("Syntax error in position ~w: ~p is wrong token.~n", [Position, Token]))})
         end,
            io:fwrite("Poping ~p from stack~n", [Right]),
            StackAfterFirstPop = stack:pop(Stack),
         Left = try
            stack:peek(StackAfterFirstPop)
         catch
            _ -> throw({error, lists:flatten(io_lib:format("Syntax error in position ~w: ~p is wrong token.~n", [Position, Token]))})
         end,
         io:fwrite("Poping ~p from stack~n", [Left]),
         StackAfterSecondPop = stack:pop(StackAfterFirstPop),
         case Token of
            "+" ->
               Result = Left + Right,
               io:fwrite("~p + ~p = ~p, pushing ~p into stack~n", [Left, Right, Result, Result]),
               stack:push(Result, StackAfterSecondPop);
            "-" ->
               Result = Left - Right,
               io:fwrite("~p - ~p = ~p, pushing ~p into stack~n", [Left, Right, Result, Result]),
               stack:push(Result, StackAfterSecondPop);
            "*" ->
               Result = Left * Right,
               io:fwrite("~p * ~p = ~p, pushing ~p into stack~n", [Left, Right, Result, Result]),
               stack:push(Result, StackAfterSecondPop);
            "/" ->
               Result = Left / Right,
               io:fwrite("~p / ~p = ~p, pushing ~p into stack~n", [Left, Right, Result, Result]),
               stack:push(Result, StackAfterSecondPop);
            "//" ->
               case is_integer(Left) and is_integer(Right) of
                  true ->
                     Result = Left div Right,
                     io:fwrite("~p // ~p = ~p, pushing ~p into stack~n", [Left, Right, Result, Result]),
                     stack:push(Result, StackAfterSecondPop);
                  false -> throw({error, lists:flatten(io_lib:format("Error at position ~w: both operands must be integers.", [Position]))})
               end;
            "%" ->
               case is_integer(Left) and is_integer(Right) of
                  true ->
                     Result = Left rem Right,
                     io:fwrite("~p % ~p = ~p, pushing ~p into stack~n", [Left, Right, Result, Result]),
                     stack:push(Result, StackAfterSecondPop);
                  false -> throw({error, lists:flatten(io_lib:format("Error at position ~w: both operands must be integers.", [Position]))})
               end;
            "**" ->
               Result = math:pow(Left, Right),
               io:fwrite("~p ** ~p = ~p, pushing ~p into stack~n", [Left, Right, Result, Result]),
               stack:push(Result, StackAfterSecondPop)
         end
   end.


'_evaluate_rpn'([], Stack) -> stack:peek(Stack);

'_evaluate_rpn'(Tokens, Stack) ->
   [FirstToken | RestTokens] = Tokens,
   '_evaluate_rpn'(RestTokens, handle_token(FirstToken, Stack)).


evaluate_rpn(Expression) -> 
   Tokens = tokenizer:tokenize(Expression),
   % io:fwrite("Tokens: ~p~n", [Tokens]),
   '_evaluate_rpn'(Tokens, []).


start() ->
   %io:fwrite("~p~n", [evaluate_rpn("1 2 3 * + 4 -")]),
   %io:fwrite("~p~n", [evaluate_rpn(" 8 2 5 * + 1 3 2 * + 4 - / ")]),
   io:fwrite("~p~n", [evaluate_rpn("1 3 4 + * 2 // 5 1 - % 2 ** 3 +")]).
