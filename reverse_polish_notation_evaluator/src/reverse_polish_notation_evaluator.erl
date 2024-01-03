-module(reverse_polish_notation_evaluator).
-export([start/0]).


handle_token({Token, Position}, Stack) ->
   io:fwrite("Token ~p in position ~w~n", [Token, Position]),
   io:fwrite("Stack: ~p~n", [Stack]),
   case utils:try_parse_number(Token) of
      Value when is_number(Value) ->
         io:fwrite("Pushing ~p into stack~n", [Value]),
         stack:push(Value, Stack);
      _ ->
         Right = stack:peek(Stack),
         io:fwrite("Poping ~p from stack~n", [Right]),
         StackAfterFirstPop = stack:pop(Stack),
         Left = stack:peek(StackAfterFirstPop),
         io:fwrite("Poping ~p from stack~n", [Left]),
         StackAfterSecondPop = stack:pop(StackAfterFirstPop),
         case Token of
            "+" ->
               io:fwrite("Pushing ~p into stack~n", [Left + Right]),
               stack:push(Left + Right, StackAfterSecondPop);
            "-" ->
               io:fwrite("Pushing ~p into stack~n", [Left - Right]),
               stack:push(Left - Right, StackAfterSecondPop);
            "*" ->
               io:fwrite("Pushing ~p into stack~n", [Left * Right]),
               stack:push(Left * Right, StackAfterSecondPop);
            "/" ->
               io:fwrite("Pushing ~p into stack~n", [Left / Right]),
               stack:push(Left / Right, StackAfterSecondPop);
            "//" ->
               case is_integer(Left) and is_integer(Right) of
                  true ->
                     io:fwrite("Pushing ~p into stack~n", [Left div Right]), 
                     stack:push(Left div Right, StackAfterSecondPop);
                  false -> throw({error, io_lib:format("Error at position ~w: both operands must be integers.", [Position])})
               end;
            "%" ->
               case is_integer(Left) and is_integer(Right) of
                  true ->
                     io:fwrite("Pushing ~p into stack~n", [Left rem Right]),
                     stack:push(Left rem Right, StackAfterSecondPop);
                  false -> throw({error, io_lib:format("Error at position ~w: both operands must be integers.", [Position])})
               end;
            "**" ->
               io:fwrite("Pushing ~p into stack~n", [math:pow(Left, Right)]),
               stack:push(math:pow(Left, Right), StackAfterSecondPop)
         end
   end.


'_tokenize'(_Expression, [], _Position, Acc) -> lists:reverse(Acc);

'_tokenize'(Expression, Tokens, Position, Acc) ->
   [FirstToken | RestTokens] = Tokens,
   FirstPositionOfToken = string:str(Expression, FirstToken),
   LengthOfToken = string:length(FirstToken),
   SubstringStart = FirstPositionOfToken + LengthOfToken,
   TrimmedExpression = string:slice(Expression, SubstringStart),
   '_tokenize'(TrimmedExpression, RestTokens, Position + SubstringStart, [{FirstToken, Position + FirstPositionOfToken - 1} | Acc]).

tokenize(Expression) ->
   Tokens = lists:filter(fun(X) -> (X =/= "") and not(lists:member(X, "\s\t\n")) end, 
               lists:map(fun(Token) -> binary_to_list(Token) end, 
                  re:split(Expression, "\\s+")
               )
            ),
   '_tokenize'(Expression, Tokens, 1, []).


'_evaluate_rpn'([], Stack) -> stack:peek(Stack);

'_evaluate_rpn'(Tokens, Stack) ->
   [FirstToken | RestTokens] = Tokens,
   '_evaluate_rpn'(RestTokens, handle_token(FirstToken, Stack)).


evaluate_rpn(Expression) -> '_evaluate_rpn'(tokenize(Expression), []).


start() ->
   io:fwrite("~p~n", [evaluate_rpn("1 2 3 * + 4 -")]),
   io:fwrite("~p~n", [evaluate_rpn(" 8 2 5 * + 1 3 2 * + 4 - / ")]).
