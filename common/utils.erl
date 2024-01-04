-module(utils).
-export([try_parse_number/1, is_digit/1, re_escape/1, tokenize/1]).


try_parse_number(String) ->
    case io_lib:fread("~d", String) of
        {ok, [Value], _} -> Value;
        {error, _} -> String
    end.


is_digit(Char) -> lists:member(Char, lists:seq($0, $9)).

re_escape(String) ->
    Escaped = re:replace(String, "[\\[\\]\\\\^$.|?*+()]", "\\\\&", [global]),
    Escaped.


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
