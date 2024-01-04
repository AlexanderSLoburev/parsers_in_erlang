-module(utils).
-export([try_parse_number/1, is_digit/1, is_whitespace/1, re_escape/1]).


try_parse_number(String) ->
    case io_lib:fread("~d", String) of
        {ok, [Value], _} -> Value;
        {error, _} -> String
    end.


is_digit(Char) -> lists:member(Char, lists:seq($0, $9)).

is_whitespace(Char) -> lists:member(Char, "\s\t\n\r").

re_escape(String) ->
    Escaped = re:replace(String, "[\\[\\]\\\\^$.|?*+()]", "\\\\&", [global]),
    Escaped.


identity(X) -> X.


ternary(Condition, TrueResult, FalseResult) ->
    case Condition of
        true -> TrueResult;
        false -> FalseResult
    end.


% '_tokenize'(_Expression, [], _Position, Acc) -> lists:reverse(Acc);

% '_tokenize'(Expression, Tokens, Position, Acc) ->
%    [FirstToken | RestTokens] = Tokens,
%    FirstPositionOfToken = string:str(Expression, FirstToken),
%    LengthOfToken = string:length(FirstToken),
%    SubstringStart = FirstPositionOfToken + LengthOfToken,
%    TrimmedExpression = string:slice(Expression, SubstringStart),
%    '_tokenize'(TrimmedExpression, RestTokens, Position + SubstringStart, [{FirstToken, Position + FirstPositionOfToken - 1} | Acc]).

% tokenize(Expression) ->
%    Tokens = lists:filter(fun(X) -> (X =/= "") and not(lists:member(X, "\s\t\n")) end, 
%                lists:map(fun(Token) -> binary_to_list(Token) end, 
%                   re:split(Expression, "\\s+")
%                )
%             ),
%    '_tokenize'(Expression, Tokens, 1, []).


create_token(TokenString, StartPosition, EndPosition) ->
    TokenSpec = [
        {is_number, try_parse_number, number},
        {fun(X) -> X =:= "+" end, identity, add},
        {fun(X) -> X =:= "-" end, identity, subtr},
        {fun(X) -> X =:= "*" end, identity, mult},
        {fun(X) -> X =:= "/" end, identity, 'div'},
        {fun(X) -> X =:= "//" end, identity, int_div},
        {fun(X) -> X =:= "%" end, identity, 'rem'},
        {fun(X) -> X =:= "**" end, identity, exp},
        {fun(X) -> X =:= "(" end, identity, left_paren},
        {fun(X) -> X =:= ")" end, identity, right_paren}
    ],
    case lists:filtermap(fun({Predicate, Convert, Atom}) ->
        case Predicate(TokenString) of
            true -> {true, {Atom, Convert(TokenString), StartPosition, EndPosition}};
            false -> false
        end
    end, TokenSpec) of
        [{Value}] -> Value;
        false -> throw({error, io_lib:format("\"~p\" is an invalid token.", TokenString)})
    end.


flush_buffer(Buffer, Tokens, StartPosition, EndPosition) ->
    case Buffer =/= [] of
        true -> [create_token(Buffer, StartPosition, EndPosition) | Tokens];
        false -> Tokens
    end.


tokenize(Expression) ->
    lists:foldl(fun(Char, Acc) ->
        {Buffer, Tokens, StartPosition, EndPosition, SignAllowed} = Acc,
        case is_whitespace(Char) of
            true ->
                {[], flush_buffer(lists:reverse(Buffer), StartPosition, EndPosition), 0, Position + 1, false};
            false ->
                case is_digit(Char) or (Char =:= $.) of
                    true -> {[Char | Buffer], Tokens, StartPosition, Position + 1, false};
                    false -> {[], flush_buffer(lists:reverse(Buffer), 0, Position + 1, ternary(Char =:= $)), ))}
                        




    end, {[], [], 0, 0, true}, Expression).
