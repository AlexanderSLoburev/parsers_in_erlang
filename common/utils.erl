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
        {fun(X) -> X =:= "+" end, identity, '+'},
        {fun(X) -> X =:= "-" end, identity, '-'},
        {fun(X) -> X =:= "*" end, identity, '*'},
        {fun(X) -> X =:= "/" end, identity, '/'},
        {fun(X) -> X =:= "//" end, identity, '//'},
        {fun(X) -> X =:= "%" end, identity, '%'},
        {fun(X) -> X =:= "**" end, identity, '**'}
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
    lists:foldr(fun(Char, Acc) ->
        case is_whitespace(Char) of
            true -> 
                {Buffer, Tokens, StartPosition, EndPosition} = Acc,
                {flush_buffer(Buffer, Tokens, StartPosition, EndPosition), [], 0, Position + 1}
            false ->
                case is_digit(Char) of

    end, {[], [], 0, 0}, Expression).
