-module(tokenizer).
-export([tokenize/1]).


% TOKEN_SPEC=[{TokenType, TokenRegexp}]
match(_Expression, []) -> throw({parse_error, _Expression});

match(Expression, TokenSpec) ->
    [{TokenType, TokenRegexp} | RestTokenSpec] = TokenSpec,
    case re:run(Expression, TokenRegexp, [{capture, first, list}]) of
        {match, [Token]} -> {TokenType, Token}; 
        nomatch -> match(Expression, RestTokenSpec)
    end.


'_tokenize'([], Tokens, _Position, _TokenSpec) -> lists:reverse(Tokens);

'_tokenize'(Expression, Tokens, Position, TokenSpec) ->
    {TokenType, Token} = try
        match(Expression, TokenSpec)
    catch 
        throw:{parse_error, Message} -> 
            throw({parse_error, io_lib:format("Error while parsing an \"~s\" in position ~w", [Message, Position])})
    end,
    TokenLength = length(Token),
    NextPosition = Position + TokenLength,
    RestExpression = string:slice(Expression, TokenLength),
    case TokenType =:= spaces of
        true -> '_tokenize'(RestExpression, Tokens, NextPosition, TokenSpec);
        false -> '_tokenize'(RestExpression, [{TokenType, Token, Position} | Tokens], NextPosition, TokenSpec)
    end.

tokenize(Expression) -> '_tokenize'(Expression, [], 1, [
    {spaces, "^\\s+"},
    {number, "^(?:\\d+(?:\\.\\d*)?|\\.\\'d+)"},
    {identifier, "^[a-zA-Z_][a-zA-Z0-9_]*"},
    {plus, "^\\+"},
    {minus, "^\\-"},
    {exponentiation, "^\\*\\*"},
    {multiplication, "^\\*"},
    {integer_division, "^\\//"},
    {division, "^\\/"},
    {reminder, "^%"},
    {left_paren, "^("},
    {right_paren, "^)"}
]).