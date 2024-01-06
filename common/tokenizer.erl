-module(tokenizer).
-export([tokenize/1]).

%     Operators = [
%         {"+", add, 5, left}, 
%         {"-", subtr, 5, left}, 
%         {"*", mult, 4, left}, 
%         {"/", float_div, 4, left}, 
%         {"//", int_div, 4, left}, 
%         {"%", mod, 4, left}, 
%         {"**", exp, 3, right},
%         {"+", unary_plus, 2, right},
%         {"-", unary_minus, 2, right},  
%         {"(", left_paren, 1, none}, 
%         {")", right_paren, 1, none}
%     ],


% '_read_number_string'([], Buffer, Length) -> {[], lists:reverse(Buffer), Length};

% '_read_number_string'(Expression, Buffer, Length) ->
%     [FirstChar | RestChars] = Expression,
%     case utils:is_digit(FirstChar) or (FirstChar =:= $.) of
%         true -> '_read_number_string'(RestChars, [FirstChar | Buffer], Length + 1);
%         false -> 
%             case utils:is_whitespace(FirstChar) of
%                 true -> {RestChars, lists:reverse(Buffer), Length + 1};
%                 false -> {Expression, lists:reverse(Buffer), Length}
%             end
%     end.
        
% read_number_string(Expression) -> '_read_number_string'(Expression, [], 0).


% '_read_operator_string'([], Buffer, Length) -> {[], lists:reverse(Buffer), Length};

% '_read_operator_string'(Expression, Buffer, Length) ->
%     [FirstChar | RestChars] = Expression,
%     case utils:is_digit(FirstChar) of
%         true -> {Expression, lists:reverse(Buffer), Length};
%         false -> 
%             case utils:is_whitespace(FirstChar) of
%                 true -> {RestChars, lists:reverse(Buffer), Length + 1};
%                 false -> '_read_operator_string'(RestChars, [FirstChar | Buffer], Length + 1)
%             end
%     end.

% read_operator_string(Expression) -> '_read_operator_string'(Expression, [], 0).


% '_tokenize'([], Tokens, _Position) -> Tokens;

% '_tokenize'(Expression, Tokens, Position) ->
%     {RestCharsAfterReadingNumber, NumberBuffer, LengthOfNumber} = read_number_string(Expression),
%     case LengthOfNumber =/= 0 of
%         true -> '_tokenize'(RestCharsAfterReadingNumber, [{NumberBuffer, Position} | Tokens], Position + LengthOfNumber);
%         false ->
%             {RestCharsAfterReadingOperator, OperatorBuffer, LengthOfOperator} = read_operator_string(Expression),
%             '_tokenize'(RestCharsAfterReadingOperator, [{OperatorBuffer, Position} | Tokens], Position + LengthOfOperator)
%         end.

% tokenize(Expression) -> lists:reverse('_tokenize'(Expression, [], 1)).

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