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


'_read_number_string'([], Buffer, Length) -> {[], lists:reverse(Buffer), Length};

'_read_number_string'(Expression, Buffer, Length) ->
    [FirstChar | RestChars] = Expression,
    case utils:is_digit(FirstChar) or (FirstChar =:= $.) of
        true -> '_read_number_string'(RestChars, [FirstChar | Buffer], Length + 1);
        false -> 
            case utils:is_whitespace(FirstChar) of
                true -> {RestChars, lists:reverse(Buffer), Length + 1};
                false -> {Expression, lists:reverse(Buffer), Length}
            end
    end.
        
read_number_string(Expression) -> '_read_number_string'(Expression, [], 0).


'_read_operator_string'([], Buffer, Length) -> {[], lists:reverse(Buffer), Length};

'_read_operator_string'(Expression, Buffer, Length) ->
    [FirstChar | RestChars] = Expression,
    case utils:is_digit(FirstChar) of
        true -> {Expression, lists:reverse(Buffer), Length};
        false -> 
            case utils:is_whitespace(FirstChar) of
                true -> {RestChars, lists:reverse(Buffer), Length + 1};
                false -> '_read_operator_string'(RestChars, [FirstChar | Buffer], Length + 1)
            end
    end.

read_operator_string(Expression) -> '_read_operator_string'(Expression, [], 0).


'_tokenize'([], Tokens, _Position) -> Tokens;

'_tokenize'(Expression, Tokens, Position) ->
    {RestCharsAfterReadingNumber, NumberBuffer, LengthOfNumber} = read_number_string(Expression),
    case LengthOfNumber =/= 0 of
        true -> '_tokenize'(RestCharsAfterReadingNumber, [{NumberBuffer, Position} | Tokens], Position + LengthOfNumber);
        false ->
            {RestCharsAfterReadingOperator, OperatorBuffer, LengthOfOperator} = read_operator_string(Expression),
            '_tokenize'(RestCharsAfterReadingOperator, [{OperatorBuffer, Position} | Tokens], Position + LengthOfOperator)
        end.

tokenize(Expression) -> lists:reverse('_tokenize'(Expression, [], 1)).
