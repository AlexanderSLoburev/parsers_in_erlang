-module(shunting_yard_algorithm).
-export([start/0]).


process_binary_operator(Op, Operand1, Operand2) ->
    case Op of
        "+" -> Operand1 + Operand2;
        "-" -> Operand1 - Operand2;
        "*" -> Operand1 * Operand2;
        "/" -> Operand1 / Operand2;
        "//" ->
            case is_integer(Operand1) and is_integer(Operand2) of
                true -> Operand1 div Operand2;
                false -> throw({syntax_error, Op, Operand1, Operand2})
            end;
        "%" ->
            case is_integer(Operand1) and is_integer(Operand2) of
                true -> Operand1 rem Operand2;
                false -> throw({syntax_error, Op, Operand1, Operand2})
            end;
        "**" -> math:pow(Operand1, Operand2);
        _WrongOp -> throw({syntax_error, Op, Operand1, Operand2})
    end.


choose_operator(Op1Value, Op1Prec, Op1Assoc, Op2Value, Op2Prec) ->
    utils:ifelse(
        (Op1Value =/= "(") and ((Op2Prec > Op1Prec) or ((Op2Prec =:= Op1Prec) and (Op1Assoc =:= left)))
    , Op1Value, Op2Value).                


% {
%   Op: {operator, Name, Value, Prec, Assoc, Arity}
% }
process_operators(Op1Spec, OpSpecs, OpStack, NumStack) ->
    {operator, _Op1Name, Op1Value, Op1Prec, Op1Assoc, _Op1Arity} = Op1Spec,
    Op2Spec = dict:find(stack:peek(OpStack), OpStack),
    {operator, _Op2Name, Op2Value, Op2Prec, _Op2Assoc, _Op2Arity} = Op2Spec,
    case choose_operator(Op1Value, Op1Prec, Op1Assoc, Op2Value, Op2Prec) =:= Op2Value of
        true -> 
            process_operator(Op2Spec),
            process_operators(Op1Spec, OpSpecs, OpStack, NumStack);
        false -> none
    end.


is_fun(Token, OpSpecs) -> 
    case dict:take(Token, OpSpecs) of
        error -> false;
        {operator, _Name, Value, _Prec, _Assoc, _Arity} -> is_function(Value).


run_process_operators(Op1, OpSpecs, OpStack, NumStack) ->
    Op1Spec = dict:find(Op1, OpStack),
    case not is_fun(Op1, OpSpecs) of
        true ->
            Op2Spec = dict:find(stack:peek(OpStack), OpStack),
            process_operators();
        false -> none
    end,
    stack:push(Op1, OpStack).


'_handle_token'(Token, OpSpecs, OpStack, NumStack) ->
    % Check if the token is a number
    case utils:try_parse_number(Token) of
        {value, Value} -> stack:push(Value, NumStack);
        {error, no_float} ->
            case Token of
                "(" -> stack:push("(", OpStack);
                ")" -> process_closing_paren();
                Op -> run_process_operators(Op, OpSpecs, OpStack, NumStack)
            end
        end.


% '_handle_token'(Token, FunctionSpecs, OpSpecs, OpStack, NumStack) ->
%     case utils:try_parse_number(Token) of
%         ProcessingToken when is_number(ProcessingToken) -> stack:push(ProcessingToken, OpStack);
%         ProcessingToken ->
%             case lists:member(ProcessingToken, FunctionNames) of
%                 true -> stack:push(list_to_atom(ProcessingToken), OpStack);
%                 false ->
%                     OpSpec = dict:find(ProcessingToken, OpSpecs),
%                     case OpSpec of
%                         {Prec, Assoc, Arity} ->
%                             apply_operators(Op, OpSpec, OpStack, NumStack, OpSpecs)


start() ->
    io:fwrite("Hello world!~n").
