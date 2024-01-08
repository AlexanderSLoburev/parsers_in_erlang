-module(utils).
-export([
    try_parse_number/1, 
    is_digit/1, 
    is_whitespace/1, 
    re_escape/1,
    identity/1,
    ifelse/3,
    unzip4/1
]).


try_parse_number(String) ->
    case string:to_float(String) of
        {Float, Rest} ->
            case Rest =:= "" of
                true -> {value, Float};
                false -> {error, no_float}
            end;
        {error, no_float} -> {error, no_float}
    end.


is_digit(Char) -> lists:member(Char, lists:seq($0, $9)).


is_whitespace(Char) -> lists:member(Char, "\s\t\n\r").


re_escape(String) ->
    Escaped = re:replace(String, "[\\[\\]\\\\^$.|?*+()]", "\\\\&", [global]),
    Escaped.


identity(X) -> X.


ifelse(Condition, TrueResult, FalseResult) ->
    case Condition of
        true -> TrueResult;
        false -> FalseResult
    end.


'_unzip4'([], {Acc1, Acc2, Acc3, Acc4}) ->
    {lists:reverse(Acc1), lists:reverse(Acc2), lists:reverse(Acc3), lists:reverse(Acc4)};

'_unzip4'([{Value1, Value2, Value3, Value4} | Rest], {Acc1, Acc2, Acc3, Acc4}) ->
    '_unzip4'(Rest, {[Value1 | Acc1], [Value2 | Acc2], [Value3 | Acc3], [Value4 | Acc4]}).

unzip4(ListOfTuples) -> '_unzip4'(ListOfTuples, {[], [], [], []}).
