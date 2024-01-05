-module(utils).
-export([
    try_parse_number/1, 
    is_digit/1, 
    is_whitespace/1, 
    re_escape/1,
    identity/1,
    ifelse/3
]).


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


ifelse(Condition, TrueResult, FalseResult) ->
    case Condition of
        true -> TrueResult;
        false -> FalseResult
    end.
