-module(utils).
-export([try_parse_number/1, is_digit/1]).

try_parse_number(String) ->
    case io_lib:fread("~d", String) of
        {ok, Value} -> Value;
        error -> String
    end.


is_digit(Char) -> lists:member(Char, lists:seq($0, $9)).
