-module(utils).
-export([try_parse_number/1, is_digit/1, re_escape/1]).

try_parse_number(String) ->
    case io_lib:fread("~d", String) of
        {ok, Value} -> Value;
        error -> String
    end.


is_digit(Char) -> lists:member(Char, lists:seq($0, $9)).

re_escape(Symbol) ->
    case lists:member(Symbol, "[]\\^$.|?*+()") of
        true ->
            case is_integer(Symbol) of
                true -> [$\ | [Symbol]];
                false -> [$\ | Symbol]
            end;
        false ->
            case is_integer(Symbol) of
                true -> [Symbol];
                false -> Symbol
            end
    end.