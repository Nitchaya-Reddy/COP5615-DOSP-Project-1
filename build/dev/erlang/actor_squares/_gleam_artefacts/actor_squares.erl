-module(actor_squares).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\actor_squares.gleam").
-export([read_line/1, main/0]).

-file("src\\actor_squares.gleam", 8).
-spec read_line(binary()) -> binary().
read_line(Prompt) ->
    io:get_line(Prompt).

-file("src\\actor_squares.gleam", 10).
-spec main() -> nil.
main() ->
    N_input = begin
        _pipe = io:get_line(<<"Enter n:"/utf8>>),
        _pipe@1 = gleam@string:trim(_pipe),
        gleam_stdlib:parse_int(_pipe@1)
    end,
    N = case N_input of
        {ok, Value} ->
            Value;

        {error, _} ->
            0
    end,
    K_input = begin
        _pipe@2 = io:get_line(<<"Enter k:"/utf8>>),
        _pipe@3 = gleam@string:trim(_pipe@2),
        gleam_stdlib:parse_int(_pipe@3)
    end,
    K = case K_input of
        {ok, Value@1} ->
            Value@1;

        {error, _} ->
            0
    end,
    Result = boss:find_all_squares(N, K, 650),
    case Result of
        {ok, Starts} ->
            gleam_stdlib:println(
                <<"Starting numbers of consecutive squares sequences:"/utf8>>
            ),
            Print_number = fun(S) ->
                gleam_stdlib:println(erlang:integer_to_binary(S))
            end,
            gleam@list:each(Starts, Print_number);

        {error, Msg} ->
            gleam_stdlib:println(<<"Error: "/utf8, Msg/binary>>)
    end.
