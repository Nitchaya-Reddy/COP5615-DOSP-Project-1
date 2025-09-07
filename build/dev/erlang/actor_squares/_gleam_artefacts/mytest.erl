-module(mytest).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test\\mytest.gleam").
-export([get_line/0, main/0]).

-file("test\\mytest.gleam", 7).
-spec get_line() -> binary().
get_line() ->
    io:get_line().

-file("test\\mytest.gleam", 9).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(<<"Please enter a number:"/utf8>>),
    case io:get_line() of
        Input_string ->
            Trimmed_string = gleam@string:trim(Input_string),
            case gleam_stdlib:parse_int(Trimmed_string) of
                {ok, Number} ->
                    gleam_stdlib:println(
                        <<"You entered the number: "/utf8,
                            (erlang:integer_to_binary(Number))/binary>>
                    );

                _ ->
                    gleam_stdlib:println(
                        <<"Error: That was not a valid number."/utf8>>
                    )
            end
    end.
