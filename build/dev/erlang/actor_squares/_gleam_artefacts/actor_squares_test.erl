-module(actor_squares_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test\\actor_squares_test.gleam").
-export([main/0, test_sum_formula/0, test_square_detection/0]).

-file("test\\actor_squares_test.gleam", 5).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("test\\actor_squares_test.gleam", 9).
-spec test_sum_formula() -> nil.
test_sum_formula() ->
    _pipe = 14,
    gleeunit@should:equal(_pipe, worker:consecutive_square_sum(1, 3)).

-file("test\\actor_squares_test.gleam", 14).
-spec test_square_detection() -> nil.
test_square_detection() ->
    _pipe = true,
    gleeunit@should:equal(_pipe, worker:check_square(25)),
    _pipe@1 = false,
    gleeunit@should:equal(_pipe@1, worker:check_square(26)).
