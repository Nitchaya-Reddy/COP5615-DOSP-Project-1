-module(argv).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\argv.gleam").
-export([load/0]).

-file("src\\argv.gleam", 3).
-spec load() -> list(binary()).
load() ->
    [].
