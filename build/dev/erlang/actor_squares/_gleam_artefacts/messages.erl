-module(messages).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\messages.gleam").
-export_type([task_msg/0, control_msg/0]).

-type task_msg() :: {analyze,
        integer(),
        integer(),
        integer(),
        gleam@erlang@process:subject(control_msg())} |
    halt_worker.

-type control_msg() :: {initialize_search,
        integer(),
        integer(),
        integer(),
        gleam@erlang@process:subject(list(integer()))} |
    {partial_result, list(integer())} |
    worker_finished |
    terminate.


