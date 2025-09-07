-record(scan, {
    start :: integer(),
    end_ :: integer(),
    k :: integer(),
    boss :: gleam@erlang@process:subject(messages:boss_msg())
}).
