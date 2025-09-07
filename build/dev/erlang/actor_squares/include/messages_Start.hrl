-record(start, {
    n :: integer(),
    k :: integer(),
    work_unit :: integer(),
    client :: gleam@erlang@process:subject(list(integer()))
}).
