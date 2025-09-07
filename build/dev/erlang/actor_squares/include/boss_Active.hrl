-record(active, {
    remaining_chunks :: integer(),
    collected :: list(integer()),
    client :: gleam@erlang@process:subject(list(integer()))
}).
