-record(running, {
    pending :: integer(),
    results :: list(integer()),
    requester :: gleam@erlang@process:subject(list(integer()))
}).
