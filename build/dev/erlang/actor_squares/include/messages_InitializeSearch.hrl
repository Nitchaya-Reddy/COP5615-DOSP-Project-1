-record(initialize_search, {
    range_end :: integer(),
    segment_size :: integer(),
    block_size :: integer(),
    responder :: gleam@erlang@process:subject(list(integer()))
}).
