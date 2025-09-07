-record(analyze, {
    start_index :: integer(),
    end_index :: integer(),
    length :: integer(),
    supervisor :: gleam@erlang@process:subject(messages:control_msg())
}).
