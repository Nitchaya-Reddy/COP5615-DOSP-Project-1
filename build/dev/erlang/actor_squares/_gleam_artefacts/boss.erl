-module(boss).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\boss.gleam").
-export([find_all_squares/3]).
-export_type([manager_state/0, chunk_range/0]).

-type manager_state() :: idle |
    {running,
        integer(),
        list(integer()),
        gleam@erlang@process:subject(list(integer()))}.

-type chunk_range() :: {chunk_range, integer(), integer()}.

-file("src\\boss.gleam", 80).
-spec accumulate_chunks(integer(), list(chunk_range()), integer(), integer()) -> list(chunk_range()).
accumulate_chunks(Start, Acc, Max, Size) ->
    case Start > Max of
        true ->
            lists:reverse(Acc);

        false ->
            Stop = gleam@int:min((Start + Size) - 1, Max),
            accumulate_chunks(
                Start + Size,
                [{chunk_range, Start, Stop} | Acc],
                Max,
                Size
            )
    end.

-file("src\\boss.gleam", 76).
-spec generate_chunks(integer(), integer()) -> list(chunk_range()).
generate_chunks(Max, Size) ->
    accumulate_chunks(1, [], Max, Size).

-file("src\\boss.gleam", 92).
-spec calculate_chunk_count(integer(), integer()) -> integer().
calculate_chunk_count(Max, Size) ->
    Q@1 = case gleam@int:divide((Max + Size) - 1, Size) of
        {ok, Q} -> Q;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"boss"/utf8>>,
                        function => <<"calculate_chunk_count"/utf8>>,
                        line => 93,
                        value => _assert_fail,
                        start => 2685,
                        'end' => 2740,
                        pattern_start => 2696,
                        pattern_end => 2701})
    end,
    Q@1.

-file("src\\boss.gleam", 43).
-spec handle_manager(manager_state(), messages:control_msg()) -> gleam@otp@actor:next(manager_state(), messages:control_msg()).
handle_manager(State, Msg) ->
    case {State, Msg} of
        {idle, {initialize_search, Limit, _, Chunk_size, Requester}} ->
            Expected = calculate_chunk_count(Limit, Chunk_size),
            _pipe = {running, Expected, [], Requester},
            gleam@otp@actor:continue(_pipe);

        {{running, Pending, Results, Requester@1}, {partial_result, Found}} ->
            _pipe@1 = {running,
                Pending,
                lists:append(Results, Found),
                Requester@1},
            gleam@otp@actor:continue(_pipe@1);

        {{running, Pending@1, Results@1, Requester@2}, worker_finished} ->
            Remaining = Pending@1 - 1,
            case Remaining =:= 0 of
                true ->
                    gleam@erlang@process:send(Requester@2, Results@1),
                    gleam@otp@actor:stop();

                false ->
                    _pipe@2 = {running, Remaining, Results@1, Requester@2},
                    gleam@otp@actor:continue(_pipe@2)
            end;

        {_, terminate} ->
            gleam@otp@actor:stop();

        {_, _} ->
            gleam@otp@actor:continue(State)
    end.

-file("src\\boss.gleam", 14).
-spec find_all_squares(integer(), integer(), integer()) -> {ok, list(integer())} |
    {error, binary()}.
find_all_squares(Limit, Length, Chunk_size) ->
    Manager_actor@1 = case begin
        _pipe = gleam@otp@actor:new(idle),
        _pipe@1 = gleam@otp@actor:on_message(_pipe, fun handle_manager/2),
        gleam@otp@actor:start(_pipe@1)
    end of
        {ok, Manager_actor} -> Manager_actor;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"boss"/utf8>>,
                        function => <<"find_all_squares"/utf8>>,
                        line => 15,
                        value => _assert_fail,
                        start => 473,
                        'end' => 582,
                        pattern_start => 484,
                        pattern_end => 501})
    end,
    Requester = gleam@erlang@process:new_subject(),
    Chunks = generate_chunks(Limit, Chunk_size),
    gleam@otp@actor:send(
        erlang:element(3, Manager_actor@1),
        {initialize_search, Limit, Length, Chunk_size, Requester}
    ),
    _pipe@2 = Chunks,
    gleam@list:each(_pipe@2, fun(Chunk) -> case Chunk of
                {chunk_range, Start, Stop} ->
                    Worker_actor@1 = case worker:initiate_worker() of
                        {ok, Worker_actor} -> Worker_actor;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"boss"/utf8>>,
                                        function => <<"find_all_squares"/utf8>>,
                                        line => 29,
                                        value => _assert_fail@1,
                                        start => 854,
                                        'end' => 908,
                                        pattern_start => 865,
                                        pattern_end => 881})
                    end,
                    gleam@otp@actor:send(
                        Worker_actor@1,
                        {analyze,
                            Start,
                            Stop,
                            Length,
                            erlang:element(3, Manager_actor@1)}
                    )
            end end),
    case gleam@erlang@process:'receive'(Requester, 60000) of
        {ok, Data} ->
            {ok, Data};

        {error, nil} ->
            {error, <<"Timeout: no response from manager"/utf8>>}
    end.
