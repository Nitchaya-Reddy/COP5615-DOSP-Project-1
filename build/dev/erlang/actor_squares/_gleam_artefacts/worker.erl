-module(worker).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\worker.gleam").
-export([check_square/1, consecutive_square_sum/2, initiate_worker/0]).

-file("src\\worker.gleam", 51).
-spec check_square(integer()) -> boolean().
check_square(Num) ->
    case Num < 0 of
        true ->
            false;

        false ->
            case gleam@int:square_root(Num) of
                {ok, Root} ->
                    Rounded = erlang:round(Root),
                    (Rounded * Rounded) =:= Num;

                {error, _} ->
                    false
            end
    end.

-file("src\\worker.gleam", 65).
-spec consecutive_square_sum(integer(), integer()) -> integer().
consecutive_square_sum(Start, Count) ->
    Term1 = (Count * Start) * Start,
    Term2 = (Count * (Count - 1)) * Start,
    Term3 = (((Count - 1) * Count) * ((2 * Count) - 1)) div 6,
    (Term1 + Term2) + Term3.

-file("src\\worker.gleam", 36).
-spec evaluate(integer(), list(integer()), integer(), integer()) -> list(integer()).
evaluate(Current, Collected, Limit, Length) ->
    case Current > Limit of
        true ->
            lists:reverse(Collected);

        false ->
            Total = consecutive_square_sum(Current, Length),
            Updated = case check_square(Total) of
                true ->
                    [Current | Collected];

                false ->
                    Collected
            end,
            evaluate(Current + 1, Updated, Limit, Length)
    end.

-file("src\\worker.gleam", 32).
-spec calculate_valid_starts(integer(), integer(), integer()) -> list(integer()).
calculate_valid_starts(From, To, Count) ->
    evaluate(From, [], To, Count).

-file("src\\worker.gleam", 18).
-spec process_incoming(nil, messages:task_msg()) -> gleam@otp@actor:next(nil, messages:task_msg()).
process_incoming(_, Message) ->
    case Message of
        {analyze, Begin, Finish, Count, Boss} ->
            Findings = calculate_valid_starts(Begin, Finish, Count),
            gleam@erlang@process:send(Boss, {partial_result, Findings}),
            gleam@erlang@process:send(Boss, worker_finished),
            gleam@otp@actor:stop();

        halt_worker ->
            gleam@otp@actor:stop()
    end.

-file("src\\worker.gleam", 10).
-spec initiate_worker() -> {ok,
        gleam@erlang@process:subject(messages:task_msg())} |
    {error, gleam@otp@actor:start_error()}.
initiate_worker() ->
    _pipe = gleam@otp@actor:new(nil),
    _pipe@1 = gleam@otp@actor:on_message(_pipe, fun process_incoming/2),
    _pipe@2 = gleam@otp@actor:start(_pipe@1),
    gleam@result:map(_pipe@2, fun(Started) -> erlang:element(3, Started) end).
