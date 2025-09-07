-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\main.gleam").
-export([main/1]).

-file("src\\main.gleam", 6).
-spec main(list(binary())) -> nil.
main(Args) ->
    case Args of
        [N_str, K_str] ->
            N@1 = case gleam_stdlib:parse_int(N_str) of
                {ok, N} -> N;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"main"/utf8>>,
                                function => <<"main"/utf8>>,
                                line => 9,
                                value => _assert_fail,
                                start => 142,
                                'end' => 177,
                                pattern_start => 153,
                                pattern_end => 158})
            end,
            K@1 = case gleam_stdlib:parse_int(K_str) of
                {ok, K} -> K;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"main"/utf8>>,
                                function => <<"main"/utf8>>,
                                line => 10,
                                value => _assert_fail@1,
                                start => 184,
                                'end' => 219,
                                pattern_start => 195,
                                pattern_end => 200})
            end,
            Result = boss:find_all(N@1, K@1, 1000),
            case Result of
                {ok, Starts} ->
                    _pipe = Starts,
                    gleam@list:each(
                        _pipe,
                        fun(S) ->
                            gleam_stdlib:println(erlang:integer_to_binary(S))
                        end
                    );

                {error, Msg} ->
                    gleam_stdlib:println(<<"Error: "/utf8, Msg/binary>>)
            end;

        _ ->
            gleam_stdlib:println(<<"Usage: lukas N k"/utf8>>)
    end.
