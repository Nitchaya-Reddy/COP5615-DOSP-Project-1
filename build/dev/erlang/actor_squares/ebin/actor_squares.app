{application, actor_squares, [
    {vsn, "1.0.0"},
    {applications, [gleam_erlang,
                    gleam_otp,
                    gleam_stdlib,
                    gleeunit]},
    {description, "Actor-model Gleam project: find starts s in 1..N where sum_{i=0}^{k-1} (s+i)^2 is a perfect square."},
    {modules, []},
    {registered, []}
]}.
