# actor_squares

Gleam + Actor Model solution that prints every starting value `s` in `1..N` such that

```
sum_{i=0}^{k-1} (s + i)^2
```

is a perfect square. The search is parallelised using actors (boss/worker).

## Build & Run

```
gleam deps download
gleam run -- <N> <k> [work_unit]
```

- `N` – max starting value to check (inclusive).
- `k` – length of consecutive numbers.
- `work_unit` – optional chunk size for each worker (default: 10000).

Example:

```
gleam run -- 10 2
```
