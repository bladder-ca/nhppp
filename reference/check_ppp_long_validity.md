# Check the validity of ppp samples in the long event format

Standard checks for a long-format sample `list(id, time, n_draws)`: ids
in range and ascending, times ascending within id, all times in
`[t_min, t_max]`, and the per-process count contracts. All checks are
whole-vector operations.

## Usage

``` r
check_ppp_long_validity(
  x,
  t_min,
  t_max = NULL,
  size = NULL,
  atmost1 = FALSE,
  atleast1 = FALSE,
  atmostk = NULL,
  atleastk = NULL
)
```

## Arguments

- x:

  (list) long event sample with elements `id`, `time`, `n_draws`

- t_min:

  (double \| vector) the start of the time interval

- t_max:

  (double \| vector) optional: the end of the time interval

- size:

  (double) optional: the per-process event count (empty processes are
  exempt, matching the matrix check)

- atmost1:

  (boolean) optional: at most one event per process

- atleast1:

  (boolean) optional: at least one event per process

- atmostk:

  (integer) optional: at most `k` events per process

- atleastk:

  (integer) optional: at least `k` events per process

## Value

None
