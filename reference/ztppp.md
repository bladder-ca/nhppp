# Simulate a count-conditioned homogeneous Poisson Point Process over (t_min, t_max\]

Simulate a constant-rate Poisson Point Process conditional on the event
count lying in `[generate_at_least_K, generate_at_most_K]`
(`generate_at_least_K = 1` alone, the default, is the zero-truncated
process; `generate_at_least_K = generate_at_most_K = K` conditions on
exactly K events).

## Usage

``` r
ztppp(
  rate,
  t_min,
  t_max,
  atmost1 = FALSE,
  report_first_K = NULL,
  report_last_K = NULL,
  generate_at_least_K = 1,
  generate_at_most_K = NULL
)
```

## Arguments

- rate:

  (scalar, double) constant instantaneous rate

- t_min:

  (scalar, double) lower bound of the time interval

- t_max:

  (scalar, double) upper bound of the time interval

- atmost1:

  boolean, report at most 1 event time of the conditioned process (alias
  for `report_first_K = 1`)

- report_first_K:

  `NULL` or a positive integer: report only the earliest K event times
  of the conditioned realization (never affects the conditioned count
  itself). At most one of `report_first_K`/`report_last_K` may be set.

- report_last_K:

  `NULL` or a positive integer: report only the latest K event times of
  the conditioned realization (ascending order).

- generate_at_least_K:

  non-negative integer: condition on at least K events in
  `(t_min, t_max]`. The default 1 is the zero-truncated process.

- generate_at_most_K:

  `NULL` or a positive integer: condition on at most K events. May be
  combined with `generate_at_least_K` (K1 \<= K2).

## Value

a vector of event times

## Examples

``` r
x <- ztppp(t_min = 0, t_max = 10, rate = 0.001)
```
