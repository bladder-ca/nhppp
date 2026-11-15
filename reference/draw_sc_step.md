# Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method) The intervals need not have the same length.

Simulate a piecewise constant-rate Poisson Point Process over
`(t_min, t_max]` (inversion method) The intervals need not have the same
length.

## Usage

``` r
draw_sc_step(
  lambda_vector,
  time_breaks,
  atmost1 = FALSE,
  report_first_K = NULL,
  report_last_K = NULL,
  atleast1 = FALSE,
  generate_at_least_K = NULL,
  generate_at_most_K = NULL
)
```

## Arguments

- lambda_vector:

  (scalar, double) `K` constant rates, one per interval

- time_breaks:

  (vector, double) `K+1` time points defining `K` intervals of constant
  rates: `[t_1 = range_t[1], t_2)`: the first interval `[t_k, t_{k+1})`:
  the `k`-th interval `[t_{K}, t_{K+1} = range_t[2])`: the `K`-th (last)
  interval

- atmost1:

  boolean, report at most 1 event time (alias for `report_first_K = 1`)

- report_first_K:

  `NULL` or a positive integer: report only the earliest K event times
  (reporting truncation — the count law is unchanged).

- report_last_K:

  `NULL` or a positive integer: report only the latest K event times
  (ascending order; reporting truncation).

- atleast1:

  boolean, condition on at least 1 event (alias for
  `generate_at_least_K = 1`)

- generate_at_least_K:

  `NULL` or a positive integer: condition the sampled process on at
  least K events.

- generate_at_most_K:

  `NULL` or a positive integer: condition the sampled process on at most
  K events.

## Value

a vector of event times t if no events realize, it will have 0 length

## Examples

``` r
x <- draw_sc_step(lambda_vector = rep(1, 5), time_breaks = c(0:5))
```
