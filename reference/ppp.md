# Simulate a homogeneous Poisson Point Process in (t_min, t_max\]

Simulate a homogeneous Poisson Point Process in (t_min, t_max\]

## Usage

``` r
ppp(
  rate,
  t_min,
  t_max,
  atmost1 = FALSE,
  tol = 10^-6,
  report_first_K = NULL,
  report_last_K = NULL,
  atleast1 = FALSE,
  generate_at_least_K = NULL,
  generate_at_most_K = NULL
)
```

## Arguments

- rate:

  (scalar, double) constant instantaneous rate

- t_min:

  (scalar, double) the lower bound of the time interval

- t_max:

  (scalar, double) the upper bound of the time interval

- atmost1:

  boolean, report at most 1 event time (alias for `report_first_K = 1`)

- tol:

  the probability that we will have more than the drawn events in
  (t_min, t_max\]

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
x <- ppp(rate = 1, t_min = 0, t_max = 10, tol = 10^-6)
```
