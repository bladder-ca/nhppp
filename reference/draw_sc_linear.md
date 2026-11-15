# Special case: Simulate from a non homogeneous Poisson Point Process (NHPPP) from (t_min, t_max) with linear intensity function (inversion method)

Sample NHPPP times from a linear intensity function using the inversion
method, optionally using an `rstream` generator

## Usage

``` r
draw_sc_linear(
  intercept,
  slope,
  t_min,
  t_max,
  atmost1 = FALSE,
  report_first_K = NULL,
  report_last_K = NULL,
  atleast1 = FALSE,
  generate_at_least_K = NULL,
  generate_at_most_K = NULL
)
```

## Arguments

- intercept:

  (double) the intercept

- slope:

  (double) the slope

- t_min:

  (double) lower bound of the time interval

- t_max:

  (double) upper bound of the time interval

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

a vector of event times (t\_); if no events realize, a vector of length
0

## Examples

``` r
x <- draw_sc_linear(intercept = 0, slope = 0.2, t_min = 0, t_max = 10)
```
