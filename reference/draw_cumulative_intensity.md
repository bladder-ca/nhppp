# Simulate from a non homogeneous Poisson Point Process (NHPPP) over an interval when you know the cumulative intensity and its inverse.

Sample NHPPP times using the inversion method

## Usage

``` r
draw_cumulative_intensity(
  Lambda,
  Lambda_inv,
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

- Lambda:

  (function, double vector) a continuous increasing R to R map which is
  the integrated rate of the NHPPP

- Lambda_inv:

  (function, double vector) the inverse of `Lambda()`

- t_min:

  (double) the lower bound of the time interval

- t_max:

  (double) the upper bound of the time interval

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
