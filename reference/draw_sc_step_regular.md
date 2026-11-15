# Sampling from NHPPPs with piecewise constant intensities with same interval lengths (non-vectorized)

Sampling from NHPPPs with piecewise constant intensities with same
interval lengths (non-vectorized)

## Usage

``` r
draw_sc_step_regular(
  Lambda_vector = NULL,
  lambda_vector = NULL,
  t_min = NULL,
  t_max = NULL,
  atmost1 = FALSE,
  report_first_K = NULL,
  report_last_K = NULL,
  atleast1 = FALSE,
  generate_at_least_K = NULL,
  generate_at_most_K = NULL
)
```

## Arguments

- Lambda_vector:

  (scalar, double) `K` integrated intensity rates at the end of each
  interval

- lambda_vector:

  (scalar, double) `K` constant intensity rates, one per interval

- t_min:

  (scalar, double) lower bound of the time interval

- t_max:

  (scalar, double) upper bound of the time interval

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
x <- draw_sc_step_regular(Lambda_vector = 1:5, t_min = 0, t_max = 5)
```
