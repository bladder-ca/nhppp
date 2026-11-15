# Vectorized sampling from NHPPPs with piecewise constant intensities with same interval lengths

Simulate a piecewise constant-rate Poisson Point Process over
`(t_min, t_max]` (inversion method) where the intervals have the same
length (are "regular").

## Usage

``` r
vdraw_sc_step_regular(
  lambda_matrix = NULL,
  Lambda_matrix = NULL,
  rate_matrix_t_min = NULL,
  rate_matrix_t_max = NULL,
  t_min = NULL,
  t_max = NULL,
  tol = 10^-6,
  atmost1 = FALSE,
  report_first_K = NULL,
  report_last_K = NULL,
  atleast1 = FALSE,
  generate_at_least_K = NULL,
  generate_at_most_K = NULL,
  budget_cap = NULL,
  atmostB = NULL,
  output = c("matrix", "long")
)
```

## Arguments

- lambda_matrix:

  (matrix) intensity rates, one per interval

- Lambda_matrix:

  (matrix) integrated intensity rates at the end of each interval

- rate_matrix_t_min:

  (scalar \| vector \| column matrix) is the lower bound of the time
  interval for each row of (Lambda\|lambda)\_maj_matrix. The length of
  this argument is the number of point processes that should be drawn.

- rate_matrix_t_max:

  (scalar \| vector \| column matrix) the upper bound of the time
  interval for each row of (Lambda\|lambda)\_maj_matrix. The length of
  this argument is the number of point processes that should be drawn.

- t_min:

  (scalar \| vector \| column matrix) is the lower bound of a
  subinterval of (rate_matrix_t_min, rate_matrix_t_max\]. If set, times
  are sampled from the subinterval. If omitted, it is equivalent to
  `rate_matrix_t_min`.

- t_max:

  (scalar \| vector \| column matrix) is the upper bound of a
  subinterval of (rate_matrix_t_min, rate_matrix_t_max\]. If set, times
  are sampled from the subinterval. If omitted, it is equivalent to
  `rate_matrix_t_max`.

- tol:

  (scalar, double) tolerance for the number of events

- atmost1:

  boolean, report at most 1 event time (alias for `report_first_K = 1`)

- report_first_K:

  `NULL` or a positive integer: report only the earliest K event times
  (a reporting truncation — the count law of the sampled process is
  unchanged). At most one of `report_first_K`/`report_last_K` may be
  set.

- report_last_K:

  `NULL` or a positive integer: report only the latest K event times
  (ascending order; reporting truncation).

- atleast1:

  boolean, condition on at least 1 event (alias for
  `generate_at_least_K = 1`)

- generate_at_least_K:

  `NULL` or a positive integer: condition the sampled process on at
  least K events in the (sub)interval (order-statistics sampling path).

- generate_at_most_K:

  `NULL` or a positive integer: condition the sampled process on at most
  K events. May be combined with `generate_at_least_K` (K1 \<= K2; K1 =
  K2 conditions on exactly K events).

- budget_cap:

  `NULL` or a positive integer: cap the computational event budget of
  the kernel. This is an approximation knob (it truncates the extreme
  tail of the event-count distribution together with the `1 - tol`
  quantile bound), not an exact reporting or conditioning contract.

- atmostB:

  deprecated alias for `budget_cap`.

- output:

  (string) `"matrix"` (default) for the NA-padded event matrix or
  `"long"` for the long event format `list(id, time, n_draws)`; see
  [`vdraw_sc_step()`](https://bladder-ca.github.io/nhppp/reference/vdraw_sc_step.md).

## Value

a matrix of event times t, with rows corresponding to the sampled point
processes, or the long event format if `output = "long"` if no events
realize, it will have 0 length

## Examples

``` r
x <- vdraw_sc_step_regular(
  Lambda_matrix = matrix(1:5, nrow = 1),
  rate_matrix_t_min = 100,
  rate_matrix_t_max = 110
)
```
