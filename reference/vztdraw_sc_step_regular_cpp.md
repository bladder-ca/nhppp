# Vectorized sampling from zero-truncated NHPPPs with piecewise constant intensities with same interval lengths

Simulate a piecewise constant-rate Poisson Point Process over
`(t_min, t_max]` (inversion method) where the intervals have the same
length (are "regular"), conditional on the event count lying in
`[generate_at_least_K, generate_at_most_K]` (`generate_at_least_K = 1`
alone, the default, is the zero-truncated process).

## Usage

``` r
vztdraw_sc_step_regular_cpp(
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
  generate_at_least_K = 1,
  generate_at_most_K = NULL,
  budget_cap = NULL,
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

  non-negative integer: condition on at least K events in the sampled
  (sub)interval. The default 1 is the zero-truncated process.

- generate_at_most_K:

  `NULL` or a positive integer: condition on at most K events. May be
  combined with `generate_at_least_K` (K1 \<= K2; K1 = K2 conditions on
  exactly K events).

- budget_cap:

  `NULL` or a positive integer: cap the computational event budget of
  the kernel (approximation knob; never truncates below
  `generate_at_least_K`).

- output:

  (string) `"matrix"` (default) returns the NA-padded event matrix, one
  row per point process. `"long"` returns the long event format
  `list(id, time, n_draws)` with one entry per event: `id` is the
  1-based point-process index (ascending; times ascending within `id`).
  A point process with no events contributes no entries, so its id is
  absent; `n_draws` distinguishes "no events" from "not sampled". No
  `NA` is used. The long format is built without allocating the dense
  matrix, so prefer it when event counts vary widely across point
  processes.

## Value

a matrix of event times t, with rows corresponding to the sampled point
processes, or the long event format if `output = "long"`.
