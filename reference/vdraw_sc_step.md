# Vectorized sampling from NHPPPs with piecewise constant intensities over arbitrary interval bounds (C++)

Simulate piecewise constant-rate Poisson Point Processes (inversion
method) where the intervals need not have the same length. The interval
bounds are given in `time_breaks`, either once for all point processes
or per point process. This generalizes
[`vdraw_sc_step_regular()`](https://bladder-ca.github.io/nhppp/reference/vdraw_sc_step_regular.md),
which assumes equal-length ("regular") intervals.

## Usage

``` r
vdraw_sc_step(
  lambda_matrix = NULL,
  Lambda_matrix = NULL,
  time_breaks = NULL,
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

- time_breaks:

  (vector \| matrix) the bounds of the intervals over which the rates
  apply. With `K` intervals (`K = ncol([Lambda|lambda]_matrix)`), either
  a vector of `K+1` increasing values (the same bounds for all point
  processes) or a matrix with `K+1` columns whose i-th row holds the
  bounds for the i-th point process (a 1-row matrix is recycled for all
  point processes).

- t_min:

  (scalar \| vector \| column matrix) is the lower bound of a
  subinterval of `(time_breaks[, 1], time_breaks[, K+1]]`. If set, times
  are sampled from the subinterval. If omitted, it is equivalent to
  `time_breaks[, 1]`.

- t_max:

  (scalar \| vector \| column matrix) the upper bound of a subinterval
  of `(time_breaks[, 1], time_breaks[, K+1]]`. If set, times are sampled
  from the subinterval. If omitted, it is equivalent to
  `time_breaks[, K+1]`.

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

## Examples

``` r
# one set of interval bounds for all point processes
x <- vdraw_sc_step(
  lambda_matrix = matrix(rep(1, 50), nrow = 10),
  time_breaks = c(100, 100.5, 102, 106, 109, 110)
)

# separate interval bounds per point process
x <- vdraw_sc_step(
  lambda_matrix = matrix(rep(1, 50), nrow = 10),
  time_breaks = matrix(rep(c(100, 100.5, 102, 106, 109, 110), each = 10), nrow = 10) + 0:9
)

# condition on 2 to 4 events and report only the earliest two
x <- vdraw_sc_step(
  lambda_matrix = matrix(rep(1, 50), nrow = 10),
  time_breaks = c(100, 100.5, 102, 106, 109, 110),
  generate_at_least_K = 2, generate_at_most_K = 4,
  report_first_K = 2
)
```
