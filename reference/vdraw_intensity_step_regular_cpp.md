# Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) given the intensity function with piecewise constant_majorizers (C++)

Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP)
using the intensity function and piecewise constant_majorizers. The
majorizers are step functions over equal-length time intevals.

## Usage

``` r
vdraw_intensity_step_regular_cpp(
  lambda = NULL,
  lambda_args = NULL,
  Lambda_maj_matrix = NULL,
  lambda_maj_matrix = NULL,
  rate_matrix_t_min = NULL,
  rate_matrix_t_max = NULL,
  t_min = NULL,
  t_max = NULL,
  tol = 10^-6,
  atmost1 = FALSE,
  report_first_K = NULL,
  report_last_K = NULL,
  generate_at_least_K = NULL,
  budget_cap = NULL,
  atmostB = NULL
)
```

## Arguments

- lambda:

  (function) intensity function, vectorized

- lambda_args:

  (list) arguments for `lambda`, with up to two elements: `shared`
  (named list of row-invariant arguments of any type, stored once and
  never replicated) and `row_args` (data.frame or data.table with one
  row per point process, auto-subset when rows are resampled). When
  non-`NULL`, the container is passed as `lambda`'s second positional
  argument exactly as given, except that `row_args` is already
  row-subset; the name of the second formal is up to you. A flat list
  with neither element is treated as all-shared; a `vector_arguments`
  element is deprecated (use `row_args`).

- Lambda_maj_matrix:

  (matrix) integrated intensity rates at the end of each interval

- lambda_maj_matrix:

  (matrix) intensity rates, one per interval

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

  `NULL` or a positive integer: report only the earliest K accepted
  event times. Ignored (all accepted events are returned) when
  `generate_at_least_K >= 1` — the conditional rejection loop needs the
  full survivor count and applies reporting after convergence.

- report_last_K:

  `NULL` or a positive integer: report only the latest K accepted event
  times (ascending order). Same conditional-path caveat as
  `report_first_K`.

- generate_at_least_K:

  `NULL`, or a positive integer: draw the majorizer candidates
  conditioned on at least K majorizer events. NOTE: this does not by
  itself condition the thinned process on K accepted events; rows with
  too few survivors must be resampled by the caller (see
  [`vztdraw_intensity_step_regular()`](https://bladder-ca.github.io/nhppp/reference/vztdraw_intensity_step_regular.md)).

- budget_cap:

  `NULL` or a positive integer: cap the computational event budget of
  the majorizer kernel (approximation knob).

- atmostB:

  deprecated alias for `budget_cap`.

## Value

a matrix of event times (columns) per draw (rows) NAs are structural
empty spots
