# Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from an interval (thinning method) with piecewise constant_majorizers (C++)

Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP)
from an interval (thinning method) with piecewise constant_majorizers.
The majorizers are step functions over equal-length time intevals.

## Usage

``` r
vdraw_intensity(
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
  atleast1 = FALSE,
  generate_at_least_K = NULL,
  generate_at_most_K = NULL,
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
  event times (reporting truncation — the count law is unchanged). At
  most one of `report_first_K`/`report_last_K` may be set.

- report_last_K:

  `NULL` or a positive integer: report only the latest K accepted event
  times (ascending order; reporting truncation).

- atleast1:

  boolean, condition on at least 1 accepted event (alias for
  `generate_at_least_K = 1`)

- generate_at_least_K:

  `NULL` or a positive integer: condition on at least K accepted events
  (rejection resampling; see
  [`vztdraw_intensity_step_regular()`](https://bladder-ca.github.io/nhppp/reference/vztdraw_intensity_step_regular.md)).

- generate_at_most_K:

  `NULL` or a positive integer: condition on at most K accepted events
  (rejection resampling on the upper bound).

- budget_cap:

  `NULL` or a positive integer: cap the computational event budget of
  the majorizer kernel (approximation knob).

- atmostB:

  deprecated alias for `budget_cap`.

## Value

a matrix of event times (columns) per draw (rows) NAs are structural
empty spots

## Examples

``` r
x <- vdraw_intensity(
  lambda = function(x, ...) 0.1 * x,
  lambda_maj_matrix = matrix(rep(1, 5), nrow = 1),
  rate_matrix_t_min = 1,
  rate_matrix_t_max = 5
)

# shared and per-row lambda arguments in the structured container,
# conditioned on at least one event
l <- function(t, a) a$shared$scale * t^a$row_args$exponent
x <- vdraw_intensity(
  lambda = l,
  lambda_args = list(
    shared = list(scale = 0.1),
    row_args = data.frame(exponent = seq(1, 1.9, by = 0.1))
  ),
  lambda_maj_matrix = matrix(rep(3, 50), nrow = 10),
  rate_matrix_t_min = 1,
  rate_matrix_t_max = 5,
  generate_at_least_K = 1
)
```
