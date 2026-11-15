# Vectorized sampling from NHPPPs given the intensity function, with piecewise constant majorizers over arbitrary interval bounds (C++)

Vectorized thinning sampler: candidate times are proposed from a
piecewise constant majorizer and accepted with probability
`lambda(t) / lambda_maj(interval of t)`. Unlike the `sc_step` samplers,
`lambda` is an arbitrary (vectorized) R function — the step structure
only concerns the majorizer. The majorizer intervals need not have the
same length: their bounds are given in `time_breaks`, either once for
all point processes or per point process. This generalizes
[`vdraw_intensity()`](https://bladder-ca.github.io/nhppp/reference/vdraw_intensity.md),
which assumes equal-length ("regular") intervals.

## Usage

``` r
vdraw_intensity_step(
  lambda = NULL,
  lambda_args = NULL,
  Lambda_maj_matrix = NULL,
  lambda_maj_matrix = NULL,
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
  budget_cap = NULL
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

  (matrix) integrated majorizer intensity rates at the end of each
  interval

- lambda_maj_matrix:

  (matrix) majorizer intensity rates, one per interval

- time_breaks:

  (vector \| matrix) the bounds of the majorizer intervals. With `K`
  intervals (`K = ncol([Lambda|lambda]_maj_matrix)`), either a vector of
  `K+1` increasing values (the same bounds for all point processes) or a
  matrix with `K+1` columns whose i-th row holds the bounds for the i-th
  point process (a 1-row matrix is recycled for all point processes).

- t_min:

  (scalar \| vector \| column matrix) is the lower bound of a
  subinterval of `(time_breaks[, 1], time_breaks[, K+1]]`. If set, times
  are sampled from the subinterval.

- t_max:

  (scalar \| vector \| column matrix) the upper bound of a subinterval
  of `(time_breaks[, 1], time_breaks[, K+1]]`. If set, times are sampled
  from the subinterval.

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
  [`vztdraw_intensity_step()`](https://bladder-ca.github.io/nhppp/reference/vztdraw_intensity_step.md)).

- generate_at_most_K:

  `NULL` or a positive integer: condition on at most K accepted events
  (rejection resampling on the upper bound).

- budget_cap:

  `NULL` or a positive integer: cap the computational event budget of
  the majorizer kernel (approximation knob).

## Value

a matrix of event times (columns) per draw (rows); NAs are structural
empty spots

## Examples

``` r
x <- vdraw_intensity_step(
  lambda = function(x, ...) 0.1 * x,
  lambda_maj_matrix = matrix(rep(1, 5), nrow = 1),
  time_breaks = c(1, 1.5, 3, 4, 4.5, 5)
)

# conditioned on exactly two events
x <- vdraw_intensity_step(
  lambda = function(x, ...) 0.1 * x,
  lambda_maj_matrix = matrix(rep(1, 50), nrow = 10),
  time_breaks = c(1, 1.5, 3, 4, 4.5, 5),
  generate_at_least_K = 2, generate_at_most_K = 2
)
```
