# Vectorized sampling from a zero-truncated non homogeneous Poisson Point Process (NHPPP) from an interval (thinning method) with piecewise constant_majorizers

Vectorized sampling from a zero-truncated non homogeneous Poisson Point
Process (NHPPP) from an interval (thinning method) with piecewise
constant_majorizers. The majorizers are step functions over equal-length
time intevals.

## Usage

``` r
vztdraw_intensity(
  lambda = NULL,
  lambda_args = NULL,
  Lambda_maj_matrix = NULL,
  lambda_maj_matrix = NULL,
  range_t = NULL,
  tol = 10^-6,
  atmost1 = FALSE,
  ...
)
```

## Arguments

- lambda:

  (function) a vectorized intensity function, with one or two arguments.
  The first is time. The optional second is a named list with additional
  arguments.

- lambda_args:

  (list) optional list of named arguments for `lambda()`

- Lambda_maj_matrix:

  (matrix) for the majorizeintegrated intensity rates at the end of each
  interval

- lambda_maj_matrix:

  (matrix) intensity rates, one per interval

- range_t:

  (vector, or matrix) `t_min` and `t_max`, possibly vectorized

- tol:

  (scalar, double) tolerance for the number of events

- atmost1:

  boolean, draw at most 1 event time

- ...:

  (any) other arguments (ignored – used for flexibility in calling from
  other functions)

## Value

a matrix of event times (columns) per draw (rows) NAs are structural
empty spots
