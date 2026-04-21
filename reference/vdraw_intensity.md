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
  atleast1 = FALSE,
  atmostB = NULL
)
```

## Arguments

- lambda:

  (function) intensity function, vectorized

- lambda_args:

  (list) optional arguments to pass to `lambda`

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

  boolean, draw at most 1 event time

- atleast1:

  boolean, draw at least 1 event time

- atmostB:

  If not NULL, draw at most B (B\>0) event times. NULL means ignore.

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
```
