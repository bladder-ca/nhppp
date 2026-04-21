# Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from an interval (thinning method) with piecewise constant_majorizers (R) – but can be forced to sample from zero-truncated proposals.

Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP)
from an interval (thinning method) with piecewise constant_majorizers.
The majorizers are step functions over equal-length time intevals. This
function is used for obtainning proposals for
[`vztdraw_intensity_step_regular()`](https://bladder-ca.github.io/nhppp/reference/vztdraw_intensity_step_regular.md)

## Usage

``` r
vdraw_intensity_step_regular_forcezt(
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
  force_zt_majorizer = FALSE,
  ...
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

- force_zt_majorizer:

  boolean, force the majorizer to be zero-truncated. This option is used
  when the function is called to make proposals for
  [`vztdraw_intensity_step_regular()`](https://bladder-ca.github.io/nhppp/reference/vztdraw_intensity_step_regular.md).
  In general, do not set this option to `TRUE`.
