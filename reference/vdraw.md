# Vectorized generic function for simulating from NHPPPs given the intensity function or the cumulative intensity function

This is a wrapper to the package's specific functions, and thus slightly
slower. For time-intensive simulations prefer one of the specific
functions.

## Usage

``` r
vdraw(
  lambda = NULL,
  lambda_args = NULL,
  Lambda_maj_matrix = NULL,
  lambda_maj_matrix = NULL,
  Lambda = NULL,
  Lambda_inv = NULL,
  Lambda_args = NULL,
  Lambda_inv_args = NULL,
  t_min = NULL,
  t_max = NULL,
  rate_matrix_t_min = NULL,
  rate_matrix_t_max = NULL,
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

- Lambda:

  (function, double vector) an increasing function which is the
  integrated rate of the NHPPP. It should take a vectorized argument t
  for times and an optional arguments list.

- Lambda_inv:

  (function, double vector) the inverse of `Lambda()`, also in
  vectorized form It should take a vectorized argument z and an optional
  arguments list.

- Lambda_args:

  (list) optional arguments to pass to Lambda.

- Lambda_inv_args:

  (list) optional arguments to pass to Lambda_inv().

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

- rate_matrix_t_min:

  (scalar \| vector \| column matrix) is the lower bound of the time
  interval for each row of (Lambda\|lambda)\_maj_matrix. The length of
  this argument is the number of point processes that should be drawn.

- rate_matrix_t_max:

  (scalar \| vector \| column matrix) the upper bound of the time
  interval for each row of (Lambda\|lambda)\_maj_matrix. The length of
  this argument is the number of point processes that should be drawn.

- tol:

  (scalar, double) tolerance for the number of events

- atmost1:

  boolean, draw at most 1 event time

- atleast1:

  boolean, draw at least 1 event time

- atmostB:

  If not NULL, draw at most B (B\>0) event times. NULL means ignore.

## Value

a vector of event times
