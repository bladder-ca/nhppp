# Vectorized sampling from zero-truncated NHPPPs with piecewise constant intensities with same interval lengths

Simulate a piecewise constant-rate Poisson Point Process over
`(t_min, t_max]` (inversion method) where the intervals have the same
length (are "regular").

## Usage

``` r
vztdraw_sc_step_regular_cpp(
  lambda_matrix = NULL,
  Lambda_matrix = NULL,
  rate_matrix_t_min = NULL,
  rate_matrix_t_max = NULL,
  t_min = NULL,
  t_max = NULL,
  atmost1 = FALSE
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

- atmost1:

  boolean, draw at most 1 event time

## Value

a vector of event times t if no events realize, it will have 0 length
