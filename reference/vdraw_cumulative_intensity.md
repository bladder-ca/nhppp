# Vectorized simulation from a non homogeneous Poisson Point Process (NHPPP) from (t_min, t_max) given the cumulative intensity function and its inverse

Sample NHPPP times using the cumulative intensity function and its
inverse.

## Usage

``` r
vdraw_cumulative_intensity(
  Lambda,
  Lambda_inv,
  t_min,
  t_max,
  Lambda_args = NULL,
  Lambda_inv_args = NULL,
  tol = 10^-6,
  atmost1 = FALSE,
  atleast1 = FALSE
)
```

## Arguments

- Lambda:

  (function, double vector) an increasing function which is the
  integrated rate of the NHPPP. It should take a vectorized argument t
  for times and an optional arguments list.

- Lambda_inv:

  (function, double vector) the inverse of `Lambda()`, also in
  vectorized form It should take a vectorized argument z and an optional
  arguments list.

- t_min:

  (scalar \| vector \| column matrix) the lower bound of the interval
  for each sampled point process The length of this argument is the
  number of point processes that should be drawn.

- t_max:

  (scalar \| vector \| column matrix) the upper bound of the interval
  for each sampled point process The length of this argument is the
  number of point processes that should be drawn.

- Lambda_args:

  (list) optional arguments to pass to Lambda.

- Lambda_inv_args:

  (list) optional arguments to pass to Lambda_inv().

- tol:

  the tolerange for the calulations.

- atmost1:

  boolean, draw at most 1 event time per sampled point process.

- atleast1:

  boolean, draw at least 1 event time

## Value

a matrix of event times with one row per sampled point process.
