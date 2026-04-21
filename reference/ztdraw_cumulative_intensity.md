# Simulate from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from (t_min, t_max) (order statistics method)

Sample zero-truncated NHPPP times using the order statistics method,
optionally using an `rstream` generator

## Usage

``` r
ztdraw_cumulative_intensity(Lambda, Lambda_inv, t_min, t_max, atmost1 = FALSE)
```

## Arguments

- Lambda:

  (function, double vector) a continuous increasing R to R map which is
  the integrated rate of the NHPPP

- Lambda_inv:

  (function, double vector) the inverse of `Lambda()`

- t_min:

  (double) the lower bound of the time interval

- t_max:

  (double) the upper bound of the time interval

- atmost1:

  (boolean) draw at most 1 event time

## Value

a vector of at least 1 event times
