# Simulate from a non homogeneous Poisson Point Process (NHPPP) over an interval when you know the cumulative intensity and its inverse.

Sample NHPPP times using the inversion method

## Usage

``` r
draw_cumulative_intensity(Lambda, Lambda_inv, t_min, t_max, atmost1 = FALSE)
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

  boolean, draw at most 1 event time

## Value

a vector of event times (t\_); if no events realize, a vector of length
0
