# Simulate `size` samples from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from (t_min, t_max) with linear intensity function

Sample zero-truncated NHPPP times from a linear intensity function using
the inversion method, optionally using an `rstream` generator

## Usage

``` r
ztdraw_sc_linear(intercept, slope, t_min, t_max, atmost1 = FALSE)
```

## Arguments

- intercept:

  (double) the intercept

- slope:

  (double) the slope

- t_min:

  (double) the lower bound of the time interval

- t_max:

  (double) the upper bound of the time interval

- atmost1:

  (boolean) draw 1 event time

## Value

a vector of at least 1 event times

## Examples

``` r
x <- ztdraw_sc_linear(intercept = 0, slope = 0.2, t_min = 0, t_max = 10)
```
