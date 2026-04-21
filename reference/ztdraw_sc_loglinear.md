# Simulate from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from (t_min, t_max) with a log-linear intensity function

Sample zt-NHPPP times from an log-linear intensity function

## Usage

``` r
ztdraw_sc_loglinear(intercept, slope, t_min, t_max, atmost1 = FALSE)
```

## Arguments

- intercept:

  (double) the intercept in the exponent

- slope:

  (double) the slope in the exponent

- t_min:

  (double) the lower bound of the time interval

- t_max:

  (double) the upper bound of the time interval

- atmost1:

  boolean, 1 event time

## Value

a vector of at least 1 event times

## Examples

``` r
x <- ztdraw_sc_loglinear(intercept = 0, slope = 0.2, t_min = 0, t_max = 10)
```
