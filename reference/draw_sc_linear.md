# Special case: Simulate from a non homogeneous Poisson Point Process (NHPPP) from (t_min, t_max) with linear intensity function (inversion method)

Sample NHPPP times from a linear intensity function using the inversion
method, optionally using an `rstream` generator

## Usage

``` r
draw_sc_linear(intercept, slope, t_min, t_max, atmost1 = FALSE)
```

## Arguments

- intercept:

  (double) the intercept

- slope:

  (double) the slope

- t_min:

  (double) lower bound of the time interval

- t_max:

  (double) upper bound of the time interval

- atmost1:

  boolean, draw at most 1 event time

## Value

a vector of event times (t\_); if no events realize, a vector of length
0

## Examples

``` r
x <- draw_sc_linear(intercept = 0, slope = 0.2, t_min = 0, t_max = 10)
```
