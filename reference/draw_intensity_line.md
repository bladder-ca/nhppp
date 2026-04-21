# Simulate from a non homogeneous Poisson Point Process (NHPPP) from (t0, t_max) (thinning method)

Sample NHPPP times using the thinning method

## Usage

``` r
draw_intensity_line(
  lambda,
  majorizer_intercept,
  majorizer_slope,
  t_min,
  t_max,
  majorizer_is_loglinear = FALSE,
  atmost1 = FALSE
)
```

## Arguments

- lambda:

  (function) the instantaneous rate of the NHPPP.

- majorizer_intercept:

  (double) the intercept (`alpha`) of the
  [log](https://rdrr.io/r/base/Log.html)linear majorizer function.

- majorizer_slope:

  (double) the slope (\`beta') of the
  [log](https://rdrr.io/r/base/Log.html)linear majorizer function.

- t_min:

  (double) the lower bound of the time interval.

- t_max:

  (double) the upper bound of the time interval.

- majorizer_is_loglinear:

  (boolean) if `TRUE` the majorizer is loglinear `exp(alpha + beta * t)`

- atmost1:

  boolean, draw at most 1 event time

## Value

a vector of event times (t\_); if no events realize, a vector of length
0
