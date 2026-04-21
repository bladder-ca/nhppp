# Generic function for simulating from NHPPPs given the intensity function.

Sample from NHPPPs given the intensity function This is a wrapper to the
package's specific functions, and thus somewhat slower. For
time-intensive simulations prefer one of the specific functions.

## Usage

``` r
draw_intensity(
  lambda,
  line_majorizer_intercept = NULL,
  line_majorizer_slope = NULL,
  line_majorizer_is_loglinear = FALSE,
  step_majorizer_vector = NULL,
  t_min = NULL,
  t_max = NULL,
  atmost1 = FALSE
)
```

## Arguments

- lambda:

  (function) the instantaneous rate

- line_majorizer_intercept:

  The intercept `alpha` of the
  [log](https://rdrr.io/r/base/Log.html)linear majorizer function:
  `alpha + beta * t` or `exp(alpha + beta * t)`

- line_majorizer_slope:

  The slope `beta` of the [log](https://rdrr.io/r/base/Log.html)linear
  majorizer function: `alpha + beta * t` or `exp(alpha + beta * t)`

- line_majorizer_is_loglinear:

  (boolean) if `TRUE` the majorizer is loglinear
  `exp(alpha + beta * t)`; if `FALSE` it is a linear function

- step_majorizer_vector:

  (vector, double) `K` constant majorizing rates, one per interval; all
  intervals are of equal length (regular)

- t_min:

  (double) the lower bound of the interval

- t_max:

  (double) the upper bound of the interval

- atmost1:

  boolean, draw at most 1 event time

## Value

a vector of event times
