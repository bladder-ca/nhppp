# Generic function for simulating from NHPPPs given the intensity function or the cumulative intensity function.

This is a wrapper to the package's specific functions, and thus somewhat
slower. For time-intensive simulations prefer one of the specific
functions.

## Usage

``` r
draw(
  Lambda = NULL,
  Lambda_inv = NULL,
  lambda = NULL,
  line_majorizer_intercept = NULL,
  line_majorizer_slope = NULL,
  line_majorizer_is_loglinear = FALSE,
  step_majorizer_vector = NULL,
  t_min = NULL,
  t_max = NULL,
  atmost1 = FALSE,
  report_first_K = NULL,
  report_last_K = NULL,
  atleast1 = FALSE,
  generate_at_least_K = NULL,
  generate_at_most_K = NULL
)
```

## Arguments

- Lambda:

  (function, double vector) the integrated (cumulative) rate of the
  NHPPP

- Lambda_inv:

  (function, double vector) the inverse of \`Lambda()'

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

  boolean, report at most 1 event time (alias for `report_first_K = 1`)

- report_first_K:

  `NULL` or a positive integer: report only the earliest K event times
  (reporting truncation).

- report_last_K:

  `NULL` or a positive integer: report only the latest K event times
  (ascending order; reporting truncation).

- atleast1:

  boolean, condition on at least 1 event (alias for
  `generate_at_least_K = 1`)

- generate_at_least_K:

  `NULL` or a positive integer: condition the sampled process on at
  least K events. With `lambda` (thinning), only
  `generate_at_least_K = 1` is implemented.

- generate_at_most_K:

  `NULL` or a positive integer: condition the sampled process on at most
  K events. Not implemented with `lambda` (thinning).

## Value

a vector of event times

## Examples

``` r
# thinning, with a constant majorizer line
x <- draw(
  lambda = function(t) 1 + sin(t),
  line_majorizer_intercept = 2,
  line_majorizer_slope = 0,
  t_min = 0, t_max = 10
)

# inversion, conditioned on at least one event
x <- draw(
  Lambda = function(t) 2 * t,
  Lambda_inv = function(z) z / 2,
  t_min = 0, t_max = 10,
  atleast1 = TRUE
)
```
