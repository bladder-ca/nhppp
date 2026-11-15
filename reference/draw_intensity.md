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
  atmost1 = FALSE,
  report_first_K = NULL,
  report_last_K = NULL,
  atleast1 = FALSE,
  generate_at_least_K = NULL,
  generate_at_most_K = NULL
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

  boolean, report at most 1 event time (alias for `report_first_K = 1`)

- report_first_K:

  `NULL` or a positive integer: report only the earliest K event times
  (reporting truncation — the count law is unchanged).

- report_last_K:

  `NULL` or a positive integer: report only the latest K event times
  (ascending order; reporting truncation).

- atleast1:

  boolean, condition on at least 1 event (alias for
  `generate_at_least_K = 1`)

- generate_at_least_K:

  `NULL` or `1`: condition the sampled process on at least 1 event.
  Larger values are not implemented for the scalar thinning sampler and
  raise an error.

- generate_at_most_K:

  must be `NULL`; conditioning on at most K events is not implemented
  for the scalar thinning sampler.

## Value

a vector of event times
