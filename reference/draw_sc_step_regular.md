# Sampling from NHPPPs with piecewise constant intensities with same interval lengths (non-vectorized)

Sampling from NHPPPs with piecewise constant intensities with same
interval lengths (non-vectorized)

## Usage

``` r
draw_sc_step_regular(
  Lambda_vector = NULL,
  lambda_vector = NULL,
  t_min = NULL,
  t_max = NULL,
  atmost1 = FALSE,
  atleast1 = FALSE
)
```

## Arguments

- Lambda_vector:

  (scalar, double) `K` integrated intensity rates at the end of each
  interval

- lambda_vector:

  (scalar, double) `K` constant intensity rates, one per interval

- t_min:

  (scalar, double) lower bound of the time interval

- t_max:

  (scalar, double) upper bound of the time interval

- atmost1:

  boolean, draw at most 1 event time

- atleast1:

  boolean, draw at least 1 event time

## Value

a vector of event times t if no events realize, it will have 0 length

## Examples

``` r
x <- draw_sc_step_regular(Lambda_vector = 1:5, t_min = 0, t_max = 5)
```
