# Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method) The intervals need not have the same length.

Simulate a piecewise constant-rate Poisson Point Process over
`(t_min, t_max]` (inversion method) The intervals need not have the same
length.

## Usage

``` r
draw_sc_step(lambda_vector, time_breaks, atmost1 = FALSE, atleast1 = FALSE)
```

## Arguments

- lambda_vector:

  (scalar, double) `K` constant rates, one per interval

- time_breaks:

  (vector, double) `K+1` time points defining `K` intervals of constant
  rates: `[t_1 = range_t[1], t_2)`: the first interval `[t_k, t_{k+1})`:
  the `k`-th interval `[t_{K}, t_{K+1} = range_t[2])`: the `K`-th (last)
  interval

- atmost1:

  boolean, draw at most 1 event time

- atleast1:

  boolean, draw at least 1 event time

## Value

a vector of event times t if no events realize, it will have 0 length

## Examples

``` r
x <- draw_sc_step(lambda_vector = rep(1, 5), time_breaks = c(0:5))
```
