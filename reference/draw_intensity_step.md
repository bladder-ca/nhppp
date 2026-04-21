# Simulate from a non homogeneous Poisson Point Process (NHPPP) from (t0, t_max) (thinning method) with piecewise constant_majorizer

Sample NHPPP times using the thinning method

## Usage

``` r
draw_intensity_step(lambda, majorizer_vector, time_breaks, atmost1 = FALSE)
```

## Arguments

- lambda:

  (function) the instantaneous rate of the NHPPP. A continuous function
  of time.

- majorizer_vector:

  (scalar, double) `K` constant majorizing rates, one per interval

- time_breaks:

  (vector, double) `K+1` time points defining `K` intervals of constant
  rates: `[t_1 = range_t[1], t_2)`: the first interval `[t_k, t_{k+1})`:
  the `k`-th interval `[t_{K}, t_{K+1} = range_t[2])`: the `K`-th (last)
  interval

- atmost1:

  boolean, draw at most 1 event time

## Value

a vector of event times (t\_); if no events realize, a vector of length
0
