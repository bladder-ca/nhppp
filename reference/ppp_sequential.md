# Simulate a homogeneous Poisson Point Process over (t_min, t_max\]

**\[deprecated\]** Use `ppp` instead.

## Usage

``` r
ppp_sequential(
  range_t = c(0, 10),
  rate = 1,
  tol = 10^-6,
  rng_stream = NULL,
  atmost1 = FALSE
)
```

## Arguments

- range_t:

  (vector, double) min and max of the time interval

- rate:

  (scalar, double) constant instantaneous rate

- tol:

  the probability that we will have more than the drawn events in
  (t_min, t_max\]

- rng_stream:

  an `rstream` object

- atmost1:

  boolean, draw at most 1 event time

## Value

a vector of event times t if no events realize, it will have 0 length

## Examples

``` r
x <- ppp_sequential(range_t = c(0, 10), rate = 1, tol = 10^-6)
#> Warning: `ppp_sequential()` was deprecated in nhppp 0.5.0.
#> ℹ Please use `ppp()` instead.
```
