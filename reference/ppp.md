# Simulate a homogeneous Poisson Point Process in (t_min, t_max\]

Simulate a homogeneous Poisson Point Process in (t_min, t_max\]

## Usage

``` r
ppp(rate, t_min, t_max, atmost1 = FALSE, tol = 10^-6)
```

## Arguments

- rate:

  (scalar, double) constant instantaneous rate

- t_min:

  (scalar, double) the lower bound of the time interval

- t_max:

  (scalar, double) the upper bound of the time interval

- atmost1:

  boolean, draw at most 1 event time

- tol:

  the probability that we will have more than the drawn events in
  (t_min, t_max\]

## Value

a vector of event times t if no events realize, it will have 0 length

## Examples

``` r
x <- ppp(rate = 1, t_min = 0, t_max = 10, tol = 10^-6)
```
