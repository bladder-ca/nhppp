# Simulate a homogeneous Poisson Point Process over (t_min, t_max\] (order statistics method)

Internal function – not to be exported. Same as `ppp` but uses the Order
Statistics algorithm.

## Usage

``` r
ppp2(rate, t_min, t_max, atmost1 = FALSE)
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

## Value

a vector of event times t if no events realize, it will have 0 length

## Examples

``` r
x <- ppp(rate = 1, t_min = 0, t_max = 10, tol = 10^-6)
```
