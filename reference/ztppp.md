# Simulate a zero-truncated homogeneous Poisson Point Process over (t_min, t_max\]

Simulate a zero-truncated homogeneous Poisson Point Process over (t_min,
t_max\]

## Usage

``` r
ztppp(rate, t_min, t_max, atmost1 = FALSE)
```

## Arguments

- rate:

  (scalar, double) constant instantaneous rate

- t_min:

  (scalar, double) lower bound of the time interval

- t_max:

  (scalar, double) upper bound of the time interval

- atmost1:

  boolean, draw at most 1 event time

## Value

a vector of event times of size `size`

## Examples

``` r
x <- ztppp(t_min = 0, t_max = 10, rate = 0.001)
```
