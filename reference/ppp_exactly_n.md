# Simulate exactly `n` points from a homogeneous Poisson Point Process over (t_min, t_max\]

Simulate exactly `n` points from a homogeneous Poisson Point Process
over (t_min, t_max\]

## Usage

``` r
ppp_exactly_n(n, t_min, t_max)
```

## Arguments

- n:

  (int) the number of points to be simulated

- t_min:

  (double) the lower bound of the time interval

- t_max:

  (double) the upper bound of the time interval

## Value

a vector of event times of size `n`

## Examples

``` r
x <- ppp_exactly_n(n = 10, t_min = 0, t_max = 10)
```
