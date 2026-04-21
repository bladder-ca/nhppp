# Simulate specific number of points from a homogeneous Poisson Point Process over (t_min, t_max\]

**\[deprecated\]** Use `ppp_exactly_n` instead.

## Usage

``` r
ppp_n(size, range_t = c(0, 10), rng_stream = NULL)
```

## Arguments

- size:

  (int) the number of points to be simulated

- range_t:

  (vector, double) min and max of the time interval

- rng_stream:

  an `rstream` object

## Value

a vector of event times of size `size`

## Examples

``` r
x <- ppp_n(size = 10, range_t = c(0, 10))
#> Warning: `ppp_n()` was deprecated in nhppp 0.5.0.
#> ℹ Please use `ppp_exactly_n()` instead.
```
