# Simulate n events from a homogeneous Poisson Point Process.

Simulate n events from a homogeneous Poisson Point Process.

## Usage

``` r
ppp_next_n(n = 1, rate = 1, t_min = 0, rng_stream = deprecated())
```

## Arguments

- n:

  scalar number of samples

- rate:

  scalar instantaneous rate

- t_min:

  scalar for the starting time value

- rng_stream:

  **\[deprecated\]** an `rstream` object

## Value

a vector with event times t (starting from t_min)

## Examples

``` r
x <- ppp_next_n(n = 10, rate = 1, t_min = 0)
```
