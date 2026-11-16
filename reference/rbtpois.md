# Truncated Poisson random samples (basic R)

Sample from a Poisson distribution conditional on the count being in
`[k_min, k_max]`, by inversion of the CDF restricted to the truncation
region (upper-tail parameterization, accurate when the region is deep in
the right tail). `k_min = 1, k_max = Inf` is the zero-truncated
distribution
([`rztpois()`](https://bladder-ca.github.io/nhppp/reference/rztpois.md)).

## Usage

``` r
rbtpois(n, lambda, k_min = 1, k_max = Inf)
```

## Arguments

- n:

  Integer, number of samples

- lambda:

  Positive number (scalar or vector), the mean of the original
  (untruncated) Poisson distribution

- k_min:

  Non-negative integer, the minimum count conditioned on

- k_max:

  Positive integer or `Inf`, the maximum count conditioned on

## Value

a vector of counts (all in `[k_min, k_max]`) of size `n`

## Examples

``` r
rbtpois(10, 1, k_min = 3)
#>  [1] 4 3 4 4 3 3 3 3 3 5
rbtpois(10, 1, k_min = 0, k_max = 2)
#>  [1] 1 0 0 1 0 1 0 1 0 0
```
