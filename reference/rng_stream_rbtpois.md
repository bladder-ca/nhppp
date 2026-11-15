# Truncated Poisson random samples from `rstream` objects

Sample from `rstream` objects a Poisson count conditional on being in
`[k_min, k_max]` (see
[`rbtpois()`](https://bladder-ca.github.io/nhppp/reference/rbtpois.md)).

## Usage

``` r
rng_stream_rbtpois(
  size = 1,
  lambda = 1,
  k_min = 1,
  k_max = Inf,
  rng_stream = NULL
)
```

## Arguments

- size:

  Integer, number of samples

- lambda:

  Positive number, the mean of the original (untruncated) Poisson
  distribution

- k_min:

  Non-negative integer, the minimum count conditioned on

- k_max:

  Positive integer or `Inf`, the maximum count conditioned on

- rng_stream:

  (`rstream`) an `rstream` object or `NULL`

## Value

a vector of counts (all in `[k_min, k_max]`) of size `size`

## Examples

``` r
rng_stream_rbtpois(10, lambda = 1, k_min = 2)
#>  [1] 2 4 2 2 3 3 2 3 2 2
```
