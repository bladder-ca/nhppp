# Zero-truncated Poisson random samples from `rstream` objects

Sample from `rstream` objects

## Usage

``` r
rng_stream_rztpois(size = 1, lambda = 1, rng_stream = NULL)
```

## Arguments

- size:

  Integer, number of samples

- lambda:

  Positive number, the mean of the original (untruncated) Poisson
  distribution

- rng_stream:

  (`rstream`) an `rstream` object or `NULL`

## Value

a vector of non zero counts of size `size`

## Examples

``` r
rng_stream_rztpois(10)
#>  [1] 1 1 1 1 2 1 1 3 2 1
```
