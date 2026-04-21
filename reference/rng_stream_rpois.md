# Poisson random samples from `rstream` objects

Sample from `rstream` objects

## Usage

``` r
rng_stream_rpois(size = 1, lambda = 1, rng_stream = NULL)
```

## Arguments

- size:

  Integer, number of samples

- lambda:

  Positive number, the mean

- rng_stream:

  (`rstream`) an `rstream` object or `NULL`

## Value

a vector of counts of size `size`

## Examples

``` r
rng_stream_rpois(10)
#>  [1] 0 2 2 0 1 1 1 0 0 0
```
