# Uniform random samples from `rstream` objects

Sample from `rstream` objects

## Usage

``` r
rng_stream_runif(size = 1, minimum = 0, maximum = 1, rng_stream = NULL)
```

## Arguments

- size:

  Integer, number of samples

- minimum:

  Lower bound

- maximum:

  Upper bound

- rng_stream:

  (`rstream`) an `rstream` object or `NULL`

## Value

a vector of uniform variates of size `size`

## Examples

``` r
rng_stream_runif(10)
#>  [1] 0.52610673 0.84416339 0.13599117 0.37646733 0.26330865 0.49785297
#>  [7] 0.01952525 0.08718381 0.99596266 0.94213221
```
