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
#>  [1] 0.9931181 0.1163092 0.7154231 0.2261087 0.1333485 0.9272818 0.8941227
#>  [8] 0.2036644 0.2569829 0.6140056
```
