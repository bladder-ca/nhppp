# Exponential random samples from `rstream` objects

Sample from `rstream` objects

## Usage

``` r
rng_stream_rexp(size = 1, rate = 1, rng_stream = NULL)
```

## Arguments

- size:

  Integer, number of samples

- rate:

  Positive number, the rate (i.e., 1/mean)

- rng_stream:

  (`rstream`) an `rstream` object or `NULL`

## Value

a vector of exponential variates of size `size`

## Examples

``` r
rng_stream_rexp(10)
#>  [1] 0.23960932 9.15313311 0.81848123 0.00278538 0.10580041 1.83457146
#>  [7] 0.26476943 0.68774057 0.45952242 3.00186797
```
