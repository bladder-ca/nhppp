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
#>  [1] 0.777058518 0.031780320 1.217374616 1.852790795 0.133738117 0.009199867
#>  [7] 1.259315609 2.298262546 0.293562610 0.347437994
```
