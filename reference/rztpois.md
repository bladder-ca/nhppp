# Zero-truncated Poisson random samples (basic R)

Sample zero-truncated Poisson random samples (basic R)

## Usage

``` r
rztpois(n, lambda)
```

## Arguments

- n:

  Integer, number of samples

- lambda:

  Positive number, the mean of the original (untruncated) Poisson
  distribution

## Value

a vector of non zero counts of size `n`

## Examples

``` r
rztpois(10, 1)
#>  [1] 1 1 1 1 1 1 1 1 2 4
rztpois(10, 1:10)
#>  [1] 1 1 1 4 6 6 9 8 9 4
```
