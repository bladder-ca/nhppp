# Return matrix with column-wise cumulative sum replacing cells larger than `ceil` with `NA`. No checks for arguments is done.

Return matrix with column-wise cumulative sum replacing cells larger
than `ceil` with `NA`. No checks for arguments is done.

## Usage

``` r
mat_cumsum_columns_with_vector_ceiling(X, ceil = Inf)
```

## Arguments

- X:

  (matrix)

- ceil:

  (vector or Inf) the set of ceilings to be applied, per row of `X`

## Value

matrix
