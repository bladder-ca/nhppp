# Numerically evaluate the inverse of a monotonically increasing continuous function from R to R at specific points.

Numerically evaluate the inverse of a monotonically increasing
continuous function from R to R at specific points.

## Usage

``` r
inverse_with_uniroot_sorted(
  f,
  y,
  range_x = c(0, 10),
  range_y = c(f(range_x[1]), f(range_x[2]))
)
```

## Arguments

- f:

  (function) the function to be inverted; must be continuous and
  increasing

- y:

  (vector, double) the f(x)=y values in which to evaluate the inverse;
  must be in ascending order

- range_x:

  (vector, double) the min and max of the domain of f()

- range_y:

  (vector, double) the min and max in the range of f()

## Value

(vector, double) vector of x=f^(-1)(y): the inverted values

## Examples

``` r
inverse_with_uniroot_sorted(f = function(x) {
  2 * x
}, y = c(0, 0.5))
#> [1] 0.00 0.25
```
