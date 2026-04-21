# Numerically evaluate the inverse of a function at a specific point

Numerically evaluate the inverse of a function at a specific point

## Usage

``` r
inverse_with_uniroot(
  f = f,
  y,
  min_x = 0,
  max_x = 1,
  min_y = f(min_x),
  max_y = f(max_x)
)
```

## Arguments

- f:

  (function) the function to be inverted; must be continuous and
  increasing

- y:

  (scalar, double) the f(x)=y value in which to evaluate the inverse

- min_x:

  (scalar, double) the min of the domain of f()

- max_x:

  (scalar, double) the max of the domain of f()

- min_y:

  (scalar, double) the min in the range of f()

- max_y:

  (scalar, double) the max in the range of f()

## Value

(scalar, double) vector of x=f^(-1)(y): the inverted value

## Examples

``` r
inverse_with_uniroot(f = function(x) {
  2 * x
}, y = 0.5)
#> [1] 0.25
```
