# Piecewise constant (step) majorizer for K-Lipschitz functions over an interval (vectorized over the `breaks` argument).

Return a piecewise constant (step) majorizer for K-Lipschitz functions
over an interval. The function is vectorized over the `breaks` argument.
The returned object has the same dimensions as `breaks`.

## Usage

``` r
get_step_majorizer(fun, breaks, is_monotone = TRUE, K = 0, fun_args = NULL)
```

## Arguments

- fun:

  A function object with a single argument `x`. If `x` is a matrix,
  `fun` should be vectorized over it.

- breaks:

  (vector or matrix) The set of `M+1` boundaries for the `M`
  subintervals in `x`. If breaks is a matrix, each row is treated as a
  separate set of breaks.

- is_monotone:

  (boolean) Is the function monotone? (Default is `TRUE`.)

- K:

  (double) A non-negative number for the Lipschitz cone. (Default is 0.)

- fun_args:

  (list) arguments for `fun`, with up to two elements: `shared` (named
  list of row-invariant arguments) and `row_args` (data.frame or
  data.table with one row per row of `breaks`). When non-`NULL`, the
  container is passed as `fun`'s second positional argument — the same
  convention (and the same `lambda`) as the vectorized samplers.

## Value

A vector of length `M` with the values of the piecewise constant
majorizer

## Examples

``` r
get_step_majorizer(fun = abs, breaks = -5:5, is_monotone = FALSE, K = 1)
#>  [1] 5.5 4.5 3.5 2.5 1.5 1.5 2.5 3.5 4.5 5.5

# a lambda that takes the structured args container
get_step_majorizer(
  fun = function(t, a) a$shared$scale * abs(t),
  breaks = matrix(rep(-5:5, each = 3), nrow = 3),
  is_monotone = FALSE, K = 2,
  fun_args = list(shared = list(scale = 2))
)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]   11    9    7    5    3    3    5    7    9    11
#> [2,]   11    9    7    5    3    3    5    7    9    11
#> [3,]   11    9    7    5    3    3    5    7    9    11
```
