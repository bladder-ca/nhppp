# Helper function for the vectorized versions of sampling functions. Takes the usual ways that `range_t` is specified (a 2-vector, a 1 x 2 or an r x 2 matrix) and returns a r x 2 matrix.

Helper function for the vectorized versions of sampling functions. Takes
the usual ways that `range_t` is specified (a 2-vector, a 1 x 2 or an r
x 2 matrix) and returns a r x 2 matrix.

## Usage

``` r
make_range_t_matrix(range_t, n_rows)
```

## Arguments

- range_t:

  a 2-vector, a 1 x 2 or an r x 2 matrix

- n_rows:

  the number of rows in the fully expanded matrix (`r`)

## Value

A matrix (r x 2), row-expanded if needed
