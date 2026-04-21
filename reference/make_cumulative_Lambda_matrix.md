# Helper function for the vectorized versions of sampling functions. Takes the usual ways that `lambda_mat` and `Lambda_mat` are specified and returns `Lambda_mat`.

Helper function for the vectorized versions of sampling functions. Takes
the usual ways that `lambda_mat` and `Lambda_mat` are specified and
returns `Lambda_mat`.

## Usage

``` r
make_cumulative_Lambda_matrix(
  Lambda_mat = NULL,
  lambda_mat = NULL,
  interval_duration = NULL
)
```

## Arguments

- Lambda_mat:

  a matrix of cumulative intensities or missing

- lambda_mat:

  a matrix of intensities or missing

- interval_duration:

  a vector with the same number of elements as the rows of `Lambda_mat`

## Value

A matrix (r x 2), row-expanded if needed
