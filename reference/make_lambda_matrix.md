# Helper function for the vectorized versions of sampling functions. Takes the usual ways that `lambda_mat` and `Lambda_mat` are specified and returns `lambda_mat`.

Helper function for the vectorized versions of sampling functions. Takes
the usual ways that `lambda_mat` and `Lambda_mat` are specified and
returns `lambda_mat`.

## Usage

``` r
make_lambda_matrix(
  lambda_mat = NULL,
  Lambda_mat = NULL,
  interval_duration = NULL
)
```

## Arguments

- lambda_mat:

  a matrix of intensities or missing

- Lambda_mat:

  a matrix of cumulative intensities or missing

- interval_duration:

  a vector with the same number of elements as the rows of `Lambda_mat`

## Value

A matrix (r x 2), row-expanded if needed
