# Check the validity of a ppp vector.

Standard checks for a vector of ordered times. Check that the `times`
vector is sorted, has unique values, has all values in `[t_min, t_max]`,
and has length `size` (if applicable).

## Usage

``` r
check_ppp_vector_validity(
  times,
  t_min,
  t_max = NULL,
  size = NULL,
  atmost1 = FALSE,
  atleast1 = FALSE
)
```

## Arguments

- times:

  (vector, double) the times to be checked

- t_min:

  (double) the start of the time nterval

- t_max:

  (double) optional: the end of the time interval

- size:

  (double) optional: the size of the vector

- atmost1:

  (boolean) optional: at most one sample returned

- atleast1:

  (boolean) optional: at least one sample returned

## Value

None
