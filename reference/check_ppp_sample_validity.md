# Check the validity of ppp samples arranged in matrix format

Standard checks for a matrix of ordered times (event series in rows,
times in columns). Check that the times in the columns are sorted, have
unique values in `[t_min, t_max]`, and has length `size` (if
applicable).

## Usage

``` r
check_ppp_sample_validity(
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

  (vector, double \| matrix) the times to be checked as vectors or
  matrices (time-vectors in rows)

- t_min:

  (double \| vector) the start of the time nterval

- t_max:

  (double\| vector) optional: the end of the time interval; if a vector,
  its length should match the number of rows of `times`.

- size:

  (double) optional: the size of the vector

- atmost1:

  (boolean) optional: at most one sample returned

- atleast1:

  (boolean) optional: at least one sample returned

## Value

None
