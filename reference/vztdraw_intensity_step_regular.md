# Vectorized sampling from a K-truncated non homogeneous Poisson Point Process (NHPPP) from an interval (thinning method) with piecewise constant majorizers (R)

Vectorized thinning sampler conditional on the accepted event count
lying in `[generate_at_least_K, generate_at_most_K]`
(`generate_at_least_K = 1` alone, the default, is the zero-truncated
process). The majorizers are step functions over equal-length time
intervals.

Conditioning is by rejection: candidate realizations are proposed from
the majorizer conditioned on at least K1 majorizer events (only the
lower bound may be pushed into the proposal — at least K1 accepted
events implies at least K1 majorizer events, whereas an upper bound on
the majorizer count would over-restrict and bias the accepted law),
thinned against `lambda`, and rows whose surviving count falls outside
`[K1, K2]` are resampled until the condition holds. The per-round
acceptance probability degrades in the strictness of the bounds and in
the looseness of the majorizer, so a tight majorizer matters much more
here than in the unconditional sampler. There is no iteration cap.

Special case `K1 = K2 = K` (exactly K events): only under-counts are
rejected; rows with more than K survivors are salvaged by keeping a
uniformly random size-K subset of the events. This is exact — given the
count, event times are conditionally iid, so the subset is an exact draw
from the exactly-K law for every surviving count alike — and avoids the
near-zero acceptance probability of two-sided rejection when the target
integrated intensity is large relative to K.

## Usage

``` r
vztdraw_intensity_step_regular(
  lambda = NULL,
  lambda_args = NULL,
  Lambda_maj_matrix = NULL,
  lambda_maj_matrix = NULL,
  rate_matrix_t_min = NULL,
  rate_matrix_t_max = NULL,
  t_min = NULL,
  t_max = NULL,
  tol = 10^-6,
  atmost1 = FALSE,
  report_first_K = NULL,
  report_last_K = NULL,
  generate_at_least_K = 1,
  generate_at_most_K = NULL,
  ...
)
```

## Arguments

- lambda:

  (function) intensity function, vectorized

- lambda_args:

  (list) arguments for `lambda`, with up to two elements: `shared`
  (named list of row-invariant arguments of any type, stored once and
  never replicated) and `row_args` (data.frame or data.table with one
  row per point process, auto-subset when rows are resampled). When
  non-`NULL`, the container is passed as `lambda`'s second positional
  argument exactly as given, except that `row_args` is already
  row-subset; the name of the second formal is up to you. A flat list
  with neither element is treated as all-shared; a `vector_arguments`
  element is deprecated (use `row_args`).

- Lambda_maj_matrix:

  (matrix) integrated intensity rates at the end of each interval

- lambda_maj_matrix:

  (matrix) intensity rates, one per interval

- rate_matrix_t_min:

  (scalar \| vector \| column matrix) is the lower bound of the time
  interval for each row of (Lambda\|lambda)\_maj_matrix. The length of
  this argument is the number of point processes that should be drawn.

- rate_matrix_t_max:

  (scalar \| vector \| column matrix) the upper bound of the time
  interval for each row of (Lambda\|lambda)\_maj_matrix. The length of
  this argument is the number of point processes that should be drawn.

- t_min:

  (scalar \| vector \| column matrix) is the lower bound of a
  subinterval of (rate_matrix_t_min, rate_matrix_t_max\]. If set, times
  are sampled from the subinterval. If omitted, it is equivalent to
  `rate_matrix_t_min`.

- t_max:

  (scalar \| vector \| column matrix) is the upper bound of a
  subinterval of (rate_matrix_t_min, rate_matrix_t_max\]. If set, times
  are sampled from the subinterval. If omitted, it is equivalent to
  `rate_matrix_t_max`.

- tol:

  (scalar, double) tolerance for the number of events

- atmost1:

  boolean, report at most 1 event time of the conditioned process (alias
  for `report_first_K = 1`)

- report_first_K:

  `NULL` or a positive integer: report only the earliest K event times
  of the conditioned realization.

- report_last_K:

  `NULL` or a positive integer: report only the latest K event times of
  the conditioned realization (ascending order).

- generate_at_least_K:

  non-negative integer: condition on at least K accepted events. The
  default 1 is the zero-truncated process.

- generate_at_most_K:

  `NULL` or a positive integer: condition on at most K accepted events
  (rejection on the upper bound).

- ...:

  absorbed (for wrapper compatibility)
