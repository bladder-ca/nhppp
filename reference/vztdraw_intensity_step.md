# Vectorized sampling from a K-truncated NHPPP given the intensity function, with piecewise constant majorizers over arbitrary interval bounds (R)

Thinning sampler conditional on the accepted event count lying in
`[generate_at_least_K, generate_at_most_K]` (`generate_at_least_K = 1`
alone is the zero-truncated process), with majorizer intervals of
arbitrary length (`time_breaks`, as in
[`vdraw_sc_step()`](https://bladder-ca.github.io/nhppp/reference/vdraw_sc_step.md)).

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
vztdraw_intensity_step(
  lambda = NULL,
  lambda_args = NULL,
  Lambda_maj_matrix = NULL,
  lambda_maj_matrix = NULL,
  time_breaks = NULL,
  t_min = NULL,
  t_max = NULL,
  tol = 10^-6,
  atmost1 = FALSE,
  report_first_K = NULL,
  report_last_K = NULL,
  generate_at_least_K = 1,
  generate_at_most_K = NULL,
  budget_cap = NULL
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

  (matrix) integrated majorizer intensity rates at the end of each
  interval

- lambda_maj_matrix:

  (matrix) majorizer intensity rates, one per interval

- time_breaks:

  (vector \| matrix) the bounds of the majorizer intervals. With `K`
  intervals (`K = ncol([Lambda|lambda]_maj_matrix)`), either a vector of
  `K+1` increasing values (the same bounds for all point processes) or a
  matrix with `K+1` columns whose i-th row holds the bounds for the i-th
  point process (a 1-row matrix is recycled for all point processes).

- t_min:

  (scalar \| vector \| column matrix) is the lower bound of a
  subinterval of `(time_breaks[, 1], time_breaks[, K+1]]`. If set, times
  are sampled from the subinterval.

- t_max:

  (scalar \| vector \| column matrix) the upper bound of a subinterval
  of `(time_breaks[, 1], time_breaks[, K+1]]`. If set, times are sampled
  from the subinterval.

- tol:

  (scalar, double) tolerance for the number of events

- atmost1:

  boolean, report at most 1 event time (alias for `report_first_K = 1`)

- report_first_K:

  `NULL` or a positive integer: report only the earliest K accepted
  event times (reporting truncation — the count law is unchanged). At
  most one of `report_first_K`/`report_last_K` may be set.

- report_last_K:

  `NULL` or a positive integer: report only the latest K accepted event
  times (ascending order; reporting truncation).

- generate_at_least_K:

  non-negative integer: condition on at least K accepted events. The
  default 1 is the zero-truncated process.

- generate_at_most_K:

  `NULL` or a positive integer: condition on at most K accepted events
  (rejection resampling on the upper bound).

- budget_cap:

  `NULL` or a positive integer: cap the computational event budget of
  the majorizer kernel (approximation knob).
