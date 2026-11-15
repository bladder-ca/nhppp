# nhppp (development version)

* Unify the sc-step C++ kernels; separate reporting from count conditioning
  (report_first_K/report_last_K vs generate_at_least_K/generate_at_most_K);
  rename atmostB to budget_cap

New features — two orthogonal truncation option classes
- REPORTING (`report_first_K`, `report_last_K`; at most one may be set):
  return only the earliest / latest min(N, K) events of the realization.
  These are reporting truncations — the count law of the sampled process
  is unchanged. `atmost1` remains as the alias for `report_first_K = 1`.
- GENERATION conditioning (`generate_at_least_K`, `generate_at_most_K`;
  combinable with K1 <= K2): change the sampled law to X | K1 <= N <= K2.
  `generate_at_least_K = 1` alone is the zero-truncated case (`atleast1`
  remains as its alias); `generate_at_least_K = generate_at_most_K = K`
  conditions on exactly K events — which is NOT the same object as
  reporting the first K of an `N >= K` draw (the reported-first-K times
  are biased early relative to the exactly-K law). Conditioned sampling
  uses the exact order-statistics construction with a doubly-truncated
  Poisson count, drawn by inversion of the CDF restricted to the
  truncation region (upper-tail parameterization, numerically stable deep
  in the right tail; primitives `rbtpois()` in R and
  `rbtpois`/`rbtpois_vec` in C++). Any generation bound routes to the
  order-statistics kernels; without generation bounds the sequential
  inversion kernels are used (they cannot condition on the future total).
- `vdraw_cumulative_intensity()` supports the full option set: any
  generation bound switches from sequential inversion to the exact
  order-statistics construction on the `Lambda` scale (doubly-truncated
  Poisson count; the ascending uniforms on
  `(Lambda(t_min), Lambda(t_max))` are drawn as normalized Exp(1)
  spacings and mapped through `Lambda_inv`), and `report_last_K` is
  honored on all paths.
- The vectorized thinning samplers support both generation bounds by
  rejection resampling (proposals push only the lower bound into the
  majorizer; the accepted-count condition is verified per row). The
  scalar thinning samplers support only `generate_at_least_K = 1`.
- `budget_cap` replaces `atmostB` (soft-deprecated alias with a warning):
  it caps the computational event budget of the vectorized kernels and is
  an approximation knob (jointly with the `1 - tol` quantile bound), not
  an exact reporting or conditioning contract. It is now honored uniformly
  by all vectorized sc-step paths (the whole-range regular path used to
  ignore `atmostB` silently) and never truncates the conditioned count
  below `generate_at_least_K`.

Internals
- The eight sc-step C++ kernels (regular/general x plain/zt x
  whole-range/subinterval) are unified into two template algorithm cores
  over two grid policies (`src/sc_step_core.h`); whole-range sampling is
  the subinterval special case. Net deletion of ~300 lines with no
  performance loss: the whole-range paths are ~6x faster (the fixed-size
  blocked Exp(1) draw is replaced by lazy per-event draws), the
  zero-truncated paths ~1.5x faster, and the subinterval paths ~1.2x
  faster in the included benchmark (`benchmarks/bench_sc_step_unify.R`).
- The zero-truncated kernels now honor `tol` instead of a hard-coded
  `.99999` event-count quantile.
- Test infrastructure: the sample-validity helper checks matrices with
  whole-matrix operations instead of per-row testthat expectations
  (the suite drops from ~15 minutes to ~15 seconds on the 10^4-row
  distributional fixtures), and the distributional tests (Q-Q agreement,
  chi-square, and Kolmogorov-Smirnov assertions) are skipped on CRAN.
- Copy/allocation fixes: no per-event row materialization in the interval
  search, fused Lambda construction, no dead matrix pre-allocations,
  column trimming skips the copy when nothing is trimmed, 1-row
  `range_t`/`t_min`/`t_max` inputs are shared across rows instead of
  being replicated at the R level.
- New on-demand R-hub v2 workflow (`.github/workflows/rhub.yaml`) for
  sanitizer and valgrind checks. The unified kernels are clean under
  clang-ASan, gcc-ASan, and valgrind (the one valgrind finding is a small
  leak in the rstream dependency, not in nhppp code).

Structured argument containers for user functions
- The args containers (`lambda_args`, `Lambda_args`) now have two explicit
  channels: `shared` (a named list of row-invariant arguments of any type,
  stored once and never replicated across point processes) and `row_args`
  (a data.frame or data.table with one row per point process, validated
  for row count and automatically row-subset by the rejection loops). The
  container is passed as the user function's SECOND POSITIONAL argument,
  exactly as given except that `row_args` is already subset; the name of
  the second formal is the user's choice.
- Deprecations and compatibility: a flat list with neither recognized
  element keeps its released all-shared behavior with no warning (thinning
  family); the nested `vector_arguments` data.table convention is
  deprecated (mapped onto `row_args` semantics, warning once per call);
  `Lambda_inv_args` is deprecated — one `Lambda_args` container is
  delivered to both `Lambda` and `Lambda_inv`. For
  `vdraw_cumulative_intensity()`, flat/legacy containers keep the released
  named-argument call (`Lambda(t, Lambda_args = ...)`) with a deprecation
  warning; structured containers get the positional call. When no
  arguments are supplied at all, user functions are now called with a
  single argument (`Lambda(t)` instead of `Lambda(t, Lambda_args = NULL)`),
  so plain one-argument closures work everywhere.
- `row_args`/`vector_arguments` accept plain data.frames (the data.table
  requirement is dropped; data.tables are still accepted as-is, read-only,
  never coerced or copied).
- New vignette "How to write an intensity or cumulative intensity function
  for vectorized models": the calling contract, the structured container
  and its alignment idiom, majorizer construction, a worked
  thinning-vs-inversion example, and the anti-patterns.
- `get_step_majorizer()` gains `fun_args`, taking the same container with
  the same delivery convention, so the sampler's `lambda` can be used for
  majorizer construction without hand-wrapping.

Thinning (intensity) family
- New `vdraw_intensity_step()`: vectorized thinning sampler with piecewise
  constant majorizers over arbitrary interval bounds (`time_breaks`, as in
  `vdraw_sc_step()`); the target intensity remains an arbitrary vectorized
  R function. Generalizes `vdraw_intensity()`, which assumes equal-length
  intervals. Both share one C++ template core (`src/intensity_step_core.h`).
- Count conditioning on the vectorized thinning samplers
  (`vdraw_intensity()`, `vdraw_intensity_step()`): conditioning is by
  rejection — candidate realizations are proposed from the majorizer
  conditioned on at least K1 majorizer events (exact: only the lower bound
  may be pushed into the proposal), thinned, and rows whose accepted count
  falls outside `[K1, K2]` are resampled. The per-round acceptance
  probability degrades in the strictness of the bounds and in the
  looseness of the majorizer; there is no iteration cap. Special case
  `K1 = K2 = K` (exactly K events): only under-counts are rejected —
  rows with more than K survivors are salvaged exactly by keeping a
  uniformly random size-K subset of the events (conditionally-iid times
  make the subset an exact exactly-K draw regardless of the surviving
  count, with no knowledge of the integrated target intensity), avoiding
  the near-zero two-sided acceptance probability when the target
  integrated intensity is large relative to K. The scalar thinning
  samplers support only `generate_at_least_K = 1`.
- The thinning kernels now error when `lambda` exceeds the majorizer also
  on the conditioned path (the old R-only zero-truncated path silently
  capped acceptance probabilities above 1, sampling from the wrong
  process when the majorizer was invalid). The internal
  `vdraw_intensity_step_regular_forcezt()` is removed, superseded by the
  conditioned C++ kernel.

Long-format output for the vectorized samplers
- New `output = c("matrix", "long")` argument on the sc-step samplers
  (`vdraw_sc_step()`, `vztdraw_sc_step()`, `vdraw_sc_step_regular()` and
  the `_cpp` kernels) and on `vdraw_cumulative_intensity()` / `vdraw()`.
  `"long"` returns `list(id, time, n_draws)` with one entry per event:
  `id` is the 1-based point-process index (ascending, times ascending
  within id). A point process with zero events contributes no entries —
  its id is absent, and `n_draws` distinguishes "no events" from "not
  sampled"; no `NA` is used. The C++ paths build the long format directly
  (no dense intermediate; accumulator reserve()d from the expected
  `Lambda` mass), so it is preferred when event counts vary widely across
  point processes: in the included benchmark (10^6 processes, mean ~1
  event, max ~40), 404 MB dense vs 11.5 MB long, and ~30% faster. RNG
  consumption is identical across the two outputs, so a same-seed dense
  and long draw hold the same event values. On the conditioned
  `vdraw_cumulative_intensity()` path, `Lambda_inv` is called on the
  event vector with `row_args` subset to one row per event (aligned by
  id). The thinning samplers do not take `output` yet.
- New vignette "Long event output for large simulations" showing the two
  formats side by side, the no-event semantics, and the memory/time
  comparison as event-count heterogeneity grows.

Documentation
- Reference examples modernized to the current API: `draw()`, `vdraw()`,
  and `vdraw_cumulative_intensity()` gain examples (they had none); the
  structured args containers, the generation bounds (including exactly-K),
  the reporting truncations, and `get_step_majorizer(fun_args = )` are now
  demonstrated on the reference pages. Vignette code uses `budget_cap` and
  the reporting/generation vocabulary throughout.

Breaking change (random streams only)
- Same-seed results differ from 1.0.5.x for the vectorized sc-step,
  zero-truncated, and thinning samplers: the package guarantees the
  sampled process distribution, not the number or order of RNG calls.
  Users relying on common random numbers should block RNG streams
  themselves.

* Generalize vectorized sc-step samplers to arbitrary interval bounds; fix four latent bugs

New features
- Add vdraw_sc_step() and vztdraw_sc_step(): vectorized sampling from
  piecewise constant intensities where intervals need not be equally
  spaced. The time_breaks argument accepts a vector of K+1 bounds shared
  by all point processes, or an n x (K+1) matrix with one row of bounds
  per point process (a 1-row matrix is treated as shared and is not
  replicated in memory; the C++ kernels index row 0 directly).
- Both lambda_matrix (per-interval rates) and Lambda_matrix (cumulative
  at interval ends) forms are supported, as are t_min/t_max subintervals,
  atmost1, atmostB, and atleast1 (which routes to the zero-truncated
  sampler), matching the vdraw_sc_step_regular() interface.
- New vignette "Step intensities over arbitrary intervals" showcasing
  vdraw_sc_step() / vztdraw_sc_step() on standard demographic age bands,
  with a timing study against vdraw_sc_step_regular().
- Implementation: new C++ kernels (vdraw_sc_step_general[2],
  vztdraw_sc_step_general[2], step_general_inverse, find_break_interval)
  that mirror the *_regular kernels with the three regularity operations
  replaced: per-interval widths in the rate-to-cumulative conversion,
  binary search over breaks for time-to-interval lookup, and breaks-based
  index+fraction-to-time reconstruction. Existing *_regular kernels are
  untouched, so previously drawn seeds reproduce exactly.

Bug fixes (each with a regression test)
- vdraw_intensity_step_regular_cpp() / vdraw_intensity() / vdraw(): the
  thinning step looked up the majorizer in the wrong interval whenever
  the interval length differed from 1 (misplaced parenthesis in the index
  arithmetic), distorting the acceptance ratio or throwing "Majorizer
  error". Draws from these functions change (are now correct) when the
  interval length is not 1.
- vdraw_sc_step_regular_cpp() / vztdraw_sc_step_regular_cpp(): a
  subinterval bound equal to rate_matrix_t_max read past the end of the
  cumulative-intensity row (out-of-bounds read). Indices and fractions
  are now clamped; results on previously-correct inputs are unchanged.
- vdraw(): supplying only Lambda_maj_matrix now routes to the thinning
  sampler as documented instead of erroring (dispatch condition tested
  lambda_maj_matrix twice).
- vztdraw_intensity() (internal): range_t was not passed through to
  vztdraw_intensity_step_regular() and every call errored.

Tests and validation
- New test files cover shared and per-row breaks, subintervals, argument
  validation, seed reproducibility, rlecuyer streams, and cross-validation:
  same-seed agreement with the *_regular kernels on equal-spaced breaks,
  bitwise shared-vs-replicated-breaks equality, and distributional
  agreement with scalar draw_sc_step().
- Full suite: 36,367 assertions, 0 failures; R CMD check: 0 errors,
  0 warnings, 0 notes.


# nhppp 1.0.5 
* CRAN release

# nhppp 1.0.4 
* Corrected the language field in DESCRIPTION to `en`

# nhppp 1.0.3

# nhppp 1.0.2

# nhppp 1.0.1

* CRAN release
* fixed rare overflow error in `rztpois` where an explicit cast from `double` to `int` was used -- now we use `safe_double_to_int` to avoid overflow; this overflow resulted in a segmentation fault in x86 hardware and in silent failures in AArch64 hardware. This bug should not be relevant for most practical uses of the package.  

* added new citation for the paper by Trikalinos and Sereda (2024, <doi:10.1371/journal.pone.0311311>)


# nhppp 1.0.0

# nhppp 0.2.1

# nhppp 0.2.0

* CRAN release

# nhppp 0.1.4

* Function arguments are now more consistent.

* More extensive numerical checks.  

* Added C++ code for vectorized functions -- for computational speed and memory efficiency.  

# nhppp 0.1.3


* CRAN release

# nhppp 0.1.2

* Renamed functions to start from `draw` (instead of `nhppp_t`), `draw_zt` (instead of `ztnhppp_t`), and `vdraw` (instead of ending in `_vec()`) 

* Initial CRAN submission.

