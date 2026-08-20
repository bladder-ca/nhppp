## R CMD check results

0 errors | 0 warnings | 0 notes

Generalize vectorized sc-step samplers to arbitrary interval bounds; fix four latent bugs

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
