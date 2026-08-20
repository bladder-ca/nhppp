# nhppp (development version)

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

