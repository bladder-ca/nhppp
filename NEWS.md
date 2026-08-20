# nhppp (development version)

* New functions `vdraw_sc_step()` and `vztdraw_sc_step()`: vectorized sampling from piecewise constant intensities over arbitrary (not necessarily equal-length) interval bounds. The `time_breaks` argument takes either a vector of `K+1` bounds shared by all point processes or a matrix with one row of bounds per point process. They generalize `vdraw_sc_step_regular()` and support the same `t_min`/`t_max` subinterval, `atmost1`, `atmostB`, and `atleast1` options.
* Bug fix in `vdraw_intensity_step_regular_cpp()` (and thus `vdraw_intensity()` / `vdraw()`): the thinning step looked up the majorizer in the wrong interval whenever the interval length differed from 1, distorting the acceptance ratio (or erroring with "Majorizer error"). Draws from these functions change (are now correct) when the interval length is not 1.
* Bug fix in the subinterval (`t_min`/`t_max`) code path of `vdraw_sc_step_regular_cpp()` and `vztdraw_sc_step_regular_cpp()`: a subinterval bound equal to `rate_matrix_t_max` read past the end of the cumulative intensity row (out-of-bounds read). Interval indices and fractions are now clamped; results on previously-correct inputs are unchanged.
* Bug fix in `vdraw()`: supplying only `Lambda_maj_matrix` (without `lambda_maj_matrix`) now routes to the thinning sampler as documented, instead of erroring.
* Bug fix in the internal `vztdraw_intensity()`: the `range_t` argument was not passed through correctly and every call errored.

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

