## R CMD check results

0 errors | 0 warnings | 0 notes


* New functions `vdraw_sc_step()` and `vztdraw_sc_step()`: vectorized sampling from piecewise constant intensities over arbitrary (not necessarily equal-length) interval bounds. The `time_breaks` argument takes either a vector of `K+1` bounds shared by all point processes or a matrix with one row of bounds per point process. They generalize `vdraw_sc_step_regular()` and support the same `t_min`/`t_max` subinterval, `atmost1`, `atmostB`, and `atleast1` options.
* Bug fix in `vdraw_intensity_step_regular_cpp()` (and thus `vdraw_intensity()` / `vdraw()`): the thinning step looked up the majorizer in the wrong interval whenever the interval length differed from 1, distorting the acceptance ratio (or erroring with "Majorizer error"). Draws from these functions change (are now correct) when the interval length is not 1.
* Bug fix in the subinterval (`t_min`/`t_max`) code path of `vdraw_sc_step_regular_cpp()` and `vztdraw_sc_step_regular_cpp()`: a subinterval bound equal to `rate_matrix_t_max` read past the end of the cumulative intensity row (out-of-bounds read). Interval indices and fractions are now clamped; results on previously-correct inputs are unchanged.
* Bug fix in `vdraw()`: supplying only `Lambda_maj_matrix` (without `lambda_maj_matrix`) now routes to the thinning sampler as documented, instead of erroring.
* Bug fix in the internal `vztdraw_intensity()`: the `range_t` argument was not passed through correctly and every call errored.

