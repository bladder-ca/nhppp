# Package index

## Functions by type

### PPP

Constant-intensity Poisson Point Processes

- [`ppp()`](https://bladder-ca.github.io/nhppp/reference/ppp.md) :
  Simulate a homogeneous Poisson Point Process in (t_min, t_max\]

- [`ppp_exactly_n()`](https://bladder-ca.github.io/nhppp/reference/ppp_exactly_n.md)
  :

  Simulate exactly `n` points from a homogeneous Poisson Point Process
  over (t_min, t_max\]

- [`ppp_next_n()`](https://bladder-ca.github.io/nhppp/reference/ppp_next_n.md)
  : Simulate n events from a homogeneous Poisson Point Process.

- [`ztppp()`](https://bladder-ca.github.io/nhppp/reference/ztppp.md) :
  Simulate a zero-truncated homogeneous Poisson Point Process over
  (t_min, t_max\]

### NHPPP wrapper functions

Non-homogeneous Poisson Point Processes wrapper functions

- [`draw()`](https://bladder-ca.github.io/nhppp/reference/draw.md) :
  Generic function for simulating from NHPPPs given the intensity
  function or the cumulative intensity function.
- [`vdraw()`](https://bladder-ca.github.io/nhppp/reference/vdraw.md) :
  Vectorized generic function for simulating from NHPPPs given the
  intensity function or the cumulative intensity function

### NHPPP from intensity function

Non-homogeneous Poisson Point Processes from intensity functions

- [`draw_intensity()`](https://bladder-ca.github.io/nhppp/reference/draw_intensity.md)
  : Generic function for simulating from NHPPPs given the intensity
  function.
- [`vdraw_intensity()`](https://bladder-ca.github.io/nhppp/reference/vdraw_intensity.md)
  : Vectorized sampling from a non homogeneous Poisson Point Process
  (NHPPP) from an interval (thinning method) with piecewise
  constant_majorizers (C++)

### NHPPP from cumulative intensity function

Non-homogeneous Poisson Point Processes from cumulative intensity
functions

- [`draw_cumulative_intensity()`](https://bladder-ca.github.io/nhppp/reference/draw_cumulative_intensity.md)
  : Simulate from a non homogeneous Poisson Point Process (NHPPP) over
  an interval when you know the cumulative intensity and its inverse.
- [`vdraw_cumulative_intensity()`](https://bladder-ca.github.io/nhppp/reference/vdraw_cumulative_intensity.md)
  : Vectorized simulation from a non homogeneous Poisson Point Process
  (NHPPP) from (t_min, t_max) given the cumulative intensity function
  and its inverse
- [`ztdraw_cumulative_intensity()`](https://bladder-ca.github.io/nhppp/reference/ztdraw_cumulative_intensity.md)
  : Simulate from a zero-truncated non homogeneous Poisson Point Process
  (zt-NHPPP) from (t_min, t_max) (order statistics method)

### NHPPP special cases

Non-homogeneous Poisson Point Processes from intensity functions

- [`draw_sc_linear()`](https://bladder-ca.github.io/nhppp/reference/draw_sc_linear.md)
  : Special case: Simulate from a non homogeneous Poisson Point Process
  (NHPPP) from (t_min, t_max) with linear intensity function (inversion
  method)

- [`draw_sc_loglinear()`](https://bladder-ca.github.io/nhppp/reference/draw_sc_loglinear.md)
  : Special case: Simulate from a non homogeneous Poisson Point Process
  (NHPPP) from (t_min, t_max) with log-linear intensity function
  (inversion method)

- [`draw_sc_step()`](https://bladder-ca.github.io/nhppp/reference/draw_sc_step.md)
  :

  Simulate a piecewise constant-rate Poisson Point Process over
  `(t_min, t_max]` (inversion method) The intervals need not have the
  same length.

- [`draw_sc_step_regular()`](https://bladder-ca.github.io/nhppp/reference/draw_sc_step_regular.md)
  : Sampling from NHPPPs with piecewise constant intensities with same
  interval lengths (non-vectorized)

- [`vdraw_sc_step_regular()`](https://bladder-ca.github.io/nhppp/reference/vdraw_sc_step_regular.md)
  : Vectorized sampling from NHPPPs with piecewise constant intensities
  with same interval lengths

- [`vdraw_sc_step_regular_cpp()`](https://bladder-ca.github.io/nhppp/reference/vdraw_sc_step_regular_cpp.md)
  : Vectorized sampling from NHPPPs with piecewise constant intensities
  with same interval lengths (C++)

- [`ztdraw_sc_linear()`](https://bladder-ca.github.io/nhppp/reference/ztdraw_sc_linear.md)
  :

  Simulate `size` samples from a zero-truncated non homogeneous Poisson
  Point Process (zt-NHPPP) from (t_min, t_max) with linear intensity
  function

- [`ztdraw_sc_loglinear()`](https://bladder-ca.github.io/nhppp/reference/ztdraw_sc_loglinear.md)
  : Simulate from a zero-truncated non homogeneous Poisson Point Process
  (zt-NHPPP) from (t_min, t_max) with a log-linear intensity function

- [`vztdraw_sc_step_regular_cpp()`](https://bladder-ca.github.io/nhppp/reference/vztdraw_sc_step_regular_cpp.md)
  : Vectorized sampling from zero-truncated NHPPPs with piecewise
  constant intensities with same interval lengths

### Auxiliary functions

Auxiliary functions

- [`get_step_majorizer()`](https://bladder-ca.github.io/nhppp/reference/get_step_majorizer.md)
  :

  Piecewise constant (step) majorizer for K-Lipschitz functions over an
  interval (vectorized over the `breaks` argument).
