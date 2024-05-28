
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nhppp <img src="man/figures/logo.png" align="right" width="130">

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/bladder-ca/nhppp/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bladder-ca/nhppp?branch=main)
[![R-CMD-check](https://github.com/bladder-ca/nhppp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bladder-ca/nhppp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

nhppp is a package for simulating events from one dimensional
nonhomogeneous Poisson point processes (NHPPPs). Its functions are based
on three algorithms that provably sample from a target NHPPP: the
time-transformation of a homogeneous Poisson process (of intensity one)
via the inverse of the integrated intensity function; the generation of
a Poisson number of order statistics from a fixed density function; and
the thinning of a majorizing NHPPP via an acceptance-rejection scheme.
It was developed to provide fast and memory efficient functions for
discrete event and statistical simulations. For a description of the
algorithms and a numerical comparison with other R packages, see
Trikalinos and Sereda (2024), accessible at
<https://arxiv.org/abs/2402.00358>.

## Installation

You can install the release version of nhppp from
[CRAN](https://cran.r-project.org) with:

``` r
install.packages("nhppp")
```

You can install the development version of nhppp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bladder-ca/nhppp")
```

## Example

These examples use the generic function `draw()`, which is a wrapper for
the packages specific functions. `draw()` is a non-vectorized function,
but `nhppp` includes vectorized functions that are fast and have small
memory footprint.

Consider the time varying intensity function
$\lambda(t) = e^{(0.2t)} (1 + \sin t)$, which is a sinusoidal intensity
function with an exponential amplitude. To draw samples over the
interval $(0, 6\pi]$ execute

``` r
l <- function(t) (1 + sin(t)) * exp(0.2 * t)
nhppp::draw(lambda = l, lambda_maj = l(6 * pi), range_t = c(0, 6 * pi)) |>
  head(n = 20)
#>  [1] 1.197587 1.238620 1.497499 1.713629 1.761914 2.256739 2.537528 3.622938
#>  [9] 5.822574 6.064265 6.645696 6.651551 6.684603 6.875765 6.891348 7.130680
#> [17] 7.446557 7.453139 7.545474 7.557381
```

where `lambda_maj` is a majorizer constant.

When available, the integrated intensity function
$\Lambda(t) = \int_0^t \lambda(s) \ ds$ and its inverse
$\Lambda^{-1}(z)$ result in faster simulation times. For this example,
$\Lambda(t) = \frac{e^{0.2t}(0.2 \sin t - \cos t)+1}{1.04} + \frac{e^{0.2t} - 1}{0.2}$;
$\Lambda^{-1}(z)$ is constructed numerically upfront (or can be
calculated numerically by the function, at a computational cost).

``` r
L <- function(t) {
  exp(0.2 * t) * (0.2 * sin(t) - cos(t)) / 1.04 +
    exp(0.2 * t) / 0.2 - 4.038462
}
Li <- stats::approxfun(x = L(seq(0, 6 * pi, 10^-3)), y = seq(0, 6 * pi, 10^-3), rule = 2)

nhppp::draw(Lambda = L, Lambda_inv = Li, range_t = c(0, 6 * pi)) |>
  head(n = 20)
#>  [1] 0.01152846 0.23558627 0.32924742 0.49921843 0.63509297 1.36677413
#>  [7] 2.38941548 3.19511655 3.28049866 4.62140995 5.96916564 6.37504015
#> [13] 6.68283108 6.76577784 7.12919141 7.29249262 7.38665270 7.92953383
#> [19] 7.94791744 7.96591106
```

## Function naming conventions

1.  All functions whose name start with `ppp` or `ztppp` sample from
    constant or piecewise constant intensity functions, as described
    below:

- Functions whose names start with `ppp_[sequential|orderstats]` sample
  event times in an interval with constant intensity functions with the
  sequential and order statistics algorithms.

- Function `ztppp()` samples one or more event times in an interval with
  constant intensity, i.e., from a zero-truncated Poisson process.

- Functions `ppp_n()` and `ppp_next_n()` sample `n` events in an
  interval and the next `n` event times after a time `t0`.

2.  All functions whose name starts with `draw` or `vdraw` sample from
    NHPPPs.

- Functions with names starting with `draw_zt` sample at least one event
  in the interval, i.e., from a zero-truncated NHPPP.

- Functions with names starting with
  `[draw|draw_zt]_intensity[_majorizer]` expect an intensity argument.
  The third part (`[_majorizer]`) denotes what, if any, majorizer
  function is used.

- Functions with names starting with
  `[draw|draw_zt]_cumulative_intensity[_algorithm]` expect a cumulative
  (integrated) intensity argument. The third part (`[_algorithm]`)
  denotes the algorithm used, if more than one algorithms are pertinent.

- Functions with names starting with `vdraw` are vectorized.
