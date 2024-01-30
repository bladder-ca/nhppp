
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nhppp

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/bladder-ca/nhppp-fast/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bladder-ca/nhppp-fast?branch=main)
[![R-CMD-check](https://github.com/bladder-ca/nhppp-fast/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bladder-ca/nhppp-fast/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

nhppp is a package for simulating events from one dimensional
nonhomogeneous Poisson point processes (NHPPPs). Its functions are based
on three algorithms that provably sample from a target NHPPP: the
time-transformation of a homogeneous Poisson process (of intensity one)
via the inverse of the integrated intensity function; the generation of
a Poisson number of order statistics from a fixed density function; and
the thinning of a majorizing NHPPP via an acceptance-rejection scheme.

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
devtools::install_github("bladder-ca/nhppp-fast")
```

## Example

These examples use the generic function `draw()`, which is a wrapper for
the packages specific functions.

Consider the time varying intensity function
$\lambda(t) = e^{(0.2t)} (1 + \sin t)$, which is a sinusoidal intensity
function with an exponential amplitude. To draw samples over the
interval $(0, 6\pi]$ execute

``` r
l <- function(t) (1 + sin(t)) * exp(0.2 * t)
nhppp::draw(lambda = l, lambda_maj = l(6 * pi), range_t = c(0, 6 * pi)) |>
  head(n = 20)
#>  [1] 0.5827159 0.6638516 0.9675573 2.0350054 2.5228102 4.0289535 5.7192824
#>  [8] 6.0371246 6.3575516 6.4217446 6.8861591 6.9845643 7.1247786 7.1699383
#> [15] 7.2168320 7.2404161 7.3152594 7.3727293 7.4948264 7.7708549
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
#>  [1] 0.2511866 2.1187543 2.3130020 2.5376045 2.9323540 3.0788802 5.4052197
#>  [8] 5.7215395 6.1030500 6.9713082 7.1461215 7.3420254 7.4309011 7.7615950
#> [15] 7.9495141 8.0699825 8.1007904 8.1147037 8.2458472 8.5601752
```
