
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nhppp

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/bladder-ca/nhppp-fast/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bladder-ca/nhppp-fast?branch=main)
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
nhppp::draw(lambda = l, lambda_maj = l(6 * pi), range_t = c(0, 6 * pi))
#>   [1]  1.064960  1.595402  2.100743  2.542632  3.358505  5.475092  5.521760
#>   [8]  5.858285  5.998411  6.436609  6.707750  6.977239  7.127531  7.150527
#>  [15]  7.367805  7.497166  7.754861  7.844681  7.861922  8.214168  8.290124
#>  [22]  8.309641  8.502231  8.523581  8.558765  8.568053  8.592491  8.714425
#>  [29]  8.785833  8.824772  8.868751  8.929338  9.130669  9.249198  9.314506
#>  [36]  9.534210  9.540589  9.577871  9.701645 10.029827 10.719201 11.768085
#>  [43] 11.832671 12.070206 12.085061 12.206305 12.338336 12.469823 12.684206
#>  [50] 12.704018 12.725010 12.733041 12.857584 12.858576 12.896404 12.898071
#>  [57] 12.991666 13.038878 13.079500 13.118238 13.135391 13.178361 13.179216
#>  [64] 13.190271 13.200918 13.223004 13.305107 13.315254 13.331038 13.410145
#>  [71] 13.429553 13.539869 13.561645 13.563092 13.648286 13.681504 13.711539
#>  [78] 13.713841 13.717385 13.758280 13.792648 13.834611 13.856249 13.872166
#>  [85] 13.889947 13.930437 13.951744 13.953407 13.999864 14.020473 14.035516
#>  [92] 14.063015 14.101655 14.119661 14.189159 14.233603 14.236057 14.252304
#>  [99] 14.282838 14.286150 14.287775 14.300828 14.324690 14.375038 14.380971
#> [106] 14.388054 14.420926 14.433081 14.474367 14.488548 14.619001 14.624501
#> [113] 14.639273 14.674845 14.678038 14.681830 14.726626 14.730373 14.739230
#> [120] 14.740202 14.745074 14.790083 14.846473 14.848041 14.850758 14.888698
#> [127] 14.908853 15.004936 15.085179 15.086788 15.098175 15.123638 15.200095
#> [134] 15.243801 15.325472 15.348450 15.486182 15.557165 15.561906 15.584407
#> [141] 15.587385 15.594358 15.693069 15.745993 15.797455 15.822780 15.888534
#> [148] 15.907685 15.977519 15.987468 15.988003 16.030804 16.066859 16.100301
#> [155] 16.120140 16.210345 16.342824 16.345158 16.515282 16.781570 18.197822
#> [162] 18.426476 18.451409 18.538435 18.563433 18.579782 18.580163 18.609917
#> [169] 18.714610 18.744154 18.769832 18.770063 18.813402 18.817163
```

where `lambda_maj` is a majorizer constant.

When available, the integrated intensity function
$\Lambda(t) = \int_0^t \lambda(s) \ ds$ and it’s inverse
$\Lambda^{-1}(z)$ result in faster simulation times. For this example,
$\Lambda(t) = \frac{e^{0.2t}(0.2 \sin t - \cos t)+1}{1.04} + \frac{e^{0.2t} - 1}{0.2}$;
$\Lambda^{-1}(z)$ is constructed numerically upfront (or can be
calculated numerically by the function, at a computational cost).

``` r
L <- function(t) {
  exp(0.2 * t) * (0.2 * sin(t) - cos(t)) / 1.04 +
    exp(0.2 * t) / 0.2 - 4.038462
}
Li <- approxfun(x = L(seq(1, 6 * pi, 10^-3)), y = seq(1, 6 * pi, 10^-3))

nhppp::draw(Lambda = L, Lambda_inv = Li, range_t = c(0, 6 * pi))
#>   [1]        NA  1.990920  2.297027  2.823227  2.832055  2.884668  2.941870
#>   [8]  3.045445  3.687603  5.824552  6.027239  6.210375  6.530126  6.546756
#>  [15]  6.853342  6.893990  6.996867  7.034328  7.116359  7.128202  7.442570
#>  [22]  7.509743  7.657270  7.764892  7.912318  7.988593  8.119832  8.265519
#>  [29]  8.327765  8.332587  8.337515  8.469223  8.470108  8.480506  8.595345
#>  [36]  9.089966  9.247228  9.430244 10.122768 10.893894 11.938226 11.963476
#>  [43] 11.996182 12.094314 12.144179 12.438412 12.443097 12.514595 12.520496
#>  [50] 12.521400 12.561567 12.572026 12.685809 12.708805 12.928212 12.959562
#>  [57] 13.037501 13.063748 13.065107 13.145358 13.157510 13.185945 13.198009
#>  [64] 13.261055 13.269785 13.355005 13.394438 13.406013 13.468501 13.513666
#>  [71] 13.618269 13.672263 13.680322 13.719741 13.722952 13.785113 13.810465
#>  [78] 13.819484 13.826035 13.850436 13.897573 13.904499 13.950549 13.958794
#>  [85] 13.959389 13.962102 13.973247 14.085060 14.087784 14.102091 14.110544
#>  [92] 14.175740 14.187852 14.198677 14.209612 14.215272 14.218523 14.259207
#>  [99] 14.259992 14.260417 14.276881 14.277699 14.303922 14.310146 14.357214
#> [106] 14.369596 14.380664 14.414546 14.417071 14.418868 14.419881 14.421239
#> [113] 14.474670 14.508946 14.514630 14.563842 14.593097 14.621627 14.674944
#> [120] 14.679985 14.691470 14.795616 14.812189 14.813940 14.835910 14.838137
#> [127] 14.854641 14.873220 14.888929 14.973540 14.980465 14.993956 15.058785
#> [134] 15.064787 15.095545 15.112461 15.139402 15.236078 15.242238 15.245915
#> [141] 15.248027 15.260768 15.292970 15.306096 15.332614 15.334614 15.335064
#> [148] 15.336015 15.370645 15.417058 15.471494 15.472012 15.480180 15.498594
#> [155] 15.501544 15.536872 15.570472 15.687155 15.763100 16.044826 16.075584
#> [162] 16.083205 16.147180 16.186895 16.200763 16.227619 16.249194 16.259852
#> [169] 16.298277 16.338181 16.463328 16.495221 16.770173 16.904879 17.650163
#> [176] 17.697353 17.772910 18.012676 18.160482 18.194637 18.283201 18.291879
#> [183] 18.316388 18.341111 18.405540 18.442460 18.494138 18.498493 18.615621
#> [190] 18.717475 18.744075 18.745807 18.747474 18.748293
```
