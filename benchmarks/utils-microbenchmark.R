# Parameters for high event rates (expected 100 events)
l <- function(x) 2 * x
L <- function(x) x^2
Li <- function(z) sqrt(z)
rngt <- c(0, 10)
hppp_rate <- 1
nsim <- 1000

res <- bench::mark(
  "ppp_next_n (1)" = ppp_next_n(n = 1, rate = hppp_rate, t_min = rngt[1]),
  "ppp_next_n (100)" = ppp_next_n(n = 100, rate = hppp_rate, t_min = rngt[1]),
  "ppp_n (1)" = ppp_n(size = 1, range_t = rngt),
  "ppp_n (100)" = ppp_n(size = 100, range_t = rngt),
  "ppp_t (1st)" = ppp_t(rate = hppp_rate, range_t = rngt, only1 = TRUE),
  "ppp_t (All)" = ppp_t(rate = hppp_rate, range_t = rngt),
  "ztppp_t (1st)" = ztppp_t(rate = hppp_rate, range_t = rngt, only1 = TRUE),
  "ztppp_t (All)" = ztppp_t(rate = hppp_rate, range_t = rngt),
  "ppp_t_orderstat (1st)" = ppp_t_orderstat(rate = hppp_rate, range_t = rngt, only1 = TRUE),
  "ppp_t_orderstat (All)" = ppp_t_orderstat(rate = hppp_rate, range_t = rngt),
  "nhppp_t_intensity_linear (1st)" = nhppp_t_intensity_linear(alpha = 0, beta = 2, range_t = rngt, only1 = TRUE),
  "nhppp_t_intensity_linear (All)" = nhppp_t_intensity_linear(alpha = 0, beta = 2, range_t = rngt),
  "ztnhppp_t_intensity_linear (1st)" = ztnhppp_t_intensity_linear(alpha = 0, beta = 2, range_t = rngt, only1 = TRUE),
  "ztnhppp_t_intensity_linear (All)" = ztnhppp_t_intensity_linear(alpha = 0, beta = 2, range_t = rngt),
  "nhppp_t_intensity (1st, constmaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(20, 0), range_t = rngt, only1 = TRUE),
  "nhppp_t_intensity (100, constmaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(20, 0), range_t = rngt),
  "nhppp_t_intensity (1st, linemaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(0, 2), range_t = rngt, only1 = TRUE),
  "nhppp_t_intensity (100, linemaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(0, 2), range_t = rngt),
  "ztnhppp_t_intensity (1st, constmaj)" = ztnhppp_t_intensity(lambda = l, lambda_maj = c(20, 0), range_t = rngt, only1 = TRUE),
  "ztnhppp_t_intensity (100, constmaj)" = ztnhppp_t_intensity(lambda = l, lambda_maj = c(20, 0), range_t = rngt),
  "ztnhppp_t_intensity (1st, linemaj)" = ztnhppp_t_intensity(lambda = l, lambda_maj = c(0, 2), range_t = rngt, only1 = TRUE),
  "ztnhppp_t_intensity (100, linemaj)" = ztnhppp_t_intensity(lambda = l, lambda_maj = c(0, 2), range_t = rngt),
  "nhppp_t_cumulative_intensity_inversion (1st, worst)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = NULL, range_t = rngt, only1 = TRUE),
  "nhppp_t_cumulative_intensity_inversion (All, worst)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = NULL, range_t = rngt),
  "nhppp_t_cumulative_intensity_inversion (1st, best)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = rngt, only1 = TRUE),
  "nhppp_t_cumulative_intensity_inversion (All, best)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = rngt),
  "nhppp_t_cumulative_intensity_orderstats (1st, worst)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = NULL, range_t = rngt, only1 = TRUE),
  "nhppp_t_cumulative_intensity_orderstats (All, worst)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = NULL, range_t = rngt),
  "nhppp_t_cumulative_intensity_orderstats (1st, best)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, range_t = rngt, only1 = TRUE),
  "nhppp_t_cumulative_intensity_orderstats (All, best)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, range_t = rngt),
  "ztnhppp_t_cumulative_intensity (1st, worst)" = ztnhppp_t_cumulative_intensity(Lambda = L, Lambda_inv = NULL, range_t = rngt, only1 = TRUE),
  "ztnhppp_t_cumulative_intensity (All, worst)" = ztnhppp_t_cumulative_intensity(Lambda = L, Lambda_inv = NULL, range_t = rngt),
  "ztnhppp_t_cumulative_intensity (1st, best)" = ztnhppp_t_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = rngt, only1 = TRUE),
  "ztnhppp_t_cumulative_intensity (All, best)" = ztnhppp_t_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = rngt),
  check = FALSE,
  iterations = nsim
)

ggplot2::autoplot(res, type = "violin")


# Parameters for LOW event rates (expected 0.01 events -- limit range to Li(0.01))
rngt <- c(0, Li(0.01))

# nhppp vs ztnhppp functions
ppp_t_loop <- function(rate = 1, range_t = c(0, 0.01)) {
  t_ <- vector(mode = "numeric")
  while (length(t_) == 0) {
    t_ <- ppp_t(rate = rate, range_t = range_t, only1 = TRUE)
  }
  return(t_)
}

nhppp_t_cumul_intensity_inversion_loop <- function() {
  t_ <- vector(mode = "numeric")
  while (length(t_) == 0) {
    t_ <- nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = rngt, range_L = L(rngt), only1 = TRUE)
  }
  return(t_)
}

nhppp_t_cumul_intensity_orderstats_loop <- function() {
  t_ <- vector(mode = "numeric")
  while (length(t_) == 0) {
    t_ <- nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, range_t = rngt, range_L = L(rngt), only1 = TRUE)
  }
  return(t_)
}

nhppp_t_intensity_loop <- function() {
  t_ <- vector(mode = "numeric")
  while (length(t_) == 0) {
    t_ <- nhppp_t_intensity(lambda = l, lambda_maj = c(0, 2), range_t = rngt, only1 = TRUE)
  }
  return(t_)
}

zt_res_low <- bench::mark(
  "01 ztppp_t" = ztppp_t(rate = 0.1, range_t = rngt, only1 = TRUE),
  "01 ppp_t loop" = ppp_t_loop(),
  "02 ztnhppp_t_cumulative_intensity" = ztnhppp_t_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = rngt, range_L = L(rngt), only1 = TRUE),
  "02 nhppp_t_cumulative_intensity_inversion loop" = nhppp_t_cumul_intensity_inversion_loop(),
  "02 nhppp_t_cumulative_intensity_orderstats loop" = nhppp_t_cumul_intensity_orderstats_loop(),
  "03 ztnhppp_t_intensity" = ztnhppp_t_intensity(lambda = l, lambda_maj = c(0, 2), range_t = rngt),
  "03 nhppp_t_intensity loop" = nhppp_t_intensity_loop(),
  check = FALSE, iterations = nsim
)

ggplot2::autoplot(zt_res_low, type = "violin")

# rng_ functions
bench::mark(
  "unif" = rng_stream_runif(size = 1, rng_stream = NULL),
  "exp" = rng_stream_rexp(size = 1, rate = 0.5, rng_stream = NULL),
  "pois" = rng_stream_rpois(size = 1, lambda = 0.5, rng_stream = NULL),
  check = FALSE, iterations = nsim
)

bench::mark(
  "U" = rng_stream_runif(size = 1, minimum = rngt[1], maximum = rngt[2], rng_stream = NULL),
  "sortintU" = sort.int(rng_stream_runif(size = 1, minimum = rngt[1], maximum = rngt[2], rng_stream = NULL)),
  "sortU" = sort(rng_stream_runif(size = 1, minimum = rngt[1], maximum = rngt[2], rng_stream = NULL)),
  "order+U" = c(1)[order(rng_stream_runif(size = 1, minimum = rngt[1], maximum = rngt[2], rng_stream = NULL))],
  "order+U shell" = c(1)[order(rng_stream_runif(size = 1, minimum = rngt[1], maximum = rngt[2], rng_stream = NULL), method = "shell")],
  check = FALSE, iterations = nsim
)
