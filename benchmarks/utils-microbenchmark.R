# Parameters
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
  "ppp_t_orderstat (1st)" = ppp_t_orderstat(rate = hppp_rate, range_t = rngt, only1 = TRUE),
  "ppp_t_orderstat (All)" = ppp_t_orderstat(rate = hppp_rate, range_t = rngt),
  "nhppp_n_intensity_linear (1)" = nhppp_n_intensity_linear(size = 1, alpha = 0, beta = 2, range_t = rngt),
  "nhppp_n_intensity_linear (100)" = nhppp_n_intensity_linear(size = 100, alpha = 0, beta = 2, range_t = rngt),
  "nhppp_t_intensity_linear (1st)" = nhppp_t_intensity_linear(alpha = 0, beta = 2, range_t = rngt, only1 = TRUE),
  "nhppp_t_intensity_linear (All)" = nhppp_t_intensity_linear(alpha = 0, beta = 2, range_t = rngt),
  "nhppp_n_intensity (1, constmaj)" = nhppp_n_intensity(size = 1, lambda = l, lambda_maj = c(20, 0), range_t = rngt),
  "nhppp_n_intensity (100, constmaj)" = nhppp_n_intensity(size = 100, lambda = l, lambda_maj = c(20, 0), range_t = rngt),
  "nhppp_n_intensity (1, linemaj)" = nhppp_n_intensity(size = 1, lambda = l, lambda_maj = c(0, 2), range_t = rngt),
  "nhppp_n_intensity (100, linemaj)" = nhppp_n_intensity(size = 100, lambda = l, lambda_maj = c(0, 2), range_t = rngt),
  "nhppp_t_intensity (1st, constmaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(20, 0), range_t = rngt, only1 = TRUE),
  "nhppp_t_intensity (100, constmaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(20, 0), range_t = rngt),
  "nhppp_t_intensity (1st, linemaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(0, 2), range_t = rngt, only1 = TRUE),
  "nhppp_t_intensity (100, linemaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(0, 2), range_t = rngt),
  "nhppp_n_cumulative_intensity (1, worst)" = nhppp_n_cumulative_intensity(size = 1, Lambda = L, Lambda_inv = NULL, range_t = rngt),
  "nhppp_n_cumulative_intensity (100, worst)" = nhppp_n_cumulative_intensity(size = 100, Lambda = L, Lambda_inv = NULL, range_t = rngt),
  "nhppp_n_cumulative_intensity (1, best)" = nhppp_n_cumulative_intensity(size = 1, Lambda = L, Lambda_inv = Li, range_t = rngt),
  "nhppp_n_cumulative_intensity (100, best)" = nhppp_n_cumulative_intensity(size = 100, Lambda = L, Lambda_inv = Li, range_t = rngt),
  "nhppp_t_cumulative_intensity_inversion (1st, worst)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = NULL, range_t = rngt, only1 = TRUE),
  "nhppp_t_cumulative_intensity_inversion (All, worst)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = NULL, range_t = rngt),
  "nhppp_t_cumulative_intensity_inversion (1st, best)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = rngt, only1 = TRUE),
  "nhppp_t_cumulative_intensity_inversion (All, best)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = rngt),
  "nhppp_t_cumulative_intensity_orderstats (1st, worst)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = NULL, range_t = rngt, only1 = TRUE),
  "nhppp_t_cumulative_intensity_orderstats (All, worst)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = NULL, range_t = rngt),
  "nhppp_t_cumulative_intensity_orderstats (1st, best)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, range_t = rngt, only1 = TRUE),
  "nhppp_t_cumulative_intensity_orderstats (All, best)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, range_t = rngt),
  check = FALSE,
  iterations = nsim
)

ggplot2::autoplot(res, type = "violin")


# _t_ vs _n_ functions for non-recurrent events
inversion_n <- function() {
  nhppp_n_cumulative_intensity(size = 1, Lambda = L, range_t = rngt)
}
inversion_t <- function() {
  t_ <- vector(mode = "numeric")
  while (length(t_) < 1) {
    t_ <- nhppp_t_cumulative_intensity_inversion(Lambda = L, range_t = rngt, only1 = TRUE)
  }
  return(t_)
}
thinning_n <- function() {
  nhppp_n_intensity(size = 1, lambda = l, lambda_maj = c(0, 2), range_t = rngt)
}
thinning_t <- function() {
  t_ <- vector(mode = "numeric")
  while (length(t_) < 1) {
    t_ <- nhppp_t_intensity(lambda = l, lambda_maj = c(0, 2), range_t = rngt, only1 = TRUE)
  }
  return(t_)
}

bench::mark(
  "inversion_n" = inversion_n(),
  "inversion_t" = inversion_t(),
  "thinning_n" = thinning_n(),
  "thinning_t" = thinning_t(),
  check = FALSE, iterations = nsim
)
