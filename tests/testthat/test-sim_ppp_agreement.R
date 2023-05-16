test_that("PPP methods agree on the first time to event", {
  r_ppp_next_n <- unlist(lapply(integer(10000), function(x) ppp_next_n(n = 1, rate = 10, t_min = 1)))
  r_ppp_t <- unlist(lapply(integer(10000), function(x) ppp_t(range_t = c(1, 3), rate = 10, only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ppp_next_n, ppp2 = r_ppp_t, threshold = 0.1, showQQ = TRUE)

  r_ppp_t_orderstat <- unlist(lapply(integer(10000), function(x) ppp_t_orderstat(range_t = c(1, 3), rate = 10, only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ppp_next_n, ppp2 = r_ppp_t_orderstat, threshold = 0.1, showQQ = TRUE)
})


test_that("NHPPP methods agree on the first time to event with constant rate", {
  l <- function(t) 2
  L <- function(t) 2 * t
  Li <- function(z) z / 2

  r_ppp_t <- unlist(lapply(integer(10000), function(x) ppp_t(range_t = c(1, 13), rate = 2, only1 = TRUE)))
  r_nhppp_ci_inv <- unlist(lapply(integer(10000), function(x) nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_ci_inv, ppp2 = r_ppp_t, threshold = 0.1, showQQ = TRUE)

  r_nhppp_ci_orderstats <- unlist(lapply(integer(10000), function(x) nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_ci_orderstats, ppp2 = r_ppp_t, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens_linear <- unlist(lapply(integer(10000), function(x) nhppp_t_intensity_linear(alpha = 2, beta = 0, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_linear, ppp2 = r_ppp_t, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens_exp <- unlist(lapply(integer(10000), function(x) nhppp_t_intensity_exponential(alpha = log(2), beta = 0, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_exp, ppp2 = r_ppp_t, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens <- unlist(lapply(integer(10000), function(x) nhppp_t_intensity(lambda = l, lambda_maj = 2.1, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens, ppp2 = r_ppp_t, threshold = 0.1, showQQ = TRUE)
})

test_that("NHPPP linear intensity agrees with general methods", {
  l <- function(t, alpha = 1, beta = 2) alpha + beta * t
  L <- function(t, alpha = 1, beta = 2, t0 = 1) Lambda_linear_form(t, alpha = alpha, beta = beta, t0 = t0)
  Li <- function(z, alpha = 1, beta = 2, t0 = 1) Lambda_inv_linear_form(z, alpha = alpha, beta = beta, t0 = t0)

  r_nhppp_intens_linear <- unlist(lapply(integer(10000), function(x) nhppp_t_intensity_linear(alpha = 1, beta = 2, range_t = c(1, 13), only1 = TRUE)))
  r_nhppp_ci_inv <- unlist(lapply(integer(10000), function(x) nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_linear, ppp2 = r_nhppp_ci_inv, threshold = 0.1, showQQ = TRUE)

  r_nhppp_ci_os <- unlist(lapply(integer(10000), function(x) nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_linear, ppp2 = r_nhppp_ci_os, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens <- unlist(lapply(integer(10000), function(x) nhppp_t_intensity(lambda = l, lambda_maj = l(13), range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_linear, ppp2 = r_nhppp_intens, threshold = 0.1, showQQ = TRUE)
})

test_that("NHPPP exponential agrees with general methods", {
  l <- function(t, alpha = .1, beta = .02) exp(alpha + beta * t)
  L <- function(t, alpha = .1, beta = .02, t0 = 1) Lambda_exp_form(t, alpha = alpha, beta = beta, t0 = t0)
  Li <- function(z, alpha = .1, beta = .02, t0 = 1) Lambda_inv_exp_form(z, alpha = alpha, beta = beta, t0 = t0)

  r_nhppp_intens_exp <- unlist(lapply(integer(10000), function(x) nhppp_t_intensity_exponential(alpha = .1, beta = .02, range_t = c(1, 13), only1 = TRUE)))
  r_nhppp_ci_inv <- unlist(lapply(integer(10000), function(x) nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_exp, ppp2 = r_nhppp_ci_inv, threshold = 0.1, showQQ = TRUE)

  r_nhppp_ci_os <- unlist(lapply(integer(10000), function(x) nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_exp, ppp2 = r_nhppp_ci_os, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens <- unlist(lapply(integer(10000), function(x) nhppp_t_intensity(lambda = l, lambda_maj = l(13), range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_exp, ppp2 = r_nhppp_intens, threshold = 0.1, showQQ = TRUE)
})
