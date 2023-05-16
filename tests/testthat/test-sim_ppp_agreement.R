test_that("PPP methods agree on the first time to event", {

  r_ppp_next_n <- unlist(lapply(integer(10000), function(x) ppp_next_n(n = 1, rate = 10, t_min = 1)))
  r_ppp_t <- unlist(lapply(integer(10000), function(x) ppp_t(range_t = c(1, 3), rate = 10, only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ppp_next_n, ppp2 = r_ppp_t, threshold = 0.1, showQQ = TRUE)

  # because the expected number of events is 20, the zero-truncation is not in effect
  r_ztppp_t <- unlist(lapply(integer(10000), function(x) ztppp_t(range_t = c(1, 3), rate = 10, only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ppp_next_n, ppp2 = r_ztppp_t, threshold = 0.1, showQQ = TRUE)

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

  r_nhppp_intens <- unlist(lapply(integer(10000), function(x) nhppp_t_intensity(lambda = l, lambda_maj = 2.1, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens, ppp2 = r_ppp_t, threshold = 0.1, showQQ = TRUE)
})


test_that("zt-NHPPP methods agree on the first time to event with constant rate", {
  l <- function(t) 2
  L <- function(t) 2 * t
  Li <- function(z) z / 2 

  r_ztppp_t <- unlist(lapply(integer(10000), function(x) ztppp_t(range_t = c(1, 13), rate = 2, only1 = TRUE)))
  r_ztnhppp_ci <- unlist(lapply(integer(10000), function(x) ztnhppp_t_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_ci, ppp2 = r_ztppp_t, threshold = 0.1, showQQ = TRUE)

  r_ztnhppp_intens_linear <- unlist(lapply(integer(10000), function(x) ztnhppp_t_intensity_linear(alpha = 2, beta = 0, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens_linear, ppp2 = r_ztppp_t, threshold = 0.1, showQQ = TRUE)

  r_ztnhppp_intens <- unlist(lapply(integer(10000), function(x) ztnhppp_t_intensity(lambda = l, lambda_maj = 2.1, range_t = c(1, 13), only1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens, ppp2 = r_ztppp_t, threshold = 0.1, showQQ = TRUE)
})


