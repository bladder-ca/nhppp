test_that("ztppp agrees with ppp for high intensities", {
  set.seed(123)
  # High intensity, there are practically no zero event trajectories
  r_ppp_t <- unlist(lapply(integer(10000), function(x) ppp(t_min = 1, t_max = 3, rate = 10, atmost1 = TRUE)))
  r_ztppp <- unlist(lapply(integer(10000), function(x) ztppp(t_min = 1, t_max = 3, rate = 10, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ppp_t, ppp2 = r_ztppp, threshold = 0.1, showQQ = TRUE)
})

test_that("ztppp agrees with ppp for low intensities", {
  set.seed(123)
  # Low intensity, there are many zero event trajectories -- condition on having >=1 event
  r_ppp_t_low <- unlist(lapply(integer(10000), function(x) ppp(t_min = 1, t_max = 3, rate = 0.1, atmost1 = TRUE)))
  r_ztppp_low <- unlist(lapply(integer(length(r_ppp_t_low)), function(x) ztppp(t_min = 1, t_max = 3, rate = 0.1, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ppp_t_low, ppp2 = r_ztppp_low, threshold = 0.1, showQQ = TRUE)
})

test_that("ztNHPPP methods agree on the first time to event with constant rate", {
  set.seed(123)
  l <- function(t) rep(2, length(t))
  L <- function(t) 2 * t
  Li <- function(z) z / 2

  r_ztppp <- unlist(lapply(integer(10000), function(x) ztppp(t_min = 1, t_max = 13, rate = 2, atmost1 = TRUE)))
  r_ztnhppp_ci <- unlist(lapply(integer(10000), function(x) ztdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_ci, ppp2 = r_ztppp, threshold = 0.1, showQQ = TRUE)

  r_ztnhppp_intens_linear <- unlist(lapply(integer(10000), function(x) ztdraw_sc_linear(intercept = 2, slope = 0, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens_linear, ppp2 = r_ztppp, threshold = 0.1, showQQ = TRUE)

  r_ztnhppp_intens_exp <- unlist(lapply(integer(10000), function(x) ztdraw_sc_loglinear(intercept = log(2), slope = 0, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens_exp, ppp2 = r_ztppp, threshold = 0.1, showQQ = TRUE)

  r_ztnhppp_intens <- unlist(lapply(integer(10000), function(x) ztdraw_intensity(lambda = l, line_majorizer_intercept = 2.1, line_majorizer_slope = 0, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens, ppp2 = r_ztppp, threshold = 0.1, showQQ = TRUE)

  r_ztnhppp_intens_piecewise <- unlist(lapply(integer(10000), function(x) ztdraw_intensity_step(lambda = l, majorizer_vector = l(2:13), time_breaks = c(1:13), atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens, ppp2 = r_ztppp, threshold = 0.1, showQQ = TRUE)
})

test_that("ztNHPPP linear intensity agrees with general methods", {
  set.seed(123)
  l <- function(t, intercept = 1, slope = 2) intercept + slope * t
  L <- function(t, intercept = 1, slope = 2, t0 = 1) Lambda_linear_form(t, intercept = intercept, slope = slope, t0 = t0)
  Li <- function(z, intercept = 1, slope = 2, t0 = 1) Lambda_inv_linear_form(z, intercept = intercept, slope = slope, t0 = t0)

  r_ztnhppp_intens_linear <- unlist(lapply(integer(10000), function(x) ztdraw_sc_linear(intercept = 1, slope = 2, t_min = 1, t_max = 13, atmost1 = TRUE)))
  r_ztnhppp_ci <- unlist(lapply(integer(10000), function(x) ztdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens_linear, ppp2 = r_ztnhppp_ci, threshold = 0.1, showQQ = TRUE)

  r_ztnhppp_intens <- unlist(lapply(integer(10000), function(x) ztdraw_intensity(lambda = l, line_majorizer_intercept = l(13), line_majorizer_slope = 0, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens_linear, ppp2 = r_ztnhppp_intens, threshold = 0.1, showQQ = TRUE)
})


test_that("ztNHPPP linear intensity agrees with linear intensity for high rates", {
  set.seed(123)
  r_ztnhppp_intens_linear <- unlist(lapply(integer(10000), function(x) ztdraw_sc_linear(intercept = 1, slope = 2, t_min = 1, t_max = 13, atmost1 = TRUE)))
  r_nhppp_intens_linear <- unlist(lapply(integer(10000), function(x) draw_sc_linear(intercept = 1, slope = 2, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens_linear, ppp2 = r_nhppp_intens_linear, threshold = 0.1, showQQ = TRUE)
})

test_that("ztNHPPP linear intensity agrees with linear intensity for low rates", {
  set.seed(123)
  r_nhppp_intens_linear <- unlist(lapply(integer(10000), function(x) draw_sc_linear(intercept = 1, slope = 2, t_min = 1, t_max = 1.1, atmost1 = TRUE)))
  r_ztnhppp_intens_linear <- unlist(lapply(integer(length(r_nhppp_intens_linear)), function(x) ztdraw_sc_linear(intercept = 1, slope = 2, t_min = 1, t_max = 1.1, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens_linear, ppp2 = r_nhppp_intens_linear, threshold = 0.15, showQQ = TRUE)
})


test_that("vztdraw_sc_step_regular() agrees with vdraw_sc_step_regular()", {
  set.seed(123)
  Lmat <- matrix(rep(c(1, 11, 14, 17), 10000), ncol = 4, byrow = TRUE)
  r_vdraw_sc_step_regular <- vdraw_sc_step_regular(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = FALSE)
  r_vdraw_sc_step_regular <- r_vdraw_sc_step_regular[!is.na(r_vdraw_sc_step_regular)]
  r_vztdraw_sc_step_regular <- vztdraw_sc_step_regular_cpp(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = FALSE)
  r_vztdraw_sc_step_regular <- r_vztdraw_sc_step_regular[!is.na(r_vztdraw_sc_step_regular)]
  compare_ppp_vectors(ppp1 = r_vdraw_sc_step_regular, ppp2 = r_vztdraw_sc_step_regular, threshold = 0.1, showQQ = TRUE)

  r_vdraw_sc_step_regular1 <- vdraw_sc_step_regular(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = TRUE)
  r_vdraw_sc_step_regular1 <- r_vdraw_sc_step_regular1[!is.na(r_vdraw_sc_step_regular1)]
  r_vztdraw_sc_step_regular1 <- vztdraw_sc_step_regular_cpp(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = TRUE)
  r_vztdraw_sc_step_regular1 <- r_vztdraw_sc_step_regular1[!is.na(r_vztdraw_sc_step_regular1)]
  compare_ppp_vectors(ppp1 = r_vdraw_sc_step_regular1, ppp2 = r_vztdraw_sc_step_regular1, threshold = 0.1, showQQ = TRUE)
})


test_that("vztdraw_sc_step_regular_cpp() agrees with vdraw_sc_step_regular()", {
  set.seed(123)
  Lmat <- matrix(rep(c(1, 11, 14, 17), 10000), ncol = 4, byrow = TRUE)
  r_vdraw_sc_step_regular <- vdraw_sc_step_regular(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = FALSE)
  r_vdraw_sc_step_regular <- r_vdraw_sc_step_regular[!is.na(r_vdraw_sc_step_regular)]
  r_vztdraw_sc_step_regular <- vztdraw_sc_step_regular_cpp(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = FALSE)
  r_vztdraw_sc_step_regular <- r_vztdraw_sc_step_regular[!is.na(r_vztdraw_sc_step_regular)]
  compare_ppp_vectors(ppp1 = r_vdraw_sc_step_regular, ppp2 = r_vztdraw_sc_step_regular, threshold = 0.1, showQQ = TRUE)

  r_vdraw_sc_step_regular1 <- vdraw_sc_step_regular(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = TRUE)
  r_vdraw_sc_step_regular1 <- r_vdraw_sc_step_regular1[!is.na(r_vdraw_sc_step_regular1)]
  r_vztdraw_sc_step_regular1 <- vztdraw_sc_step_regular_cpp(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = TRUE)
  r_vztdraw_sc_step_regular1 <- r_vztdraw_sc_step_regular1[!is.na(r_vztdraw_sc_step_regular1)]
  compare_ppp_vectors(ppp1 = r_vdraw_sc_step_regular1, ppp2 = r_vztdraw_sc_step_regular1, threshold = 0.1, showQQ = TRUE)
})



test_that("ztNHPPP exponential agrees with general methods", {
  set.seed(123)
  l <- function(t, intercept = .1, slope = .02) exp(intercept + slope * t)
  L <- function(t, intercept = .1, slope = .02, t0 = 1) Lambda_exp_form(t, intercept = intercept, slope = slope, t0 = t0)
  Li <- function(z, intercept = .1, slope = .02, t0 = 1) Lambda_inv_exp_form(z, intercept = intercept, slope = slope, t0 = t0)

  r_ztnhppp_intens_exp <- unlist(lapply(integer(10000), function(x) ztdraw_sc_loglinear(intercept = .1, slope = .02, t_min = 1, t_max = 13, atmost1 = TRUE)))
  r_ztnhppp_ci <- unlist(lapply(integer(10000), function(x) ztdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens_exp, ppp2 = r_ztnhppp_ci, threshold = 0.1, showQQ = TRUE)

  r_ztnhppp_intens <- unlist(lapply(integer(10000), function(x) ztdraw_intensity(lambda = l, line_majorizer_intercept = l(13), line_majorizer_slope = 0, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ztnhppp_intens_exp, ppp2 = r_ztnhppp_intens, threshold = 0.1, showQQ = TRUE)
})


test_that("ztNHPPP exponential agrees with NHPPP exponential for high rates", {
  set.seed(123)
  r_nhppp_intens_exp <- unlist(lapply(integer(10000), function(x) draw_sc_loglinear(intercept = .1, slope = .02, t_min = 1, t_max = 13, atmost1 = TRUE)))
  r_ztnhppp_intens_exp <- unlist(lapply(integer(10000), function(x) ztdraw_sc_loglinear(intercept = .1, slope = .02, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_exp, ppp2 = r_ztnhppp_intens_exp, threshold = 0.1, showQQ = TRUE)
})


test_that("ztNHPPP exponential agrees with NHPPP exponential for low rates", {
  set.seed(123)
  r_nhppp_intens_exp <- unlist(lapply(integer(10000), function(x) draw_sc_loglinear(intercept = .1, slope = .02, t_min = 1, t_max = 1.1, atmost1 = TRUE)))
  r_ztnhppp_intens_exp <- unlist(lapply(integer(length(r_nhppp_intens_exp)), function(x) ztdraw_sc_loglinear(intercept = .1, slope = .02, t_min = 1, t_max = 1.1, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_exp, ppp2 = r_ztnhppp_intens_exp, threshold = 0.1, showQQ = TRUE)
})
