test_that("PPP methods agree on the first time to event", {
  set.seed(123)
  r_ppp_next_n <- unlist(lapply(integer(10000), function(x) ppp_next_n(n = 1, rate = 10, t_min = 1)))
  r_ppp <- unlist(lapply(integer(10000), function(x) ppp(t_min = 1, t_max = 3, rate = 10, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ppp_next_n, ppp2 = r_ppp, threshold = 0.1, showQQ = TRUE)

  r_ppp2 <- unlist(lapply(integer(10000), function(x) ppp2(t_min = 1, t_max = 3, rate = 10, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ppp_next_n, ppp2 = r_ppp2, threshold = 0.1, showQQ = TRUE)

  r_draw_sc_step <- unlist(lapply(integer(10000), function(x) draw_sc_step(time_breaks = c(1, 2, 3), lambda_vector = rep(10, 2), atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ppp_next_n, ppp2 = r_draw_sc_step, threshold = 0.1, showQQ = TRUE)
})


test_that("draw_sc_step() agrees with strung together constant rates", {
  set.seed(123)
  r_ppp <- unlist(lapply(
    integer(10000),
    function(x) {
      c(
        ppp(t_min = 1, t_max = 2, rate = 1, atmost1 = FALSE),
        ppp(t_min = 2, t_max = 2.2, rate = 10, atmost1 = FALSE),
        ppp(t_min = 2.2, t_max = 3, rate = 3, atmost1 = FALSE)
      )
    }
  ))

  r_draw_sc_step <- unlist(lapply(integer(10000), function(x) draw_sc_step(time_breaks = c(1, 2, 2.2, 3), lambda_vector = c(1, 10, 3), atmost1 = FALSE)))
  compare_ppp_vectors(ppp1 = r_ppp, ppp2 = r_draw_sc_step, threshold = 0.1, showQQ = TRUE)


  r_ppp1 <- unlist(lapply(
    integer(10000),
    function(x) {
      c(
        ppp(t_min = 1, t_max = 2, rate = 1, atmost1 = TRUE),
        ppp(t_min = 2, t_max = 2.2, rate = 10, atmost1 = TRUE),
        ppp(t_min = 2.2, t_max = 3, rate = 3, atmost1 = TRUE)
      )[1]
    }
  ))
  r_draw_sc_step1 <- unlist(lapply(integer(10000), function(x) draw_sc_step(time_breaks = c(1, 2, 2.2, 3), lambda_vector = c(1, 10, 3), atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ppp1, ppp2 = r_draw_sc_step1, threshold = 0.1, showQQ = TRUE)
})


test_that("draw_sc_step_regular() agrees with strung together constant rates", {
  set.seed(123)
  r_ppp <- unlist(lapply(
    integer(10000),
    function(x) {
      c(
        ppp(t_min = 1, t_max = 2, rate = 1, atmost1 = FALSE),
        ppp(t_min = 2, t_max = 3, rate = 10, atmost1 = FALSE),
        ppp(t_min = 3, t_max = 5, rate = 3, atmost1 = FALSE)
      )
    }
  ))

  L <- c(1, 11, 14, 17)

  r_ppp_regularstep <- unlist(lapply(integer(10000), function(x) draw_sc_step_regular(Lambda_vector = L, t_min = 1, t_max = 5, atmost1 = FALSE)))
  compare_ppp_vectors(ppp1 = r_ppp, ppp2 = r_ppp_regularstep, threshold = 0.1, showQQ = TRUE)


  r_ppp1 <- unlist(lapply(
    integer(10000),
    function(x) {
      c(
        ppp(t_min = 1, t_max = 2, rate = 1, atmost1 = TRUE),
        ppp(t_min = 2, t_max = 3, rate = 10, atmost1 = TRUE),
        ppp(t_min = 3, t_max = 5, rate = 3, atmost1 = TRUE)
      )[1]
    }
  ))
  r_ppp_regularstep1 <- unlist(lapply(integer(10000), function(x) draw_sc_step_regular(Lambda_vector = L, t_min = 1, t_max = 5, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_ppp1, ppp2 = r_ppp_regularstep1, threshold = 0.1, showQQ = TRUE)
})

test_that("vdraw_sc_step_regular() agrees with strung together constant rates", {
  set.seed(123)
  r_ppp <- unlist(lapply(
    integer(10000),
    function(x) {
      c(
        ppp(t_min = 1, t_max = 2, rate = 1, atmost1 = FALSE),
        ppp(t_min = 2, t_max = 3, rate = 10, atmost1 = FALSE),
        ppp(t_min = 3, t_max = 5, rate = 3, atmost1 = FALSE)
      )
    }
  ))

  Lmat <- matrix(rep(c(1, 11, 14, 17), 10000), ncol = 4, byrow = TRUE)

  r_vdraw_sc_step_regular <- vdraw_sc_step_regular(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = FALSE)
  r_vdraw_sc_step_regular <- r_vdraw_sc_step_regular[!is.na(r_vdraw_sc_step_regular)]
  compare_ppp_vectors(ppp1 = r_ppp, ppp2 = r_vdraw_sc_step_regular, threshold = 0.1, showQQ = TRUE)

  r_ppp1 <- unlist(lapply(
    integer(10000),
    function(x) {
      c(
        ppp(t_min = 1, t_max = 2, rate = 1, atmost1 = TRUE),
        ppp(t_min = 2, t_max = 3, rate = 10, atmost1 = TRUE),
        ppp(t_min = 3, t_max = 5, rate = 3, atmost1 = TRUE)
      )[1]
    }
  ))
  r_vdraw_sc_step_regular1 <- vdraw_sc_step_regular(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = TRUE)
  r_vdraw_sc_step_regular1 <- r_vdraw_sc_step_regular1[!is.na(r_vdraw_sc_step_regular1)]
  compare_ppp_vectors(ppp1 = r_ppp1, ppp2 = r_vdraw_sc_step_regular1, threshold = 0.1, showQQ = TRUE)
})



test_that("vdraw_sc_step_regular_cpp() agrees with strung together constant rates", {
  set.seed(123)
  r_ppp <- unlist(lapply(
    integer(10000),
    function(x) {
      c(
        ppp(t_min = 1, t_max = 2, rate = 1, atmost1 = FALSE),
        ppp(t_min = 2, t_max = 3, rate = 10, atmost1 = FALSE),
        ppp(t_min = 3, t_max = 5, rate = 3, atmost1 = FALSE)
      )
    }
  ))

  Lmat <- matrix(rep(c(1, 11, 14, 17), 10000), ncol = 4, byrow = TRUE)

  r_vdraw_sc_step_regular <- vdraw_sc_step_regular_cpp(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = FALSE)
  r_vdraw_sc_step_regular <- r_vdraw_sc_step_regular[!is.na(r_vdraw_sc_step_regular)]
  compare_ppp_vectors(ppp1 = r_ppp, ppp2 = r_vdraw_sc_step_regular, threshold = 0.1, showQQ = TRUE)

  r_ppp1 <- unlist(lapply(
    integer(10000),
    function(x) {
      c(
        ppp(t_min = 1, t_max = 2, rate = 1, atmost1 = TRUE),
        ppp(t_min = 2, t_max = 3, rate = 10, atmost1 = TRUE),
        ppp(t_min = 3, t_max = 5, rate = 3, atmost1 = TRUE)
      )[1]
    }
  ))
  r_vdraw_sc_step_regular1 <- vdraw_sc_step_regular_cpp(Lambda_matrix = Lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5, atmost1 = TRUE)
  r_vdraw_sc_step_regular1 <- r_vdraw_sc_step_regular1[!is.na(r_vdraw_sc_step_regular1)]
  compare_ppp_vectors(ppp1 = r_ppp1, ppp2 = r_vdraw_sc_step_regular1, threshold = 0.1, showQQ = TRUE)
})


test_that("NHPPP methods agree on the first time to event with constant rate", {
  set.seed(123)
  l <- function(t) 2
  L <- function(t) 2 * t
  Li <- function(z) z / 2

  r_ppp <- unlist(lapply(integer(10000), function(x) ppp(t_min = 1, t_max = 13, rate = 2, atmost1 = TRUE)))
  r_nhppp_ci_inv <- unlist(lapply(integer(10000), function(x) draw_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_ci_inv, ppp2 = r_ppp, threshold = 0.1, showQQ = TRUE)

  r_nhppp_ci_orderstats <- unlist(lapply(integer(10000), function(x) draw_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_ci_orderstats, ppp2 = r_ppp, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens_linear <- unlist(lapply(integer(10000), function(x) draw_sc_linear(intercept = 2, slope = 0, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_linear, ppp2 = r_ppp, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens_exp <- unlist(lapply(integer(10000), function(x) draw_sc_loglinear(intercept = log(2), slope = 0, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_exp, ppp2 = r_ppp, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens <- unlist(lapply(integer(10000), function(x) draw_intensity_line(lambda = l, majorizer_intercept = 2.1, majorizer_slope = 0, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens, ppp2 = r_ppp, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens_piecewise <- unlist(lapply(integer(10000), function(x) draw_intensity_step(lambda = l, majorizer_vector = rep(2.1, 4), time_breaks = c(1, 2, pi, 2 * pi, 13), atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_piecewise, ppp2 = r_ppp, threshold = 0.1, showQQ = TRUE)
})

test_that("NHPPP linear intensity agrees with general methods", {
  set.seed(123)
  l <- function(t, intercept = 1, slope = 2) intercept + slope * t
  L <- function(t, intercept = 1, slope = 2, t0 = 1) Lambda_linear_form(t, intercept = intercept, slope = slope, t0 = t0)
  Li <- function(z, intercept = 1, slope = 2, t0 = 1) Lambda_inv_linear_form(z, intercept = intercept, slope = slope, t0 = t0)

  r_nhppp_intens_linear <- unlist(lapply(integer(10000), function(x) draw_sc_linear(intercept = 1, slope = 2, t_min = 1, t_max = 13, atmost1 = TRUE)))
  r_nhppp_ci_inv <- unlist(lapply(integer(10000), function(x) draw_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_linear, ppp2 = r_nhppp_ci_inv, threshold = 0.1, showQQ = TRUE)

  r_nhppp_ci_os <- unlist(lapply(integer(10000), function(x) draw_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_linear, ppp2 = r_nhppp_ci_os, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens <- unlist(lapply(integer(10000), function(x) draw_intensity_line(lambda = l, majorizer_intercept = l(13), majorizer_slope = 0, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_linear, ppp2 = r_nhppp_intens, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens_piecewise <- unlist(lapply(integer(10000), function(x) draw_intensity_step(lambda = l, majorizer_vector = l(2:13), time_breaks = c(1:13), atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_linear, ppp2 = r_nhppp_intens_piecewise, threshold = 0.1, showQQ = TRUE)
})

test_that("NHPPP loglinear agrees with general methods", {
  set.seed(123)
  l <- function(t, intercept = .1, slope = .02) exp(intercept + slope * t)
  L <- function(t, intercept = .1, slope = .02, t0 = 1) Lambda_exp_form(t, intercept = intercept, slope = slope, t0 = t0)
  Li <- function(z, intercept = .1, slope = .02, t0 = 1) Lambda_inv_exp_form(z, intercept = intercept, slope = slope, t0 = t0)

  r_nhppp_intens_exp <- unlist(lapply(integer(10000), function(x) draw_sc_loglinear(intercept = .1, slope = .02, t_min = 1, t_max = 13, atmost1 = TRUE)))
  r_nhppp_ci_inv <- unlist(lapply(integer(10000), function(x) draw_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_exp, ppp2 = r_nhppp_ci_inv, threshold = 0.1, showQQ = TRUE)

  r_nhppp_ci_os <- unlist(lapply(integer(10000), function(x) draw_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_exp, ppp2 = r_nhppp_ci_os, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens <- unlist(lapply(integer(10000), function(x) draw_intensity_line(lambda = l, majorizer_intercept = l(13), majorizer_slope = 0, t_min = 1, t_max = 13, atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_exp, ppp2 = r_nhppp_intens, threshold = 0.1, showQQ = TRUE)

  r_nhppp_intens_piecewise <- unlist(lapply(integer(10000), function(x) draw_intensity_step(lambda = l, majorizer_vector = l(2:13), time_breaks = c(1:13), atmost1 = TRUE)))
  compare_ppp_vectors(ppp1 = r_nhppp_intens_exp, ppp2 = r_nhppp_intens_piecewise, threshold = 0.1, showQQ = TRUE)
})
