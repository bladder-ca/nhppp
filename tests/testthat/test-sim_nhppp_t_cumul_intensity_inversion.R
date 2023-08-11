test_that("nhppp_t_cumulative_intensity_inversion() works without the Lambda_inv option", {
  L <- function(t) {
    return(2 * t)
  }
  expect_no_error(withr::with_preserve_seed(df <- nhppp_t_cumulative_intensity_inversion(Lambda = L)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10)
})

test_that("nhppp_t_cumulative_intensity_inversion() works with the Lambda_inv option", {
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }

  withr::with_seed(12345, df1 <- nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = NULL, range_t = c(0, 1)))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 1)
  expect_no_error(withr::with_seed(12345, df2 <- nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = c(0, 1))))
  expect_identical(round(df1, 4), round(df2, 4))
})

test_that("nhppp_t_cumulative_intensity_inversion() works with rstream generator", {
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }
  S1 <- methods::new("rstream.mrg32k3a")

  expect_no_error(df <- nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, rng_stream = S1))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10)
})

test_that("nhppp_t_cumulative_intensity_inversion() is the same as ppp_t() if Lambda=rate*t", {
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }

  withr::with_seed(12345, df1 <- nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = c(0, 10)))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)
  withr::with_seed(12345, df2 <- ppp_t(rate = 2, range_t = c(0, 10)))
  expect_identical(round(df1, 4), round(df2, 4))
})


test_that("nhppp_t_intensity_linear() works", {
  withr::with_seed(12345, df1 <- nhppp_t_intensity_linear(alpha = 2, beta = 0, range_t = c(1, 10)))
  withr::with_seed(12345, df2 <- ppp_t_orderstat(rate = 2, range_t = c(1, 10)))
  expect_identical(df1, df2)

  withr::with_seed(12345, df1 <- nhppp_t_intensity_linear(alpha = 0, beta = 1, range_t = c(1, 10)))
  withr::with_seed(12345, df2 <- nhppp_t_cumulative_intensity_inversion(
    Lambda = function(t) Lambda_linear_form(t, alpha = 0, beta = 1, t0 = 1),
    range_t = c(1, 10)
  ))
  expect_identical(round(df1, 3), round(df2, 3))

  withr::with_seed(12345, df1 <- nhppp_t_intensity_linear(alpha = 10, beta = -2, range_t = c(1, 10)))
  withr::with_seed(12345, df2 <- nhppp_t_cumulative_intensity_inversion(
    Lambda = function(t) Lambda_linear_form(t, alpha = 10, beta = -2, t0 = 1),
    range_t = c(1, 5)
  ))
  expect_identical(round(df1, 3), round(df2, 3))
})
