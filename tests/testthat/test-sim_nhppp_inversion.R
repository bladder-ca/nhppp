test_that("sim_nhppp_t_inv() works without the Lambda_inv option", {
  L <- function(t) {
    return(2 * t)
  }
  # Li = function (z) { return(z/2) }
  expect_no_error(withr::with_preserve_seed(df <- sim_nhppp_t_inv(Lambda = L)))
  expect_true(max(df) <= 10)
})

test_that("sim_nhppp_t_inv() works with the Lambda_inv option", {
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }

  withr::with_seed(12345, df1 <- sim_nhppp_t_inv(Lambda = L, Lambda_inv = NULL, range_t = c(0, 1)))
  expect_no_error(withr::with_seed(12345, df2 <- sim_nhppp_t_inv(Lambda = L, Lambda_inv = Li, range_t = c(0, 1))))
  expect_identical(round(df1, 4), round(df2, 4))
})

test_that("sim_nhppp_t_inv() works with rstream generator", {
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }
  S1 <- methods::new("rstream.mrg32k3a")

  expect_no_error(sim_nhppp_t_inv(Lambda = L, Lambda_inv = Li, rng_stream = S1))
})

test_that("sim_nhppp_t_inv() is the same as sim_ppp_t() if Lambda=rate*t", {
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }

  withr::with_seed(12345, df1 <- sim_nhppp_t_inv(Lambda = L, Lambda_inv = Li, range_t = c(0, 10)))
  withr::with_seed(12345, df2 <- sim_ppp_t(rate = 2, range_t = c(0, 10)))
  expect_identical(round(df1, 4), round(df2, 4))
})
