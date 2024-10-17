test_that("vdraw_cumulative_intensity_inversion() works with minimal options", {
  L <- function(t, ...) {
    return(2 * t)
  }
  Li <- function(z, ...) {
    return(z / 2)
  }

  range_T0 <- c(0, 1)
  range_T1 <- matrix(c(0, 1), ncol = 2) + runif(n = 1)
  range_T10 <- range_T1[rep(1, 10), ] + runif(n = 10)

  # range_t is one vector
  expect_no_error(df0 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = range_T0, atmost1 = FALSE))
  check_ppp_sample_validity(df0, t_min = 0, t_max = 1)

  # range_T is one-row matrix vector
  expect_no_error(df1 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = range_T1, atmost1 = FALSE))
  check_ppp_sample_validity(df1, t_min = min(range_T1), t_max = max(range_T1))

  # range_T is many-row matrix
  expect_no_error(df10 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = range_T10, atmost1 = FALSE))
  check_ppp_sample_validity(df10, t_min = range_T10[, 1], t_max = range_T10[, 2])

  # range_T is many-row matrix atmost1 = TRUE
  expect_no_error(df10.1 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = range_T10, atmost1 = TRUE))
  check_ppp_sample_validity(df10.1, t_min = range_T10[, 1], t_max = range_T10[, 2], atmost1 = TRUE)
})

test_that("vdraw_cumulative_intensity_inversion() works when functions take arguments", {
  L <- function(t, Lambda_args) {
    return(Lambda_args$a * t)
  }
  Li <- function(z, Lambda_inv_args) {
    return(z / Lambda_inv_args$a)
  }

  args <- list(a = 2)

  range_T0 <- c(0, 1)
  range_T1 <- matrix(c(0, 1), ncol = 2) + runif(n = 1)
  range_T10 <- range_T1[rep(1, 10), ] + runif(n = 10)

  expect_no_error(df0 <- vdraw_cumulative_intensity(Lambda = L, Lambda_args = args, Lambda_inv = Li, Lambda_inv_args = args, range_t = range_T0))
  check_ppp_sample_validity(df0, t_min = 0, t_max = 1)
  expect_no_error(df1 <- vdraw_cumulative_intensity(Lambda = L, Lambda_args = args, Lambda_inv = Li, Lambda_inv_args = args, range_t = range_T1))
  check_ppp_sample_validity(df1, t_min = min(range_T1), t_max = max(range_T1))
  expect_no_error(df10 <- vdraw_cumulative_intensity(Lambda = L, Lambda_args = args, Lambda_inv = Li, Lambda_inv_args = args, range_t = range_T10))
  check_ppp_sample_validity(df10, t_min = min(range_T10), t_max = max(range_T10))


  expect_no_error(df10.1 <- vdraw_cumulative_intensity(Lambda = L, Lambda_args = args, Lambda_inv = Li, Lambda_inv_args = args, range_t = range_T10, atmost1 = TRUE))
  check_ppp_sample_validity(df10.1, t_min = min(range_T10), t_max = max(range_T10), atmost1 = TRUE)
})



test_that("vdraw_cumulative_intensity_inversion() uses blocked random numbers", {
  set.seed(123)
  L <- function(t, ...) {
    return(2 * t)
  }
  Li <- function(z, ...) {
    return(z / 2)
  }

  range_T1 <- matrix(c(0, 1), ncol = 2)
  range_T20 <- range_T1[rep(1, 20), ] + runif(n = 20)

  Z0 <- list()
  for (i in 1:2) {
    set.seed(123)
    expect_no_error(Z0[[i]] <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = range_T20, atmost1 = FALSE))
    if (i > 1) {
      expect_true(identical(Z0[[1]], Z0[[i]]))
    }
  }
  check_ppp_sample_validity(Z0[[1]], t_min = range_T20[, 1], t_max = range_T20[, 2])
})
