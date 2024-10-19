test_that("vdraw_cumulative_intensity_inversion() works with minimal options", {
  L <- function(t, ...) {
    return(2 * t)
  }
  Li <- function(z, ...) {
    return(z / 2)
  }

  t0_a <- 0.5
  t1_a <- 1.5
  t0_b <- rep(0.5, 10) + runif(n = 10)
  t1_b <- rep(2, 10) + runif(n = 10)
  t0_c <- matrix(t0_b, nrow = 1)
  t1_c <- matrix(t1_b, nrow = 1)
  t0_d <- matrix(t0_b, ncol = 1)
  t1_d <- matrix(t1_b, ncol = 1)


  # scalars
  expect_no_error(df0 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_a, t_max = t1_a, atmost1 = FALSE))
  check_ppp_sample_validity(df0, t_min = t0_a, t_max = t1_a)

  # vectors
  expect_no_error(df1 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_b, t_max = t1_b, atmost1 = FALSE))
  check_ppp_sample_validity(df1, t_min = t0_b, t_max = t1_b)

  # row matrices
  expect_no_error(df10 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_c, t_max = t1_c, atmost1 = FALSE))
  check_ppp_sample_validity(df10, t_min = t0_b, t_max = t1_b)

  # col matrices
  expect_no_error(df10 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_d, t_max = t1_d, atmost1 = FALSE))
  check_ppp_sample_validity(df10, t_min = t0_b, t_max = t1_b)


  # mixed arguments and atmost1 = TRUE
  expect_no_error(df10.1 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_a, t_max = t1_d, atmost1 = TRUE))
  check_ppp_sample_validity(df10.1, t_min = t0_a, t_max = t1_b, atmost1 = TRUE)
})

test_that("vdraw_cumulative_intensity_inversion() works when functions take arguments", {
  L <- function(t, Lambda_args) {
    return(Lambda_args$a * t)
  }
  Li <- function(z, Lambda_inv_args) {
    return(z / Lambda_inv_args$a)
  }

  args <- list(a = 2)

  t0_a <- 0.5
  t1_a <- 1.5
  t0_b <- rep(0.5, 10) + runif(n = 10)
  t1_b <- rep(2, 10) + runif(n = 10)

  # scalars
  expect_no_error(df0 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_a, t_max = t1_a, Lambda_args = args, Lambda_inv_args = args, atmost1 = FALSE))
  check_ppp_sample_validity(df0, t_min = t0_a, t_max = t1_a)
  # vectors
  expect_no_error(df1 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_b, t_max = t1_b, Lambda_args = args, Lambda_inv_args = args, atmost1 = FALSE))
  check_ppp_sample_validity(df1, t_min = t0_b, t_max = t1_b)
})

test_that("vdraw_cumulative_intensity_inversion() uses blocked random numbers", {
  set.seed(123)
  L <- function(t, ...) {
    return(2 * t)
  }
  Li <- function(z, ...) {
    return(z / 2)
  }

  t0_a <- 0.5
  t1_a <- 1.5
  t0_b <- rep(0.5, 10) + runif(n = 10)
  t1_b <- rep(2, 10) + runif(n = 10)


  Z0 <- list()
  for (i in 1:2) {
    set.seed(123)
    expect_no_error(Z0[[i]] <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_b, t_max = t1_b, atmost1 = FALSE))
    if (i > 1) {
      expect_true(identical(Z0[[1]], Z0[[i]]))
    }
  }
  check_ppp_sample_validity(Z0[[1]], t_min = t0_b, t_max = t1_b)
})
