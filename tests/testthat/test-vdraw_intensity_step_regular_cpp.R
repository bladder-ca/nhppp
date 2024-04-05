## add tests that have lambda_maj < lambda
## add tests that have lamda_args

test_that("vdraw_intensity_step_regular_cpp() works", {
  lfun <- function(x, lambda_args, ...) .2 * x^lambda_args$exponent
  l_args <- list(exponent = 1L)
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = TRUE)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = TRUE)
})



test_that("vdraw_intensity_step_regular_cpp() does not break with matrices whose mode is list", {
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  mode(Lmaj) <- "list"
  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  mode(lmaj) <- "list"
  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
})



test_that("vdraw_intensity_step_regular_cpp() works with subinterval", {
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)


  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    subinterval = c(2.3, 4.8),
    tol = 10^-6,
    atmost1 = FALSE
  ))

  check_ppp_sample_validity(Z, t_min = 2.3, t_max = 4.8, atmost1 = FALSE)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5),
    subinterval = c(2.3, 4.8),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 2.3, t_max = 4.8, atmost1 = FALSE)
})
