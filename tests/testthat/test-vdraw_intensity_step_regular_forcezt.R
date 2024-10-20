## add tests that have lambda_maj < lambda
## add tests that have lamda_args

test_that("vdraw_intensity_step_regular_forcezt() works", {
  set.seed(123)
  lfun <- function(x, lambda_args, ...) .2 * x^lambda_args$exponent
  l_args <- list(exponent = 1L)
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  expect_no_error(Z <- vdraw_intensity_step_regular_forcezt(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)

  expect_no_error(Z <- vdraw_intensity_step_regular_forcezt(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)

  expect_no_error(Z <- vdraw_intensity_step_regular_forcezt(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = TRUE)

  expect_no_error(Z <- vdraw_intensity_step_regular_forcezt(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = TRUE)
})



test_that("vdraw_intensity_step_regular_forcezt() does not break with matrices whose mode is list", {
  set.seed(123)
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  mode(Lmaj) <- "list"
  expect_no_error(Z <- vdraw_intensity_step_regular_forcezt(
    lambda = lfun,
    Lambda_maj_matrix = Lmaj,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = FALSE)
  mode(lmaj) <- "list"
  expect_no_error(Z <- vdraw_intensity_step_regular_forcezt(
    lambda = lfun,
    lambda_maj_matrix = lmaj,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = FALSE)
})


test_that("vdraw_intensity_step_regular_forcezt() works with different majorizers", {
  set.seed(123)
  lfun <- function(x, lambda_args, ...) .2 * x^lambda_args$exponent
  l_args <- list(exponent = 1L)
  lmaj <- matrix(rep(1, 1000), ncol = 5)

  expect_no_error(Z1 <- vdraw_intensity_step_regular_forcezt(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z1, t_min = 1, t_max = 5)

  expect_no_error(Z2 <- vdraw_intensity_step_regular_forcezt(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj + 10,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z2, t_min = 1, t_max = 5)

  compare_ppp_vectors(ppp1 = Z1, ppp2 = Z2, threshold = 0.1, showQQ = TRUE)
})
