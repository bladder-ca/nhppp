## add tests that have lambda_maj < lambda
## add tests that have lamda_args

test_that("vdraw_intensity_step_regular_cpp() works", {
  set.seed(123)
  lfun <- function(x, lambda_args, ...) .2 * x^lambda_args$exponent
  l_args <- list(exponent = 1L)
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = TRUE)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = TRUE)


  #### Also works for 1 row and range_t being matrix or vector

  # range_t vector
  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj[1, , drop = FALSE],
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)

  # range_t matrix
  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj[1, , drop = FALSE],
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)
})



test_that("vdraw_intensity_step_regular_cpp() does not break with matrices whose mode is list", {
  set.seed(123)
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  mode(Lmaj) <- "list"
  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    Lambda_maj_matrix = Lmaj,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  mode(lmaj) <- "list"
  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_maj_matrix = lmaj,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
})



test_that("vdraw_intensity_step_regular_cpp() works with subinterval", {
  set.seed(123)
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    Lambda_maj_matrix = Lmaj,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    t_min = 2.3,
    t_max = 4.8,
    tol = 10^-6,
    atmost1 = FALSE
  ))

  check_ppp_sample_validity(Z, t_min = 2.3, t_max = 4.8, atmost1 = FALSE)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_maj_matrix = lmaj,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    t_min = 2.3,
    t_max = 4.8,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 2.3, t_max = 4.8, atmost1 = FALSE)


  #### Also works for 1 row and range_t, subinterval being matrix or vector

  # range_t, subinterval vectors
  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_maj_matrix = lmaj[1, , drop = FALSE],
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    t_min = 2.3,
    t_max = 4.8,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 2.3, t_max = 4.8, atmost1 = FALSE)

  # range_t,subinterval matrices
  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_maj_matrix = lmaj[1, , drop = FALSE],
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    t_min = 2.3,
    t_max = 4.8,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 2.3, t_max = 4.8, atmost1 = FALSE)
})


test_that("vdraw_intensity_step_regular_cpp() works with different majorizers", {
  set.seed(123)
  lfun <- function(x, lambda_args, ...) .2 * x^lambda_args$exponent
  l_args <- list(exponent = 1L)
  lmaj <- matrix(rep(1, 1000), ncol = 5)

  expect_no_error(Z1 <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z1, t_min = 1, t_max = 5)

  expect_no_error(Z2 <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj + 10,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z2, t_min = 1, t_max = 5)

  compare_ppp_vectors(ppp1 = Z1, ppp2 = Z2, threshold = 0.1, showQQ = TRUE)
})





test_that("vdraw_intensity_step_regular_cpp() uses blocked random numbers", {
  set.seed(123)
  lfun <- function(x, lambda_args, ...) .2 * x^lambda_args$exponent
  l_args <- list(exponent = 1L)
  l_ <- function(x) lfun(x, lambda_args = l_args)
  N <- 1000
  lmaj0 <- get_step_majorizer(fun = l_, breaks = matrix(rep(1:11, each = N), nrow = N), is_monotone = FALSE, K = 0)
  lmaj1 <- get_step_majorizer(fun = l_, breaks = matrix(rep(1:11, each = N), nrow = N), is_monotone = FALSE, K = 1)
  lmaj10 <- get_step_majorizer(fun = l_, breaks = matrix(rep(1:11, each = N), nrow = N), is_monotone = FALSE, K = 10)

  Z0 <- list()
  for (i in 1:2) {
    set.seed(123)
    expect_no_error(Z0[[i]] <- vdraw_intensity_step_regular_cpp(
      lambda = lfun,
      lambda_args = l_args,
      lambda_maj_matrix = lmaj0,
      rate_matrix_t_min = 1,
      rate_matrix_t_max = 5,
      tol = 10^-6,
      atmost1 = FALSE
    ))
    if (i > 1) {
      expect_true(identical(Z0[[1]], Z0[[i]]))
    }
  }
  check_ppp_sample_validity(Z0[[1]], t_min = 1, t_max = 5)
})
