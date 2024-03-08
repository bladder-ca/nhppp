## add tests that have lambda_maj < lambda
## add tests that have lamda_args

test_that("vztdraw_intensity_step_regular_R() works", {
  lfun <- function(x, lambda_args, ...) .2 * x^lambda_args$exponent
  l_args <- list(exponent = 1L)
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  expect_no_error(Z <- vztdraw_intensity_step_regular_R(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  expect_true(ncol(Z) > 0)

  for (i in 1:nrow(Z)) {
    tmp <- Z[i, !is.na(Z[i, ])]
    if (length(tmp) > 0) {
      check_ppp_sample_validity(tmp, t_min = 1, t_max = 5)
    }
  }

  expect_no_error(Z <- vztdraw_intensity_step_regular_R(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  expect_true(ncol(Z) > 0)
  for (i in 1:nrow(Z)) {
    tmp <- Z[i, !is.na(Z[i, ])]
    if (length(tmp) > 0) {
      check_ppp_sample_validity(tmp, t_min = 1, t_max = 5)
    }
  }

  expect_no_error(Z <- vztdraw_intensity_step_regular_R(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  expect_true(ncol(Z) > 0)
  for (i in 1:nrow(Z)) {
    tmp <- Z[i, !is.na(Z[i, ])]
    if (length(tmp) > 0) {
      check_ppp_sample_validity(tmp, t_min = 1, t_max = 5, atmost1 = TRUE)
    }
  }

  expect_no_error(Z <- vztdraw_intensity_step_regular_R(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  expect_true(ncol(Z) > 0)
  for (i in 1:nrow(Z)) {
    tmp <- Z[i, !is.na(Z[i, ])]
    if (length(tmp) > 0) {
      check_ppp_sample_validity(tmp, t_min = 1, t_max = 5, atmost1 = TRUE)
    }
  }
})



test_that("vztdraw_intensity_step_regular_R() does not break with matrices whose mode is list", {
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)


  mode(Lmaj) <- "list"
  expect_no_error(Z <- vztdraw_intensity_step_regular_R(
    lambda = lfun,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  expect_true(ncol(Z) > 0)
  mode(lmaj) <- "list"
  expect_no_error(Z <- vztdraw_intensity_step_regular_R(
    lambda = lfun,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  expect_true(ncol(Z) > 0)
})
