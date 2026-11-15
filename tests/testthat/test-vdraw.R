test_that("vdraw() routes to thinning with lambda_maj_matrix", {
  set.seed(123)
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)

  expect_no_error(Z <- vdraw(
    lambda = lfun,
    lambda_maj_matrix = lmaj,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)
})

test_that("vdraw() routes to thinning with only Lambda_maj_matrix", {
  # Regression: the dispatch condition tested lambda_maj_matrix twice,
  # so a lone Lambda_maj_matrix fell through to vdraw_cumulative_intensity()
  # with Lambda = NULL and errored.
  set.seed(123)
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  expect_no_error(Z <- vdraw(
    lambda = lfun,
    Lambda_maj_matrix = Lmaj,
    rate_matrix_t_min = 1,
    rate_matrix_t_max = 5,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)
})

test_that("vdraw() routes to the cumulative intensity sampler", {
  set.seed(123)
  L <- function(t, Lambda_args = NULL) .2 * t
  Li <- function(z, Lambda_inv_args = NULL) 5 * z

  expect_no_error(Z <- vdraw(
    Lambda = L,
    Lambda_inv = Li,
    t_min = rep(1, 10),
    t_max = rep(5, 10),
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)
})
