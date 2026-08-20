test_that("vztdraw_intensity() works with vector range_t", {
  # Regression: vztdraw_intensity() passed `range_t =` through to
  # vztdraw_intensity_step_regular(), which takes rate_matrix_t_min/_max;
  # range_t was swallowed by `...` and the call errored.
  set.seed(123)
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  expect_no_error(Z <- nhppp:::vztdraw_intensity(
    lambda = lfun,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5)
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleast1 = TRUE)

  expect_no_error(Z <- nhppp:::vztdraw_intensity(
    lambda = lfun,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = TRUE, atleast1 = TRUE)
})

test_that("vztdraw_intensity() works with matrix range_t", {
  set.seed(123)
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)

  expect_no_error(Z <- nhppp:::vztdraw_intensity(
    lambda = lfun,
    lambda_maj_matrix = lmaj,
    range_t = matrix(rep(c(1, 5), each = 10), ncol = 2)
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleast1 = TRUE)
})
