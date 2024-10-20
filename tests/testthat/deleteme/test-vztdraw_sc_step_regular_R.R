test_that("vztdraw_sc_step_regular_R() works", {
  set.seed(123)
  # 1 row matrix
  expect_no_error(Z0 <- vztdraw_sc_step_regular_R(
    Lambda_matrix = matrix(1:5, nrow = 1),
    range_t = c(100, 110),
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z0, t_min = 100, t_max = 110, atleast1 = TRUE)

  l <- matrix(rep(1, 50), ncol = 5)
  L <- mat_cumsum_columns(l)

  expect_no_error(Z <- vztdraw_sc_step_regular_R(
    Lambda_matrix = L,
    range_t = c(100, 110),
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110, atleast1 = TRUE)

  expect_no_error(Z1 <- vztdraw_sc_step_regular_R(
    Lambda_matrix = L,
    range_t = c(100, 110),
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z1, t_min = 100, t_max = 110, atmost1 = TRUE, atleast1 = TRUE)

  expect_no_error(Z2 <- vztdraw_sc_step_regular_R(
    lambda_matrix = l,
    range_t = c(100, 110),
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z2, t_min = 100, t_max = 110, atmost1 = TRUE, atleast1 = TRUE)

  # very small rates
  expect_no_error(Z2 <- vztdraw_sc_step_regular_R(
    lambda_matrix = l * 0.001,
    range_t = c(100, 110),
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z2, t_min = 100, t_max = 110, atmost1 = TRUE, atleast1 = TRUE)
})

test_that("vztdraw_sc_step_regular_R() does not break with matrices whose mode is list", {
  set.seed(123)
  l <- matrix(rep(1, 50), ncol = 5)
  L <- mat_cumsum_columns(l)

  mode(L) <- "list"
  expect_no_error(Z <- vztdraw_sc_step_regular_R(
    Lambda_matrix = L,
    range_t = c(100, 110),
    atmost1 = FALSE
  ))
})
