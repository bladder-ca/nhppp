test_that("nhppp_t_piecewise_regular_vec() works", {
  # 1 row matrix
  expect_no_error(Z0 <- nhppp_t_piecewise_regular_vec(
    Lambda_matrix = matrix(1:5, nrow = 1),
    range_t = c(100, 110),
    tol = 10^-6,
    only1 = FALSE
  ))

  check_ppp_sample_validity(Z0[1, ], t_min = 100, t_max = 110)

  l <- matrix(rep(1, 50), ncol = 5)
  L <- mat_cumsum_columns(l)

  expect_no_error(Z <- nhppp_t_piecewise_regular_vec(
    Lambda_matrix = L,
    range_t = c(100, 110),
    tol = 10^-6,
    only1 = FALSE
  ))

  for (i in 1:nrow(Z)) {
    check_ppp_sample_validity(Z[i, !is.na(Z[i, ])], t_min = 100, t_max = 110)
  }
  expect_no_error(Z1 <- nhppp_t_piecewise_regular_vec(
    Lambda_matrix = L,
    range_t = c(100, 110),
    tol = 10^-6,
    only1 = TRUE
  ))
  for (i in 1:nrow(Z1)) {
    check_ppp_sample_validity(Z1[i, !is.na(Z1[i, ])], t_min = 100, t_max = 110, only1 = TRUE)
  }
})
