test_that("vdraw_sc_step_regular() works", {
  # 1 row matrix
  expect_no_error(Z0 <- vdraw_sc_step_regular(
    Lambda_matrix = matrix(1:5, nrow = 1),
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = FALSE
  ))

  Z0 <- Z0[1, !is.na(Z0[1, ])]
  if (length(Z0) > 0) {
    check_ppp_sample_validity(Z0, t_min = 100, t_max = 110)
  }

  l <- matrix(rep(1, 50), ncol = 5)
  L <- mat_cumsum_columns(l)

  expect_no_error(Z <- vdraw_sc_step_regular(
    Lambda_matrix = L,
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = FALSE
  ))

  for (i in 1:nrow(Z)) {
    tmp <- Z[i, !is.na(Z[i, ])]
    if (length(tmp) > 0) {
      check_ppp_sample_validity(tmp, t_min = 100, t_max = 110)
    }
  }
  expect_no_error(Z1 <- vdraw_sc_step_regular(
    Lambda_matrix = L,
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  for (i in 1:nrow(Z1)) {
    tmp <- Z1[i, !is.na(Z1[i, ])]
    if (length(tmp) > 0) {
      check_ppp_sample_validity(tmp, t_min = 100, t_max = 110, atmost1 = TRUE)
    }
  }
  expect_no_error(Z2 <- vdraw_sc_step_regular(
    lambda_matrix = l,
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  for (i in 1:nrow(Z2)) {
    tmp <- Z2[i, !is.na(Z2[i, ])]
    if (length(tmp) > 0) {
      check_ppp_sample_validity(tmp, t_min = 100, t_max = 110, atmost1 = TRUE)
    }
  }
})
