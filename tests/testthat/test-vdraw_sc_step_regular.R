test_that("vdraw_sc_step_regular() works", {
  set.seed(123)
  # 1 row matrix
  expect_no_error(Z0 <- vdraw_sc_step_regular(
    Lambda_matrix = matrix(1:5, nrow = 1),
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z0, t_min = 100, t_max = 110)

  l <- matrix(rep(1, 50), ncol = 5)
  L <- mat_cumsum_columns(l)

  expect_no_error(Z <- vdraw_sc_step_regular(
    Lambda_matrix = L,
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110)

  expect_no_error(Z1 <- vdraw_sc_step_regular(
    Lambda_matrix = L,
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z1, t_min = 100, t_max = 110, atmost1 = TRUE)

  expect_no_error(Z2 <- vdraw_sc_step_regular(
    lambda_matrix = l,
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z2, t_min = 100, t_max = 110, atmost1 = TRUE)
})

test_that("vdraw_sc_step_regular() does not break with matrices whose mode is list", {
  set.seed(123)
  l <- matrix(rep(1, 50), ncol = 5)
  L <- mat_cumsum_columns(l)

  mode(L) <- "list"
  expect_no_error(Z <- vdraw_sc_step_regular(
    Lambda_matrix = L,
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = FALSE
  ))
})



test_that("vdraw_sc_step_regular() uses blocked random numbers", {
  set.seed(123)
  l <- matrix(rep(1, 500), ncol = 5)
  L <- mat_cumsum_columns(l)

  Z <- list()
  for (i in 1:2) {
    set.seed(123)
    expect_no_error(Z[[i]] <- vdraw_sc_step_regular(
      Lambda_matrix = L,
      range_t = c(100, 110),
      tol = 10^-6,
      atmost1 = FALSE,
      use_cpp = FALSE
    ))
    if (i > 1) {
      expect_true(identical(Z[[1]], Z[[i]]))
    }
  }
})
