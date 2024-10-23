test_that("vztdraw_sc_step_regular_cpp() works", {
  set.seed(123)
  # 1 row matrix
  expect_no_error(Z0 <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:5, nrow = 1),
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z0, t_min = 100, t_max = 110, atleast1 = TRUE)


  l <- lref <- matrix(rep(1, 50), ncol = 5)
  L <- Lref <- mat_cumsum_columns(l)

  expect_no_error(Z <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = L,
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110, atleast1 = TRUE)


  expect_no_error(Z1 <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = L,
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z1, t_min = 100, t_max = 110, atmost1 = TRUE, atleast1 = TRUE)

  expect_no_error(Z2 <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = l,
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z2, t_min = 100, t_max = 110, atmost1 = TRUE, atleast1 = TRUE)

  # very small rates
  expect_no_error(Z2 <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = l * 0.001,
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z2, t_min = 100, t_max = 110, atmost1 = TRUE, atleast1 = TRUE)


  expect_equal(l, lref)
  expect_equal(L, Lref)
})

test_that("vztdraw_sc_step_regular_cpp() does not break with matrices whose mode is list", {
  set.seed(123)
  l <- matrix(rep(1, 50), ncol = 5)
  L <- mat_cumsum_columns(l)

  mode(L) <- "list"
  expect_no_error(Z <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = L,
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    atmost1 = FALSE
  ))
})



test_that("vztdraw_sc_step_regular_cpp() uses blocked random numbers", {
  set.seed(123)
  l <- matrix(rep(1, 500), ncol = 5)
  L <- mat_cumsum_columns(l)

  Z <- list()
  for (i in 1:2) {
    set.seed(123)
    expect_no_error(Z[[i]] <- vztdraw_sc_step_regular_cpp(
      Lambda_matrix = L,
      rate_matrix_t_min = 100, rate_matrix_t_max = 110,
      atmost1 = FALSE
    ))
    if (i > 1) {
      expect_true(identical(Z[[1]], Z[[i]]))
    }
  }
})


test_that("vztdraw_sc_step_regular_cpp() works with subinterval", {
  set.seed(123)
  expect_no_error(Z0 <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:5, nrow = 1),
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    t_min = 100, t_max = 110, # matrix
    atmost1 = FALSE
  ))

  expect_no_error(Z0 <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:5, nrow = 1),
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    t_min = 100, t_max = 110, # vector
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z0, t_min = 100, t_max = 110, atmost1 = FALSE, atleast1 = TRUE)
  expect_no_error(Z0 <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:5, nrow = 1),
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    t_min = 101.01, t_max = 108.99,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z0, t_min = 101.01, t_max = 108.99, atmost1 = FALSE, atleast1 = TRUE)

  expect_no_error(Z0 <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:5, nrow = 1) * 10,
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    t_min = 105.01, t_max = 105.99,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z0, t_min = 105.01, t_max = 105.99, atmost1 = FALSE, atleast1 = TRUE)

  expect_no_error(Z0 <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:5, nrow = 1),
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    t_min = 101.01, t_max = 108.99,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z0, t_min = 101.01, t_max = 108.99, atmost1 = TRUE, atleast1 = TRUE)

  expect_no_error(Z0 <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:500, nrow = 10),
    rate_matrix_t_min = 0, rate_matrix_t_max = 10,
    t_min = 0, t_max = 5,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z0, t_min = 0, t_max = 5, atmost1 = TRUE, atleast1 = TRUE)
})
