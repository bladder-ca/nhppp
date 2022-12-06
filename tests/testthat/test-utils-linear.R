test_that("Lambda_linear_form works", {
  expect_no_error(L_t <- Lambda_linear_form(t = 1:10, alpha = 0, beta = 0, t0 = 1))
  expect_identical(L_t, rep(0, 10))

  expect_no_error(L_t <- Lambda_linear_form(t = 1:10, alpha = 1, beta = 0, t0 = 1))
  expect_identical(L_t, as.double(0:9))

  expect_no_error(L_t <- Lambda_linear_form(t = 1:10, alpha = 1, beta = 1, t0 = 1))
  expect_identical(L_t, 0:9 + (1:10)^2 / 2 - 1 / 2)
})


test_that("Lambda_inv_linear_form works", {
  L_t <- Lambda_linear_form(t = 1:10, alpha = 0, beta = 1, t0 = 1)
  expect_identical(
    as.double(1:10),
    Lambda_inv_linear_form(z = L_t, alpha = 0, beta = 1, t0 = 1)
  )

  L_t <- Lambda_linear_form(t = 1:10, alpha = 1, beta = 0, t0 = 1)
  expect_identical(
    as.double(1:10),
    Lambda_inv_linear_form(z = L_t, alpha = 1, beta = 0, t0 = 1)
  )

  L_t <- Lambda_linear_form(t = 1:10, alpha = 1, beta = 1, t0 = 1)
  expect_identical(
    as.double(1:10),
    Lambda_inv_linear_form(z = L_t, alpha = 1, beta = 1, t0 = 1)
  )

  expect_error(
    Lambda_inv_linear_form(z = rep(0, 10), alpha = 0, beta = 0, t0 = 1)
  )
})


test_that("Lambda_lf() and Lambda_inv_lf() work", {
  expect_identical(
    as.double(1:100),
    round(Lambda_inv_lf(
      Lambda_lf(1:100, c(10, -.1, 0)), c(10, -.1, 0)
    )), 5
  )
  expect_identical(
    as.double(1:100),
    round(Lambda_inv_lf(
      Lambda_lf(1:100, c(10, .1, 0)), c(10, .1, 0)
    )), 5
  )
  expect_identical(
    as.double(1:100),
    round(Lambda_inv_lf(
      Lambda_lf(1:100, c(10, 0, 0)), c(10, 0, 0)
    )), 5
  )

  expect_identical(
    Lambda_linear_form(t = 1:10, alpha = 1, beta = 0, t0 = 1),
    Lambda_lf(t = 1:10, c(1, 0, 1))
  )

  expect_identical(
    Lambda_linear_form(t = 1:10, alpha = 0, beta = 1, t0 = 1),
    Lambda_lf(t = 1:10, c(0, 1, 1))
  )

  expect_identical(
    Lambda_linear_form(t = 1:10, alpha = 10, beta = -.1, t0 = 1),
    Lambda_lf(t = 1:10, c(10, -.1, 1))
  )
})
