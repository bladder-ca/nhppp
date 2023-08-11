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
