test_that("mat_cumsum_columns() works", {
  X <- matrix(rep(1, 10), ncol = 1)
  cumul_tX <- matrix(1:10, nrow = 1)

  expect_equal(X, mat_cumsum_columns(X))
  expect_equal(matrix(1:10, nrow = 1), mat_cumsum_columns(t(X)))

  X2 <- matrix(rep(1, 100), ncol = 10)
  cumul_X2 <- matrix(rep(cumul_tX, 10), byrow = TRUE, ncol = 10)
  expect_equal(cumul_X2, mat_cumsum_columns(X2))
})


test_that("mat_cumsum_columns_with_scalar_ceiling() works", {
  X <- matrix(rep(1, 10), ncol = 1)
  cumul_tX <- matrix(1:10, nrow = 1)

  # test one-row matrix
  expect_equal(
    matrix(1:10, nrow = 1),
    mat_cumsum_columns_with_scalar_ceiling(t(X), ceil = Inf)
  )
  expect_equal(
    matrix(1:8, nrow = 1),
    mat_cumsum_columns_with_scalar_ceiling(t(X), ceil = 8.2)
  )

  # test one-column matrix
  expect_equal(
    X,
    mat_cumsum_columns_with_scalar_ceiling(X, ceil = Inf)
  )
  expect_equivalent(
    matrix(rep(NA, 10), ncol = 1),
    mat_cumsum_columns_with_scalar_ceiling(X, ceil = 0.5)
  )

  X2 <- X3 <- matrix(rep(1, 100), ncol = 10)
  cumul_X2 <- cumul_X3 <- matrix(rep(cumul_tX, 10), byrow = TRUE, ncol = 10)
  expect_equal(cumul_X2, mat_cumsum_columns_with_scalar_ceiling(X2, ceil = Inf))

  X3[1, ] <- 9
  cumul_X3[1, ] <- NA
  expect_equal(cumul_X3[, 1:8], mat_cumsum_columns_with_scalar_ceiling(X3, ceil = 8.2))
})


test_that("mat_cumsum_columns_with_vector_ceiling() works", {
  X <- matrix(rep(1, 10), ncol = 1)
  cumul_tX <- matrix(1:10, nrow = 1)

  # test one-row matrix
  expect_equal(
    matrix(1:10, nrow = 1),
    mat_cumsum_columns_with_vector_ceiling(t(X), ceil = Inf)
  )
  expect_equal(
    matrix(1:8, nrow = 1),
    mat_cumsum_columns_with_vector_ceiling(t(X), ceil = 8.2)
  )

  # test one-column matrix
  expect_equal(
    X,
    mat_cumsum_columns_with_vector_ceiling(X, ceil = Inf)
  )
  expect_equal(
    matrix(c(NA, X[2:10, 1]), ncol = 1),
    mat_cumsum_columns_with_vector_ceiling(X, ceil = c(0.5, rep(8.2, 9)))
  )


  X2 <- X3 <- matrix(rep(1, 100), ncol = 10)
  cumul_X2 <- cumul_X3 <- matrix(rep(cumul_tX, 10), byrow = TRUE, ncol = 10)
  expect_equal(cumul_X2, mat_cumsum_columns_with_vector_ceiling(X2, ceil = Inf))

  X3[1, ] <- 9
  cumul_X3[1, ] <- NA
  expect_equal(cumul_X3[, 1:8], mat_cumsum_columns_with_vector_ceiling(X3, ceil = (802:811) / 100))
})
