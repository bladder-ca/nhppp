test_that("matrix_diff_columns[_inplace]() works", {
  set.seed(123)
  N <- 100
  J0 <- J <- matrix(rnorm(10 * N), ncol = 10)
  J_target <- J
  for (i in 10:2) {
    J_target[, i] <- J_target[, i] - J_target[, i - 1]
  }

  J_R <- mat_diff_columns(J)
  expect_equal(J_R, J_target) # The  R code version of the function
  expect_false(data.table::address(J_R) == data.table::address(J))
  expect_equal(J, J0)

  J_Cpp <- matrix_diff_columns(J)
  expect_equal(J_Cpp, J_target) # The C++ version of the function -- not inplace
  expect_false(data.table::address(J_Cpp) == data.table::address(J))
  expect_equal(J, J0)

  expect_no_error(matrix_diff_columns_inplace(J))
  expect_equal(J, J_target)

  # cumsum/diff cancel out
  expect_equal(J0, matrix_diff_columns(matrix_cumsum_columns(J0)))

  # works with 1 column:
  expect_equal(J0[, 1, drop = FALSE], mat_diff_columns(J0[, 1, drop = FALSE]))
  expect_equal(J0[, 1, drop = FALSE], matrix_diff_columns(J0[, 1, drop = FALSE]))
})
