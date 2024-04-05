test_that("get_step_majorizer() works", {
  # with breaks being a vector
  expect_no_error(l1 <- get_step_majorizer(
    fun = abs,
    breaks = -10:10, is_monotone = FALSE, K = 1
  ))
  l1_fun <- stats::approxfun(y = l1, x = -10:9, method = "constant", rule = 2)
  x <- seq(-10, 10, length.out = 1000)
  expect_true(sum(l1_fun(x) < abs(x)) == 0)


  # with breaks being a row_matrix
  expect_no_error(l1 <- get_step_majorizer(
    fun = abs,
    breaks = matrix(-10:10, nrow = 1), is_monotone = FALSE, K = 1
  ))
  l1_fun <- stats::approxfun(y = l1, x = -10:9, method = "constant", rule = 2)
  x <- seq(-10, 10, length.out = 1000)
  expect_true(sum(l1_fun(x) < abs(x)) == 0)

  # with breaks being multi-row matrix
  breaks_mat <- rbind(-10:10, (-10:10) / 2, (-10:10) * 3, (-10:10) - 20)

  expect_no_error(l1 <- get_step_majorizer(
    fun = abs,
    breaks = breaks_mat, is_monotone = FALSE, K = 1
  ))
  for (r in 1:4) {
    l1_fun <- stats::approxfun(y = l1[r, ], x = breaks_mat[r, 1:20], method = "constant", rule = 2)
    x <- seq(breaks_mat[r, 1], breaks_mat[r, 21], length.out = 1000)
    expect_true(sum(l1_fun(x) < abs(x)) == 0)
  }

  # with breaks being multi-row matrix with all rows the same
  expect_no_error(l1 <- get_step_majorizer(
    fun = abs,
    breaks = breaks_mat[rep(1, 4), ], is_monotone = FALSE, K = 1
  ))
  expect_identical(l1[1, ], l1[2, ])
  expect_identical(l1[1, ], l1[3, ])
  expect_identical(l1[1, ], l1[4, ])

  l1_fun <- stats::approxfun(y = l1[1, ], x = breaks_mat[1, 1:20], method = "constant", rule = 2)
  x <- seq(breaks_mat[1, 1], breaks_mat[1, 21], length.out = 1000)
  expect_true(sum(l1_fun(x) < abs(x)) == 0)
})
