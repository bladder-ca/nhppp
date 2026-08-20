test_that("vztdraw_sc_step() works with shared irregular breaks", {
  set.seed(123)
  b <- c(100, 100.5, 102, 106, 109, 110)
  l <- matrix(rep(1, 50), ncol = 5)
  L <- matrix(rep(cumsum(c(0.5, 1.5, 4, 3, 1)), each = 10), nrow = 10)

  # 1-row matrix
  expect_no_error(Z0 <- vztdraw_sc_step(
    Lambda_matrix = L[1, , drop = FALSE],
    time_breaks = b,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z0, t_min = 100, t_max = 110, atleast1 = TRUE)

  expect_no_error(Z <- vztdraw_sc_step(
    Lambda_matrix = L,
    time_breaks = b,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110, atleast1 = TRUE)

  expect_no_error(Z <- vztdraw_sc_step(
    lambda_matrix = l,
    time_breaks = b,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110, atleast1 = TRUE)

  expect_no_error(Z <- vztdraw_sc_step(
    lambda_matrix = l,
    time_breaks = b,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110, atmost1 = TRUE, atleast1 = TRUE)

  # tiny total rate: the zero-truncated count is (almost surely) exactly 1
  expect_no_error(Z <- vztdraw_sc_step(
    lambda_matrix = l * 0.001,
    time_breaks = b,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110, atleast1 = TRUE)
})


test_that("vztdraw_sc_step() works with per-row breaks", {
  set.seed(123)
  b <- c(100, 100.5, 102, 106, 109, 110)
  B <- matrix(rep(b, each = 10), nrow = 10) + 0:9
  l <- matrix(rep(1, 50), ncol = 5)

  expect_no_error(Z <- vztdraw_sc_step(
    lambda_matrix = l,
    time_breaks = B,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = B[, 1], t_max = B[, 6], atleast1 = TRUE)

  # shared breaks as a vector and as a replicated matrix agree exactly
  Bs <- matrix(rep(b, each = 10), nrow = 10)
  set.seed(123)
  Z1 <- vztdraw_sc_step(lambda_matrix = l, time_breaks = b)
  set.seed(123)
  Z2 <- vztdraw_sc_step(lambda_matrix = l, time_breaks = Bs)
  expect_true(identical(Z1, Z2))
})


test_that("vztdraw_sc_step() does not modify its arguments and accepts list-mode matrices", {
  set.seed(123)
  b <- bref <- c(100, 100.5, 102, 106, 109, 110)
  l <- lref <- matrix(rep(1, 50), ncol = 5)
  L <- Lref <- matrix(rep(cumsum(c(0.5, 1.5, 4, 3, 1)), each = 10), nrow = 10)

  expect_no_error(Z <- vztdraw_sc_step(lambda_matrix = l, time_breaks = b))
  expect_no_error(Z <- vztdraw_sc_step(Lambda_matrix = L, time_breaks = b))
  expect_identical(l, lref)
  expect_identical(L, Lref)
  expect_identical(b, bref)

  mode(L) <- "list"
  expect_no_error(Z <- vztdraw_sc_step(Lambda_matrix = L, time_breaks = b))
  mode(l) <- "list"
  expect_no_error(Z <- vztdraw_sc_step(lambda_matrix = l, time_breaks = b))
})


test_that("vztdraw_sc_step() validates its arguments", {
  l <- matrix(rep(1, 50), ncol = 5)
  b <- c(100, 100.5, 102, 106, 109, 110)

  expect_error(vztdraw_sc_step(time_breaks = b))
  expect_error(vztdraw_sc_step(lambda_matrix = l, Lambda_matrix = l, time_breaks = b))
  expect_error(vztdraw_sc_step(lambda_matrix = l))
  expect_error(vztdraw_sc_step(lambda_matrix = l, time_breaks = b[1:5]))
  expect_error(vztdraw_sc_step(lambda_matrix = l, time_breaks = rev(b)))
  expect_error(vztdraw_sc_step(lambda_matrix = l, time_breaks = matrix(rep(b, each = 3), nrow = 3)))
  expect_error(vztdraw_sc_step(lambda_matrix = l, time_breaks = b, t_min = 99))
  expect_error(vztdraw_sc_step(lambda_matrix = l, time_breaks = b, t_max = 111))
})


test_that("vztdraw_sc_step() uses blocked random numbers", {
  b <- c(100, 100.5, 102, 106, 109, 110)
  L <- matrix(rep(cumsum(c(0.5, 1.5, 4, 3, 1)), each = 100), nrow = 100)

  Z <- list()
  for (i in 1:2) {
    set.seed(123)
    expect_no_error(Z[[i]] <- vztdraw_sc_step(
      Lambda_matrix = L,
      time_breaks = b,
      atmost1 = FALSE
    ))
    if (i > 1) {
      expect_true(identical(Z[[1]], Z[[i]]))
    }
  }
})


test_that("vztdraw_sc_step() works with subintervals", {
  set.seed(123)
  b <- c(100, 100.5, 102, 106, 109, 110)
  l <- matrix(rep(1, 50), ncol = 5)

  # degenerate subinterval == full range
  expect_no_error(Z <- vztdraw_sc_step(
    lambda_matrix = l, time_breaks = b, t_min = 100, t_max = 110
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110, atleast1 = TRUE)

  # interior subinterval
  expect_no_error(Z <- vztdraw_sc_step(
    lambda_matrix = l, time_breaks = b, t_min = 101.01, t_max = 108.99
  ))
  check_ppp_sample_validity(Z, t_min = 101.01, t_max = 108.99, atleast1 = TRUE)

  # subinterval entirely inside one (wide) interval, atmost1
  expect_no_error(Z <- vztdraw_sc_step(
    lambda_matrix = l, time_breaks = b, t_min = 102.2, t_max = 105.9, atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 102.2, t_max = 105.9, atmost1 = TRUE, atleast1 = TRUE)

  # per-row subinterval bounds
  expect_no_error(Z <- vztdraw_sc_step(
    lambda_matrix = l, time_breaks = b,
    t_min = seq(100, 104.5, length.out = 10),
    t_max = seq(105.5, 110, length.out = 10)
  ))
  check_ppp_sample_validity(Z,
    t_min = seq(100, 104.5, length.out = 10),
    t_max = seq(105.5, 110, length.out = 10),
    atleast1 = TRUE
  )

  # near-degenerate subinterval at the top break: events stay in bounds
  expect_no_error(Z <- vztdraw_sc_step(
    lambda_matrix = l, time_breaks = b, t_min = 109.99, t_max = 110
  ))
  check_ppp_sample_validity(Z, t_min = 109.99, t_max = 110, atleast1 = TRUE)

  # measure-zero subinterval: the zero-truncated count degenerates to 0
  expect_no_error(Z <- vztdraw_sc_step(
    lambda_matrix = l, time_breaks = b, t_min = 110, t_max = 110
  ))
  expect_true(all(is.na(Z)))
})


test_that("vztdraw_sc_step() agrees with vztdraw_sc_step_regular_cpp() on equal-spaced breaks", {
  b <- seq(100, 110, length.out = 6)
  l <- matrix(rep(1, 50), ncol = 5)
  L <- mat_cumsum_columns(l)

  # same-seed, Lambda form: identical RNG consumption, only ~ulp arithmetic differences
  set.seed(123)
  Z1 <- vztdraw_sc_step(Lambda_matrix = L, time_breaks = b)
  set.seed(123)
  Z2 <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = L, rate_matrix_t_min = 100, rate_matrix_t_max = 110
  )
  expect_equal(Z1, Z2, tolerance = 1e-12)

  # distributional agreement on a subinterval, independent seeds
  set.seed(1)
  Z1 <- vztdraw_sc_step(
    Lambda_matrix = matrix(rep(L[1, ], each = 1000), nrow = 1000),
    time_breaks = b, t_min = 101.01, t_max = 108.99
  )
  set.seed(2)
  Z2 <- vztdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(rep(L[1, ], each = 1000), nrow = 1000),
    rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    t_min = 101.01, t_max = 108.99
  )
  compare_ppp_vectors(
    ppp1 = as.vector(Z1), ppp2 = as.vector(Z2),
    threshold = 0.1, showQQ = FALSE
  )
})


test_that("vztdraw_sc_step() agrees with the scalar draw_sc_step(atleast1 = TRUE)", {
  set.seed(123)
  n <- 2000
  lambda_vector <- c(0.5, 1, 2, 0.5, 1)
  b <- c(100, 100.5, 102, 106, 109, 110)

  Zv <- vztdraw_sc_step(
    lambda_matrix = matrix(rep(lambda_vector, each = n), nrow = n),
    time_breaks = b
  )
  Zs <- unlist(lapply(1:n, function(i) {
    draw_sc_step(lambda_vector = lambda_vector, time_breaks = b, atleast1 = TRUE)
  }))

  compare_ppp_vectors(ppp1 = as.vector(Zv), ppp2 = Zs, threshold = 0.1, showQQ = FALSE)
})
