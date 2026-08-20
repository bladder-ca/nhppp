test_that("vdraw_sc_step() works with shared irregular breaks", {
  set.seed(123)
  b <- c(100, 100.5, 102, 106, 109, 110)
  l <- matrix(rep(1, 50), ncol = 5)
  L <- matrix(rep(cumsum(c(0.5, 1.5, 4, 3, 1)), each = 10), nrow = 10)

  # 1-row matrix
  expect_no_error(Z0 <- vdraw_sc_step(
    Lambda_matrix = L[1, , drop = FALSE],
    time_breaks = b,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z0, t_min = 100, t_max = 110)

  expect_no_error(Z <- vdraw_sc_step(
    Lambda_matrix = L,
    time_breaks = b,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110)

  expect_no_error(Z <- vdraw_sc_step(
    lambda_matrix = l,
    time_breaks = b,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110)

  expect_no_error(Z <- vdraw_sc_step(
    lambda_matrix = l,
    time_breaks = b,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110, atmost1 = TRUE)

  expect_no_error(Z <- vdraw_sc_step(
    lambda_matrix = l,
    time_breaks = b,
    atmostB = 2
  ))
  expect_true(ncol(Z) <= 2)
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110)

  # atleast1 routes to the zero-truncated sampler
  expect_no_error(Z <- vdraw_sc_step(
    lambda_matrix = l * 0.001,
    time_breaks = b,
    atleast1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110, atleast1 = TRUE)
})


test_that("vdraw_sc_step() works with per-row breaks", {
  set.seed(123)
  b <- c(100, 100.5, 102, 106, 109, 110)
  B <- matrix(rep(b, each = 10), nrow = 10) + 0:9
  l <- matrix(rep(1, 50), ncol = 5)

  expect_no_error(Z <- vdraw_sc_step(
    lambda_matrix = l,
    time_breaks = B,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = B[, 1], t_max = B[, 6])

  # shared breaks as a vector and as a replicated matrix agree exactly
  Bs <- matrix(rep(b, each = 10), nrow = 10)
  set.seed(123)
  Z1 <- vdraw_sc_step(lambda_matrix = l, time_breaks = b)
  set.seed(123)
  Z2 <- vdraw_sc_step(lambda_matrix = l, time_breaks = Bs)
  expect_true(identical(Z1, Z2))
})


test_that("vdraw_sc_step() does not modify its arguments and accepts list-mode matrices", {
  set.seed(123)
  b <- bref <- c(100, 100.5, 102, 106, 109, 110)
  l <- lref <- matrix(rep(1, 50), ncol = 5)
  L <- Lref <- matrix(rep(cumsum(c(0.5, 1.5, 4, 3, 1)), each = 10), nrow = 10)

  expect_no_error(Z <- vdraw_sc_step(lambda_matrix = l, time_breaks = b))
  expect_no_error(Z <- vdraw_sc_step(Lambda_matrix = L, time_breaks = b))
  expect_identical(l, lref)
  expect_identical(L, Lref)
  expect_identical(b, bref)

  mode(L) <- "list"
  expect_no_error(Z <- vdraw_sc_step(Lambda_matrix = L, time_breaks = b))
  mode(l) <- "list"
  expect_no_error(Z <- vdraw_sc_step(lambda_matrix = l, time_breaks = b))
})


test_that("vdraw_sc_step() validates its arguments", {
  l <- matrix(rep(1, 50), ncol = 5)
  b <- c(100, 100.5, 102, 106, 109, 110)

  # neither or both rate matrices
  expect_error(vdraw_sc_step(time_breaks = b))
  expect_error(vdraw_sc_step(lambda_matrix = l, Lambda_matrix = l, time_breaks = b))

  # missing breaks
  expect_error(vdraw_sc_step(lambda_matrix = l))

  # NA in rate or breaks
  l_na <- l
  l_na[1, 1] <- NA
  expect_error(vdraw_sc_step(lambda_matrix = l_na, time_breaks = b))
  b_na <- b
  b_na[2] <- NA
  expect_error(vdraw_sc_step(lambda_matrix = l, time_breaks = b_na))

  # wrong number of break columns
  expect_error(vdraw_sc_step(lambda_matrix = l, time_breaks = b[1:5]))

  # non-increasing breaks
  expect_error(vdraw_sc_step(lambda_matrix = l, time_breaks = rev(b)))
  expect_error(vdraw_sc_step(lambda_matrix = l, time_breaks = c(100, 100, 102, 106, 109, 110)))

  # wrong number of break rows
  B3 <- matrix(rep(b, each = 3), nrow = 3)
  expect_error(vdraw_sc_step(lambda_matrix = l, time_breaks = B3))

  # subinterval outside the breaks range
  expect_error(vdraw_sc_step(lambda_matrix = l, time_breaks = b, t_min = 99))
  expect_error(vdraw_sc_step(lambda_matrix = l, time_breaks = b, t_max = 111))
  expect_error(vdraw_sc_step(lambda_matrix = l, time_breaks = b, t_min = 106, t_max = 104))
})


test_that("vdraw_sc_step() uses blocked random numbers", {
  b <- c(100, 100.5, 102, 106, 109, 110)
  L <- matrix(rep(cumsum(c(0.5, 1.5, 4, 3, 1)), each = 100), nrow = 100)

  Z <- list()
  for (i in 1:2) {
    set.seed(123)
    expect_no_error(Z[[i]] <- vdraw_sc_step(
      Lambda_matrix = L,
      time_breaks = b,
      atmost1 = FALSE
    ))
    if (i > 1) {
      expect_true(identical(Z[[1]], Z[[i]]))
    }
  }
})


test_that("vdraw_sc_step() works with subintervals", {
  set.seed(123)
  b <- c(100, 100.5, 102, 106, 109, 110)
  l <- matrix(rep(1, 50), ncol = 5)

  # degenerate subinterval == full range
  expect_no_error(Z <- vdraw_sc_step(
    lambda_matrix = l, time_breaks = b, t_min = 100, t_max = 110
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110)

  # interior subinterval
  expect_no_error(Z <- vdraw_sc_step(
    lambda_matrix = l, time_breaks = b, t_min = 101.01, t_max = 108.99
  ))
  check_ppp_sample_validity(Z, t_min = 101.01, t_max = 108.99)

  # subinterval entirely inside one (wide) interval
  expect_no_error(Z <- vdraw_sc_step(
    lambda_matrix = l * 10, time_breaks = b, t_min = 102.2, t_max = 105.9
  ))
  check_ppp_sample_validity(Z, t_min = 102.2, t_max = 105.9)

  # per-row subinterval bounds
  expect_no_error(Z <- vdraw_sc_step(
    lambda_matrix = l, time_breaks = b,
    t_min = seq(100, 104.5, length.out = 10),
    t_max = seq(105.5, 110, length.out = 10)
  ))
  check_ppp_sample_validity(Z,
    t_min = seq(100, 104.5, length.out = 10),
    t_max = seq(105.5, 110, length.out = 10)
  )

  # measure-zero subinterval at the top break: no error, no events
  expect_no_error(Z <- vdraw_sc_step(
    lambda_matrix = l, time_breaks = b, t_min = 110, t_max = 110
  ))
  expect_true(all(is.na(Z)))
})


test_that("vdraw_sc_step() agrees with vdraw_sc_step_regular_cpp() on equal-spaced breaks", {
  b <- seq(100, 110, length.out = 6)
  l <- matrix(rep(1, 50), ncol = 5)
  L <- mat_cumsum_columns(l) # cumulative in "already integrated" units

  # same-seed, Lambda form: identical RNG consumption, only ~ulp arithmetic differences
  set.seed(123)
  Z1 <- vdraw_sc_step(Lambda_matrix = L, time_breaks = b)
  set.seed(123)
  Z2 <- vdraw_sc_step_regular_cpp(
    Lambda_matrix = L, rate_matrix_t_min = 100, rate_matrix_t_max = 110
  )
  expect_equal(Z1, Z2, tolerance = 1e-12)

  # same-seed, lambda form (interval widths enter the Lambda construction)
  set.seed(123)
  Z1 <- vdraw_sc_step(lambda_matrix = l, time_breaks = b)
  set.seed(123)
  Z2 <- vdraw_sc_step_regular_cpp(
    lambda_matrix = l, rate_matrix_t_min = 100, rate_matrix_t_max = 110
  )
  expect_equal(Z1, Z2, tolerance = 1e-8)

  # same-seed with subinterval
  set.seed(123)
  Z1 <- vdraw_sc_step(Lambda_matrix = L, time_breaks = b, t_min = 101.01, t_max = 108.99)
  set.seed(123)
  Z2 <- vdraw_sc_step_regular_cpp(
    Lambda_matrix = L, rate_matrix_t_min = 100, rate_matrix_t_max = 110,
    t_min = 101.01, t_max = 108.99
  )
  expect_equal(Z1, Z2, tolerance = 1e-12)
})


test_that("vdraw_sc_step() agrees with the scalar draw_sc_step()", {
  set.seed(123)
  n <- 2000
  lambda_vector <- c(0.5, 1, 2, 0.5, 1)
  b <- c(100, 100.5, 102, 106, 109, 110)

  Zv <- vdraw_sc_step(
    lambda_matrix = matrix(rep(lambda_vector, each = n), nrow = n),
    time_breaks = b
  )
  Zs <- unlist(lapply(1:n, function(i) {
    draw_sc_step(lambda_vector = lambda_vector, time_breaks = b)
  }))

  compare_ppp_vectors(ppp1 = as.vector(Zv), ppp2 = Zs, threshold = 0.1, showQQ = FALSE)

  # event-count agreement
  n_v <- sum(!is.na(Zv)) / n
  n_s <- length(Zs) / n
  expect_true(abs(n_v - n_s) / n_s < 0.1)
})
