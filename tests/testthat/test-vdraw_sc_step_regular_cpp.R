test_that("vdraw_sc_step_regular_cpp() works", {
  set.seed(123)
  # 1 row matrix
  expect_no_error(Z0 <- vdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:5, nrow = 1),
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z0, t_min = 100, t_max = 110)


  l <- lref <- matrix(rep(1, 50), ncol = 5)
  L <- Lref <- mat_cumsum_columns(l)

  expect_no_error(Z <- vdraw_sc_step_regular_cpp(
    Lambda_matrix = L,
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 100, t_max = 110)

  expect_no_error(Z1 <- vdraw_sc_step_regular_cpp(
    Lambda_matrix = L,
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z1, t_min = 100, t_max = 110)

  expect_no_error(Z2 <- vdraw_sc_step_regular_cpp(
    lambda_matrix = l,
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z2, t_min = 100, t_max = 110)

  expect_equal(L, Lref) # no side effects on l, L
  expect_equal(l, lref)
})

test_that("vdraw_sc_step_regular_cpp() does not break with matrices whose mode is list", {
  set.seed(123)
  l <- matrix(rep(1, 50), ncol = 5)
  L <- mat_cumsum_columns(l)

  mode(L) <- "list"
  expect_no_error(Z <- vdraw_sc_step_regular_cpp(
    Lambda_matrix = L,
    range_t = c(100, 110),
    tol = 10^-6,
    atmost1 = FALSE
  ))
})



test_that("vdraw_sc_step_regular_cpp() uses blocked random numbers", {
  set.seed(123)
  l <- matrix(rep(1, 500), ncol = 5)
  L <- mat_cumsum_columns(l)

  Z <- list()
  for (i in 1:2) {
    set.seed(123)
    expect_no_error(Z[[i]] <- vdraw_sc_step_regular_cpp(
      Lambda_matrix = L,
      range_t = c(100, 110),
      tol = 10^-6,
      atmost1 = FALSE
    ))
    if (i > 1) {
      expect_true(identical(Z[[1]], Z[[i]]))
    }
  }
})


test_that("vdraw_sc_step_regular_cpp() works with subinterval", {
  set.seed(123)
  expect_no_error(Z0 <- vdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:5, nrow = 1),
    range_t = c(100, 110),
    subinterval = c(100, 110), # vector
    tol = 10^-6,
    atmost1 = FALSE
  ))
  expect_no_error(Z0 <- vdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:5, nrow = 1),
    range_t = cbind(100, 110),
    subinterval = cbind(100, 110), # 1-row matrix (was a bug)
    tol = 10^-6,
    atmost1 = FALSE
  ))

  check_ppp_sample_validity(Z0, t_min = 100, t_max = 110, atmost1 = FALSE)
  expect_no_error(Z0 <- vdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:5, nrow = 1),
    range_t = c(100, 110),
    subinterval = c(101.01, 108.99),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z0, t_min = 101.01, t_max = 108.99, atmost1 = FALSE)

  expect_no_error(Z0 <- vdraw_sc_step_regular_cpp(
    Lambda_matrix = matrix(1:5, nrow = 1) * 10,
    range_t = c(100, 110),
    subinterval = c(105.01, 105.99),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z0, t_min = 105.01, t_max = 105.99, atmost1 = FALSE)
})

test_that("vdraw_sc_step_regular_cpp() uses user-supplied RNGs", {
  l <- matrix(rep(1, 50), ncol = 5)
  L <- mat_cumsum_columns(l)

  seed <- rep(1, 6)
  rlecuyer::.lec.SetPackageSeed(seed)
  rlecuyer::.lec.CreateStream(c("a", "b"))

  res <- list()

  # seed the old.kind generator (default ) hereon "R"
  set.seed(123)
  res[["R0"]] <- vdraw_sc_step_regular_cpp(Lambda_matrix = L, range_t = c(100, 110))
  set.seed(123)
  res[["R1"]] <- vdraw_sc_step_regular_cpp(Lambda_matrix = L, range_t = c(100, 110))
  expect_equal(res[["R0"]], res[["R1"]])

  # activate "a" and reseed "R" -- If the function uses the "R", the test will fail
  set.seed(123)
  old.kind <- rlecuyer::.lec.CurrentStream("a")
  res[["a0"]] <- vdraw_sc_step_regular_cpp(Lambda_matrix = L, range_t = c(100, 110))
  rlecuyer::.lec.CurrentStreamEnd(old.kind)
  expect_false(identical(res[["R0"]], res[["a0"]]))

  # activate "b", again, re-seed "R"
  set.seed(123)
  old.kind <- rlecuyer::.lec.CurrentStream("b")
  res[["b0"]] <- vdraw_sc_step_regular_cpp(Lambda_matrix = L, range_t = c(100, 110))
  rlecuyer::.lec.CurrentStreamEnd(old.kind)
  expect_false(identical(res[["R0"]], res[["b0"]]))
  expect_false(identical(res[["b0"]], res[["a0"]]))

  # reset the RNGs -- but advance the "R"
  burn <- runif(10000)
  rm("burn")
  rlecuyer::.lec.ResetStartStream("a")
  rlecuyer::.lec.ResetStartStream("b")

  # activate "a" -- do not reseed "R"
  old.kind <- rlecuyer::.lec.CurrentStream("a")
  res[["a1"]] <- vdraw_sc_step_regular_cpp(Lambda_matrix = L, range_t = c(100, 110))
  rlecuyer::.lec.CurrentStreamEnd(old.kind)
  expect_equal(res[["a0"]], res[["a1"]])
  expect_false(identical(res[["R0"]], res[["a1"]]))

  # activate "b" -- do not reseed "R"
  old.kind <- rlecuyer::.lec.CurrentStream("b")
  res[["b1"]] <- vdraw_sc_step_regular_cpp(Lambda_matrix = L, range_t = c(100, 110))
  rlecuyer::.lec.CurrentStreamEnd(old.kind)
  expect_equal(res[["b0"]], res[["b1"]])
  expect_false(identical(res[["R0"]], res[["b1"]]))
  expect_false(identical(res[["a1"]], res[["b1"]]))

  rlecuyer::.lec.exit()
})
