## add tests that have lambda_maj < lambda
## add tests that have lamda_args

test_that("vdraw_intensity_step_regular_cpp() works", {
  lfun <- function(x, lambda_args, ...) .2 * x^lambda_args$exponent
  l_args <- list(exponent = 1L)
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = TRUE)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_args = l_args,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = TRUE
  ))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = TRUE)
})



test_that("vdraw_intensity_step_regular_cpp() does not break with matrices whose mode is list", {
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)

  mode(Lmaj) <- "list"
  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  mode(lmaj) <- "list"
  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5),
    tol = 10^-6,
    atmost1 = FALSE
  ))
})



test_that("vdraw_intensity_step_regular_cpp() works with subinterval", {
  lfun <- function(x, ...) .2 * x
  lmaj <- matrix(rep(1, 50), ncol = 5)
  Lmaj <- mat_cumsum_columns(lmaj)


  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    Lambda_maj_matrix = Lmaj,
    range_t = c(1, 5),
    subinterval = c(2.3, 4.8),
    tol = 10^-6,
    atmost1 = FALSE
  ))

  check_ppp_sample_validity(Z, t_min = 2.3, t_max = 4.8, atmost1 = FALSE)

  expect_no_error(Z <- vdraw_intensity_step_regular_cpp(
    lambda = lfun,
    lambda_maj_matrix = lmaj,
    range_t = c(1, 5),
    subinterval = c(2.3, 4.8),
    tol = 10^-6,
    atmost1 = FALSE
  ))
  check_ppp_sample_validity(Z, t_min = 2.3, t_max = 4.8, atmost1 = FALSE)
})


test_that("vdraw_intensity_step_regular_cpp() uses user-supplied RNGs", {
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
