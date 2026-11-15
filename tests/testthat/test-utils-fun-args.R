test_that(".resolve_fun_args() detects the container modes", {
  expect_identical(nhppp:::.resolve_fun_args(NULL)$mode, "none")
  expect_identical(nhppp:::.resolve_fun_args(list(exponent = 1))$mode, "flat")
  expect_identical(
    nhppp:::.resolve_fun_args(list(shared = list(a = 1)))$mode,
    "structured"
  )
  expect_identical(
    nhppp:::.resolve_fun_args(list(row_args = data.frame(x = 1:3)), n_draws = 3)$mode,
    "structured"
  )
  expect_identical(
    nhppp:::.resolve_fun_args(
      list(shared = list(a = 1), row_args = data.frame(x = 1:3)),
      n_draws = 3
    )$mode,
    "structured"
  )
  expect_warning(
    r <- nhppp:::.resolve_fun_args(
      list(vector_arguments = data.frame(x = 1:3)),
      n_draws = 3
    ),
    "deprecated"
  )
  expect_identical(r$mode, "legacy_va")

  # validation errors
  expect_error(nhppp:::.resolve_fun_args(list(shared = list(a = 1), other = 2)), "mixes")
  expect_error(nhppp:::.resolve_fun_args(list(shared = data.frame(a = 1))), "named list")
  expect_error(nhppp:::.resolve_fun_args(list(row_args = 1:3)), "data.frame")
  expect_error(
    nhppp:::.resolve_fun_args(list(row_args = data.frame(x = 1:3)), n_draws = 5),
    "3 rows but 5"
  )
  expect_error(nhppp:::.resolve_fun_args(data.frame(a = 1)), "named list")
})


test_that(".subset_fun_args() subsets only the per-row channel", {
  shared <- list(basis = matrix(1, 2, 2))
  ctr <- list(shared = shared, row_args = data.frame(x = 1:10))
  r <- nhppp:::.resolve_fun_args(ctr, n_draws = 10)

  out <- nhppp:::.subset_fun_args(r, rows = c(2L, 5L))
  expect_identical(out$shared, shared) # untouched (same object)
  expect_identical(out$row_args$x, c(2L, 5L))

  # NULL rows: the container passes through unchanged
  expect_identical(nhppp:::.subset_fun_args(r, rows = NULL), ctr)

  # flat containers pass through regardless of rows
  rf <- nhppp:::.resolve_fun_args(list(exponent = 1))
  expect_identical(nhppp:::.subset_fun_args(rf, rows = 1:2), list(exponent = 1))

  # legacy vector_arguments is subset
  suppressWarnings(
    rl <- nhppp:::.resolve_fun_args(
      list(vector_arguments = data.frame(x = 1:10)),
      n_draws = 10
    )
  )
  expect_identical(
    nhppp:::.subset_fun_args(rl, rows = 3:4)$vector_arguments$x,
    3:4
  )
})


test_that(".wrap_fun() and .call_with_args() honor the positional contract", {
  f <- function(x, a) x * a$shared$m
  ctr <- list(shared = list(m = 3))
  expect_identical(nhppp:::.wrap_fun(f, NULL), f)
  expect_identical(nhppp:::.wrap_fun(f, ctr)(2), 6)
  expect_identical(nhppp:::.call_with_args(function(x) x + 1, 2, NULL), 3)
  expect_identical(nhppp:::.call_with_args(f, 2, ctr), 6)
})


test_that("the rejection loops deliver a correctly row-subset container", {
  set.seed(20260902)
  N <- 500
  # this lambda ASSERTS the delivery contract on every call: the subset
  # row_args must align with the candidate matrix it is evaluated on
  lam <- function(t, a) {
    stopifnot(nrow(a$row_args) == nrow(t))
    a$shared$scale * t^a$row_args$exponent
  }
  ctr <- list(
    shared = list(scale = 0.2),
    row_args = data.frame(exponent = seq(0.5, 2, length.out = N))
  )
  lmaj <- matrix(5.5, nrow = N, ncol = 5)

  Z <- vdraw_intensity_step(
    lambda = lam, lambda_args = ctr, lambda_maj_matrix = lmaj,
    time_breaks = c(1, 1.5, 3, 4, 4.5, 5), generate_at_least_K = 2
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = 2)

  Z <- vdraw_intensity(
    lambda = lam, lambda_args = ctr, lambda_maj_matrix = lmaj,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5, generate_at_least_K = 2
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = 2)
})


test_that("legacy vector_arguments containers keep working, with one warning", {
  set.seed(20260903)
  N <- 300
  lfun <- function(x, a, ...) .2 * x^a$vector_arguments$exponent
  l_args <- list(
    vector_arguments = data.frame(exponent = seq(from = 0.5, to = 2, length.out = N))
  )
  lmaj <- matrix(5.5, nrow = N, ncol = 5)

  warns <- character(0)
  withCallingHandlers(
    Z <- vdraw_intensity(
      lambda = lfun, lambda_args = l_args, lambda_maj_matrix = lmaj,
      rate_matrix_t_min = 1, rate_matrix_t_max = 5, generate_at_least_K = 1
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warns, 1) # once, not once per rejection round
  expect_match(warns, "deprecated")
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleast1 = TRUE)

  # the general-grid loop too
  warns <- character(0)
  withCallingHandlers(
    Z <- vdraw_intensity_step(
      lambda = lfun, lambda_args = l_args, lambda_maj_matrix = lmaj,
      time_breaks = c(1, 1.5, 3, 4, 4.5, 5), generate_at_least_K = 1
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warns, 1)
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleast1 = TRUE)
})


test_that("vdraw_cumulative_intensity() delivers one structured container to both functions", {
  set.seed(20260904)
  L <- function(t, a) a$shared$r * t + t^a$row_args$p
  Li <- function(z, a) {
    # numerically trivial only when p = 1: Lambda = (r + 1) t
    z / (a$shared$r + 1)
  }
  N <- 40
  ctr <- list(shared = list(r = 0.5), row_args = data.frame(p = rep(1, N)))
  Z <- vdraw_cumulative_intensity(
    Lambda = L, Lambda_inv = Li,
    t_min = rep(0, N), t_max = rep(10, N),
    Lambda_args = ctr
  )
  check_ppp_sample_validity(Z, t_min = 0, t_max = 10)

  # flat containers keep the released named-argument behavior, with a warning
  Lold <- function(t, Lambda_args) Lambda_args$a * t
  Liold <- function(z, Lambda_inv_args) z / Lambda_inv_args$a
  expect_warning(
    Z <- vdraw_cumulative_intensity(
      Lambda = Lold, Lambda_inv = Liold,
      t_min = rep(0, 10), t_max = rep(10, 10),
      Lambda_args = list(a = 2), Lambda_inv_args = list(a = 2)
    ),
    "deprecated"
  )
  check_ppp_sample_validity(Z, t_min = 0, t_max = 10)

  # a flat Lambda_args alone also keeps the named-call behavior, with a warning
  Lflat <- function(t, Lambda_args) Lambda_args$a * t
  Liflat <- function(z, Lambda_inv_args = NULL) z / 2
  expect_warning(
    Z <- vdraw_cumulative_intensity(
      Lambda = Lflat, Lambda_inv = Liflat,
      t_min = rep(0, 10), t_max = rep(10, 10),
      Lambda_args = list(a = 2)
    ),
    "deprecated"
  )
  check_ppp_sample_validity(Z, t_min = 0, t_max = 10)
})


test_that("get_step_majorizer() accepts the same args container", {
  N <- 20L
  lam <- function(t, a) a$shared$scale * t^a$row_args$exponent
  ctr <- list(
    shared = list(scale = 0.2),
    row_args = data.frame(exponent = seq(0.5, 2, length.out = N))
  )
  mj <- get_step_majorizer(
    fun = lam, breaks = matrix(rep(1:6, each = N), nrow = N),
    is_monotone = FALSE, K = 1, fun_args = ctr
  )
  expect_identical(dim(mj), c(N, 5L))
  expect_true(all(mj > 0))
})
