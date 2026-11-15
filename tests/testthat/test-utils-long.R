test_that("long and dense outputs are same-seed identical across cores, grids, and options", {
  N <- 50
  lmat <- matrix(rep(c(0.5, 1, 0.2, 0.8, 0.4), each = N), nrow = N)
  breaks <- c(1, 1.5, 3, 4, 4.5, 5)

  fixtures <- list(
    list(f = vdraw_sc_step, args = list()),
    list(f = vdraw_sc_step, args = list(t_min = 2, t_max = 4.5)),
    list(f = vdraw_sc_step, args = list(report_first_K = 2)),
    list(f = vdraw_sc_step, args = list(report_last_K = 2)),
    list(f = vdraw_sc_step, args = list(generate_at_least_K = 2)),
    list(f = vdraw_sc_step, args = list(generate_at_most_K = 3)),
    list(f = vdraw_sc_step, args = list(generate_at_least_K = 2, generate_at_most_K = 2)),
    list(f = vdraw_sc_step, args = list(generate_at_least_K = 2, report_last_K = 1)),
    list(f = vztdraw_sc_step, args = list())
  )
  for (fx in fixtures) {
    base_args <- c(list(lambda_matrix = lmat, time_breaks = breaks), fx$args)
    set.seed(20260901)
    Zd <- do.call(fx$f, c(base_args, list(output = "matrix")))
    set.seed(20260901)
    Zl <- do.call(fx$f, c(base_args, list(output = "long")))
    expect_identical(nhppp:::.matrix_from_long(Zl), Zd)
  }

  # regular grid goes through the same sinks
  set.seed(20260902)
  Zd <- vdraw_sc_step_regular(
    lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    generate_at_least_K = 1
  )
  set.seed(20260902)
  Zl <- vdraw_sc_step_regular(
    lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    generate_at_least_K = 1, output = "long"
  )
  expect_identical(nhppp:::.matrix_from_long(Zl), Zd)
})


test_that("the long format represents zero-event processes by absence", {
  # rows 2 and 4 have zero rate: their ids must be absent
  lmat <- rbind(rep(1, 5), rep(0, 5), rep(1, 5), rep(0, 5))
  Zl <- vdraw_sc_step(
    lambda_matrix = lmat, time_breaks = seq(1, 5, length.out = 6),
    output = "long"
  )
  expect_identical(Zl$n_draws, 4L)
  expect_true(all(Zl$id %in% c(1L, 3L)))
  expect_false(anyNA(Zl$time))
  check_ppp_sample_validity(Zl, t_min = 1, t_max = 5)

  # all-empty draw: empty vectors, n_draws intact
  Zl <- vdraw_sc_step(
    lambda_matrix = matrix(0, nrow = 3, ncol = 5),
    time_breaks = seq(1, 5, length.out = 6), output = "long"
  )
  expect_identical(Zl, list(id = integer(0), time = numeric(0), n_draws = 3L))

  # a pure upper bound leaves N = 0 mass: some ids absent, none out of range
  set.seed(20260903)
  Zl <- vdraw_sc_step(
    lambda_matrix = matrix(1, nrow = 500, ncol = 5),
    time_breaks = seq(1, 5, length.out = 6),
    generate_at_most_K = 1, output = "long"
  )
  cnt <- tabulate(Zl$id, Zl$n_draws)
  expect_true(any(cnt == 0) && all(cnt <= 1))
})


test_that("the long converters round-trip ragged samples", {
  Z <- rbind(
    c(1.5, 2.5, 4.0),
    c(NA, NA, NA),
    c(2.0, NA, NA)
  )
  x <- nhppp:::.long_from_matrix(Z)
  expect_identical(x$id, c(1L, 1L, 1L, 3L))
  expect_identical(x$time, c(1.5, 2.5, 4.0, 2.0))
  expect_identical(x$n_draws, 3L)
  expect_identical(nhppp:::.matrix_from_long(x), Z)

  # empty round trip keeps the 1-column NA convention
  e <- nhppp:::.long_from_matrix(matrix(NA_real_, 2, 1))
  expect_identical(e, list(id = integer(0), time = numeric(0), n_draws = 2L))
  expect_identical(nhppp:::.matrix_from_long(e), matrix(NA_real_, 2, 1))
})


test_that("the long validity checker enforces the contracts", {
  ok <- list(id = c(1L, 1L, 3L), time = c(1.5, 2.5, 2.0), n_draws = 3L)
  expect_no_error(check_ppp_sample_validity(ok, t_min = 1, t_max = 5))

  fails <- function(x, ...) {
    inherits(
      tryCatch(
        {
          testthat::capture_output(check_ppp_sample_validity(x, ...))
          NULL
        },
        expectation_failure = function(e) e
      ),
      "expectation_failure"
    )
  }
  bad_order <- list(id = c(1L, 1L, 3L), time = c(2.5, 1.5, 2.0), n_draws = 3L)
  bad_group <- list(id = c(1L, 3L, 1L), time = c(1.5, 2.0, 2.5), n_draws = 3L)
  bad_range <- list(id = c(1L, 4L), time = c(1.5, 2.0), n_draws = 3L)
  expect_true(fails(bad_order, t_min = 1, t_max = 5))
  expect_true(fails(bad_group, t_min = 1, t_max = 5))
  expect_true(fails(bad_range, t_min = 1, t_max = 5))
  expect_true(fails(ok, t_min = 2, t_max = 5)) # below t_min
  expect_true(fails(ok, t_min = 1, t_max = 2)) # above t_max
  expect_true(fails(ok, t_min = 1, t_max = 5, atleast1 = TRUE)) # id 2 empty
  expect_true(fails(ok, t_min = 1, t_max = 5, atmost1 = TRUE)) # id 1 has 2
  expect_no_error(check_ppp_sample_validity(ok, t_min = 1, t_max = 5, atmostk = 2))
})


test_that("vdraw_cumulative_intensity() long output matches the dense law", {
  skip_on_cran()
  set.seed(20260904)
  N <- 10000
  L <- function(t, a) a$shared$r * t
  Li <- function(z, a) z / a$shared$r
  ctr <- list(shared = list(r = 0.8))

  Zl <- vdraw_cumulative_intensity(
    Lambda = L, Lambda_inv = Li, t_min = rep(0, N), t_max = rep(5, N),
    Lambda_args = ctr, generate_at_least_K = 2, output = "long"
  )
  check_ppp_sample_validity(Zl, t_min = 0, t_max = 5, atleastk = 2)
  Zd <- vdraw_cumulative_intensity(
    Lambda = L, Lambda_inv = Li, t_min = rep(0, N), t_max = rep(5, N),
    Lambda_args = ctr, generate_at_least_K = 2
  )
  compare_ppp_vectors(ppp1 = Zl$time, ppp2 = Zd, threshold = 0.1, showQQ = FALSE)
  # count law agreement too
  cnt_l <- tabulate(Zl$id, Zl$n_draws)
  cnt_d <- rowSums(!is.na(Zd))
  expect_gt(
    suppressWarnings(stats::ks.test(cnt_l, cnt_d))$p.value,
    0.001
  )

  # per-event row_args alignment on the conditioned long path
  Lp <- function(t, a) a$row_args$r * t
  Lip <- function(z, a) z / a$row_args$r
  r_row <- c(rep(0.5, 100), rep(2, 100))
  Zl <- vdraw_cumulative_intensity(
    Lambda = Lp, Lambda_inv = Lip, t_min = rep(0, 200), t_max = rep(5, 200),
    Lambda_args = list(row_args = data.frame(r = r_row)),
    generate_at_least_K = 1, output = "long"
  )
  check_ppp_sample_validity(Zl, t_min = 0, t_max = 5, atleast1 = TRUE)

  # reporting composes with the long conditioned path
  Zl <- vdraw_cumulative_intensity(
    Lambda = L, Lambda_inv = Li, t_min = rep(0, 50), t_max = rep(5, 50),
    Lambda_args = ctr, generate_at_least_K = 3, report_first_K = 2, output = "long"
  )
  expect_true(all(tabulate(Zl$id, Zl$n_draws) == 2))

  # unconditioned path: same-seed dense/long agreement via conversion
  set.seed(1)
  Zd <- vdraw_cumulative_intensity(
    Lambda = function(t) 2 * t, Lambda_inv = function(z) z / 2,
    t_min = rep(0, 100), t_max = rep(3, 100)
  )
  set.seed(1)
  Zl <- vdraw_cumulative_intensity(
    Lambda = function(t) 2 * t, Lambda_inv = function(z) z / 2,
    t_min = rep(0, 100), t_max = rep(3, 100), output = "long"
  )
  expect_identical(nhppp:::.matrix_from_long(Zl), Zd)
})


test_that("vdraw() routes and guards the output option", {
  set.seed(2)
  Zl <- vdraw(
    Lambda = function(t) 2 * t, Lambda_inv = function(z) z / 2,
    t_min = 0, t_max = rep(3, 10), atleast1 = TRUE, output = "long"
  )
  check_ppp_sample_validity(Zl, t_min = 0, t_max = 3, atleast1 = TRUE)

  expect_error(
    vdraw(
      lambda = function(t, ...) rep(1, length(t)),
      lambda_maj_matrix = matrix(2, 2, 5),
      rate_matrix_t_min = 0, rate_matrix_t_max = 5, output = "long"
    ),
    "not been implemented"
  )
  expect_error(
    vdraw_sc_step(
      lambda_matrix = matrix(1, 2, 5), time_breaks = 0:5, output = "wide"
    ),
    "'arg' should be one of"
  )
})
