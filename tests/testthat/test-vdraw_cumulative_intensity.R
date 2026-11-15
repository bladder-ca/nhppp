test_that("vdraw_cumulative_intensity_inversion() works with minimal options", {
  L <- function(t, ...) {
    return(2 * t)
  }
  Li <- function(z, ...) {
    return(z / 2)
  }

  t0_a <- 0.5
  t1_a <- 1.5
  t0_b <- rep(0.5, 10) + runif(n = 10)
  t1_b <- rep(2, 10) + runif(n = 10)
  t0_c <- matrix(t0_b, nrow = 1)
  t1_c <- matrix(t1_b, nrow = 1)
  t0_d <- matrix(t0_b, ncol = 1)
  t1_d <- matrix(t1_b, ncol = 1)


  # scalars
  expect_no_error(df0 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_a, t_max = t1_a, atmost1 = FALSE))
  check_ppp_sample_validity(df0, t_min = t0_a, t_max = t1_a)

  # vectors
  expect_no_error(df1 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_b, t_max = t1_b, atmost1 = FALSE))
  check_ppp_sample_validity(df1, t_min = t0_b, t_max = t1_b)

  # row matrices
  expect_no_error(df10 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_c, t_max = t1_c, atmost1 = FALSE))
  check_ppp_sample_validity(df10, t_min = t0_b, t_max = t1_b)

  # col matrices
  expect_no_error(df10 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_d, t_max = t1_d, atmost1 = FALSE))
  check_ppp_sample_validity(df10, t_min = t0_b, t_max = t1_b)


  # mixed arguments and atmost1 = TRUE
  expect_no_error(df10.1 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_a, t_max = t1_d, atmost1 = TRUE))
  check_ppp_sample_validity(df10.1, t_min = t0_a, t_max = t1_b, atmost1 = TRUE)
})

test_that("vdraw_cumulative_intensity_inversion() works when functions take arguments", {
  L <- function(t, Lambda_args) {
    return(Lambda_args$a * t)
  }
  Li <- function(z, Lambda_inv_args) {
    return(z / Lambda_inv_args$a)
  }

  args <- list(a = 2)

  t0_a <- 0.5
  t1_a <- 1.5
  t0_b <- rep(0.5, 10) + runif(n = 10)
  t1_b <- rep(2, 10) + runif(n = 10)

  # scalars; the released flat-list/Lambda_inv_args form now warns
  expect_warning(
    df0 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_a, t_max = t1_a, Lambda_args = args, Lambda_inv_args = args, atmost1 = FALSE),
    "deprecated"
  )
  check_ppp_sample_validity(df0, t_min = t0_a, t_max = t1_a)
  # vectors
  expect_warning(
    df1 <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_b, t_max = t1_b, Lambda_args = args, Lambda_inv_args = args, atmost1 = FALSE),
    "deprecated"
  )
  check_ppp_sample_validity(df1, t_min = t0_b, t_max = t1_b)

  # the structured container is delivered positionally to both functions
  Ls <- function(t, a) a$shared$a * t
  Lis <- function(z, a) z / a$shared$a
  expect_no_error(
    df2 <- vdraw_cumulative_intensity(
      Lambda = Ls, Lambda_inv = Lis, t_min = t0_b, t_max = t1_b,
      Lambda_args = list(shared = list(a = 2)), atmost1 = FALSE
    )
  )
  check_ppp_sample_validity(df2, t_min = t0_b, t_max = t1_b)
})

test_that("vdraw_cumulative_intensity_inversion() uses blocked random numbers", {
  set.seed(123)
  L <- function(t, ...) {
    return(2 * t)
  }
  Li <- function(z, ...) {
    return(z / 2)
  }

  t0_a <- 0.5
  t1_a <- 1.5
  t0_b <- rep(0.5, 10) + runif(n = 10)
  t1_b <- rep(2, 10) + runif(n = 10)


  Z0 <- list()
  for (i in 1:2) {
    set.seed(123)
    expect_no_error(Z0[[i]] <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0_b, t_max = t1_b, atmost1 = FALSE))
    if (i > 1) {
      expect_true(identical(Z0[[1]], Z0[[i]]))
    }
  }
  check_ppp_sample_validity(Z0[[1]], t_min = t0_b, t_max = t1_b)
})

test_that("vdraw_cumulative_intensity() honors the generation bounds", {
  skip_on_cran()
  set.seed(20260830)
  N <- 10000
  L <- function(t, ...) 0.8 * t # constant rate 0.8 on (0, 5): Lambda_total = 4
  Li <- function(z, ...) z / 0.8
  t0 <- rep(0, N)
  t1 <- rep(5, N)

  # at-least-K: K-truncated Poisson counts, uniform times
  k <- 3L
  Z <- vdraw_cumulative_intensity(
    Lambda = L, Lambda_inv = Li, t_min = t0, t_max = t1,
    generate_at_least_K = k
  )
  check_ppp_sample_validity(Z, t_min = 0, t_max = 5, atleastk = k)
  counts <- rowSums(!is.na(Z))
  sup <- k:max(counts)
  pmf <- dpois(sup, 4) / ppois(k - 1, 4, lower.tail = FALSE)
  obs <- tabulate(factor(counts, levels = sup), nbins = length(sup))
  chi <- suppressWarnings(stats::chisq.test(obs, p = pmf, rescale.p = TRUE))
  expect_gt(chi$p.value, 0.001)
  expect_gt(
    suppressWarnings(stats::ks.test(as.vector(Z[!is.na(Z)]), "punif", 0, 5))$p.value,
    0.001
  )

  # pure upper bound: right-truncated Poisson counts, N = 0 has positive mass
  Z <- vdraw_cumulative_intensity(
    Lambda = L, Lambda_inv = Li, t_min = t0, t_max = t1,
    generate_at_most_K = 2
  )
  counts <- rowSums(!is.na(Z))
  expect_true(all(counts <= 2))
  expect_true(any(counts == 0))
  sup <- 0:2
  pmf <- dpois(sup, 4) / ppois(2, 4)
  obs <- tabulate(factor(counts, levels = sup), nbins = length(sup))
  chi <- suppressWarnings(stats::chisq.test(obs, p = pmf, rescale.p = TRUE))
  expect_gt(chi$p.value, 0.001)

  # doubly truncated [2, 4]
  Z <- vdraw_cumulative_intensity(
    Lambda = L, Lambda_inv = Li, t_min = t0, t_max = t1,
    generate_at_least_K = 2, generate_at_most_K = 4
  )
  counts <- rowSums(!is.na(Z))
  expect_true(all(counts >= 2 & counts <= 4))
  sup <- 2:4
  pmf <- dpois(sup, 4) / sum(dpois(sup, 4))
  obs <- tabulate(factor(counts, levels = sup), nbins = length(sup))
  chi <- suppressWarnings(stats::chisq.test(obs, p = pmf, rescale.p = TRUE))
  expect_gt(chi$p.value, 0.001)

  # distributional agreement with the conditioned sc-step sampler
  Z1 <- vdraw_cumulative_intensity(
    Lambda = function(t, ...) t, Lambda_inv = function(z, ...) z,
    t_min = rep(1, N), t_max = rep(5, N),
    generate_at_least_K = 2, generate_at_most_K = 5
  )
  Z2 <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = matrix(rep(1, 5 * N), ncol = 5),
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    generate_at_least_K = 2, generate_at_most_K = 5
  )
  compare_ppp_vectors(ppp1 = Z1, ppp2 = Z2, threshold = 0.1, showQQ = FALSE)
})

test_that("exactly-K conditioning is exact for a nonlinear Lambda", {
  skip_on_cran()
  set.seed(20260831)
  N <- 10000
  K <- 2L
  # lambda(t) = t on (0, 3): given N = K the times are iid with CDF t^2 / 9,
  # and the first event has CDF 1 - (1 - t^2 / 9)^K
  Z <- vdraw_cumulative_intensity(
    Lambda = function(t, ...) t^2 / 2, Lambda_inv = function(z, ...) sqrt(2 * z),
    t_min = rep(0, N), t_max = rep(3, N),
    generate_at_least_K = K, generate_at_most_K = K
  )
  expect_true(all(rowSums(!is.na(Z)) == K))
  expect_gt(
    suppressWarnings(stats::ks.test(as.vector(Z), function(q) q^2 / 9))$p.value,
    0.001
  )
  expect_gt(
    suppressWarnings(stats::ks.test(Z[, 1], function(q) 1 - (1 - q^2 / 9)^K))$p.value,
    0.001
  )
})

test_that("vdraw_cumulative_intensity() reporting truncations", {
  L <- function(t, ...) 2 * t
  Li <- function(z, ...) z / 2
  N <- 200
  t0 <- rep(0, N)
  t1 <- rep(3, N)

  # report_last_K consumes the same RNG stream as the unrestricted draw and
  # compacts the last K events per row
  set.seed(101)
  Z_full <- vdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = t0, t_max = t1)
  set.seed(101)
  Z_last <- vdraw_cumulative_intensity(
    Lambda = L, Lambda_inv = Li, t_min = t0, t_max = t1, report_last_K = 2
  )
  expect_identical(Z_last, nhppp:::.report_slice(Z_full, list(first = 0L, last = 2L)))
  check_ppp_sample_validity(Z_last, t_min = 0, t_max = 3, atmostk = 2)

  # reporting composes with generation bounds
  Z <- vdraw_cumulative_intensity(
    Lambda = L, Lambda_inv = Li, t_min = t0, t_max = t1,
    generate_at_least_K = 3, report_first_K = 2
  )
  check_ppp_sample_validity(Z, t_min = 0, t_max = 3)
  expect_true(all(rowSums(!is.na(Z)) == 2))
  Z <- vdraw_cumulative_intensity(
    Lambda = L, Lambda_inv = Li, t_min = t0, t_max = t1,
    generate_at_least_K = 3, report_last_K = 2
  )
  check_ppp_sample_validity(Z, t_min = 0, t_max = 3)
  expect_true(all(rowSums(!is.na(Z)) == 2))

  # atleast1 alias routes to the conditioned path
  Z <- vdraw_cumulative_intensity(
    Lambda = L, Lambda_inv = Li, t_min = t0, t_max = t1, atleast1 = TRUE
  )
  check_ppp_sample_validity(Z, t_min = 0, t_max = 3, atleast1 = TRUE)
})

test_that("the conditioned path delivers the args containers", {
  set.seed(20260901)
  N <- 50
  # structured container, positional to both functions
  Ls <- function(t, a) a$shared$r * t
  Lis <- function(z, a) z / a$shared$r
  Z <- vdraw_cumulative_intensity(
    Lambda = Ls, Lambda_inv = Lis,
    t_min = rep(0, N), t_max = rep(10, N),
    Lambda_args = list(shared = list(r = 0.5)),
    generate_at_least_K = 2
  )
  check_ppp_sample_validity(Z, t_min = 0, t_max = 10, atleastk = 2)

  # legacy named-argument call keeps working, with a deprecation warning
  Lold <- function(t, Lambda_args) Lambda_args$a * t
  Liold <- function(z, Lambda_inv_args) z / Lambda_inv_args$a
  expect_warning(
    Z <- vdraw_cumulative_intensity(
      Lambda = Lold, Lambda_inv = Liold,
      t_min = rep(0, N), t_max = rep(10, N),
      Lambda_args = list(a = 2), Lambda_inv_args = list(a = 2),
      generate_at_least_K = 2
    ),
    "deprecated"
  )
  check_ppp_sample_validity(Z, t_min = 0, t_max = 10, atleastk = 2)
})
