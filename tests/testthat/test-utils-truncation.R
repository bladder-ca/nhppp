test_that("reporting / generation resolvers work", {
  expect_identical(nhppp:::.resolve_reporting(FALSE, NULL, NULL), list(first = 0L, last = 0L))
  expect_identical(nhppp:::.resolve_reporting(TRUE, NULL, NULL), list(first = 1L, last = 0L))
  expect_identical(nhppp:::.resolve_reporting(TRUE, 1, NULL), list(first = 1L, last = 0L))
  expect_identical(nhppp:::.resolve_reporting(FALSE, 3, NULL), list(first = 3L, last = 0L))
  expect_identical(nhppp:::.resolve_reporting(FALSE, NULL, 2), list(first = 0L, last = 2L))
  expect_error(nhppp:::.resolve_reporting(TRUE, 3, NULL), "contradicts")
  expect_error(nhppp:::.resolve_reporting(TRUE, NULL, 2), "contradicts")
  expect_error(nhppp:::.resolve_reporting(FALSE, 2, 2), "only one of")
  expect_error(nhppp:::.resolve_reporting(FALSE, 0, NULL), "positive integer")

  expect_identical(nhppp:::.resolve_generation(FALSE, NULL, NULL), list(at_least = 0L, at_most = 0L))
  expect_identical(nhppp:::.resolve_generation(TRUE, NULL, NULL), list(at_least = 1L, at_most = 0L))
  expect_identical(nhppp:::.resolve_generation(TRUE, 1, NULL), list(at_least = 1L, at_most = 0L))
  expect_identical(nhppp:::.resolve_generation(FALSE, 2, 5), list(at_least = 2L, at_most = 5L))
  expect_identical(nhppp:::.resolve_generation(FALSE, 3, 3), list(at_least = 3L, at_most = 3L))
  expect_error(nhppp:::.resolve_generation(TRUE, 3, NULL), "contradicts")
  expect_error(nhppp:::.resolve_generation(FALSE, 5, 2), "exceeds")
  expect_error(nhppp:::.resolve_generation(FALSE, 0, NULL), "positive integer")

  expect_identical(nhppp:::.resolve_budget_cap(NULL, NULL), 0L)
  expect_identical(nhppp:::.resolve_budget_cap(7, NULL), 7L)
  expect_warning(b <- nhppp:::.resolve_budget_cap(NULL, 7), "deprecated")
  expect_identical(b, 7L)
  expect_warning(
    expect_error(nhppp:::.resolve_budget_cap(5, 7), "disagree"),
    "deprecated"
  )
})


test_that("rbtpois() samples the doubly-truncated Poisson distribution", {
  skip_on_cran()
  set.seed(20260825)
  n <- 20000
  for (fixture in list(
    list(lambda = 2.5, k_min = 1, k_max = Inf), # zero-truncated
    list(lambda = 2.5, k_min = 3, k_max = Inf),
    list(lambda = 0.5, k_min = 2, k_max = Inf),
    list(lambda = 8, k_min = 12, k_max = Inf), # deep right tail
    list(lambda = 2.5, k_min = 0, k_max = 2), # pure upper truncation
    list(lambda = 3, k_min = 2, k_max = 5) # doubly truncated
  )) {
    lam <- fixture$lambda
    k1 <- fixture$k_min
    k2 <- fixture$k_max
    x <- rbtpois(n = n, lambda = lam, k_min = k1, k_max = k2)
    expect_true(all(x >= k1))
    if (is.finite(k2)) expect_true(all(x <= k2))

    sup <- k1:(if (is.finite(k2)) k2 else max(max(x), k1 + 10))
    region_mass <- if (is.finite(k2)) {
      sum(dpois(k1:k2, lam))
    } else if (k1 >= 1) {
      ppois(k1 - 1, lam, lower.tail = FALSE)
    } else {
      1
    }
    pmf <- dpois(sup, lam) / region_mass
    keep <- pmf * n >= 5
    if (!is.finite(k2)) keep[length(keep)] <- FALSE # pool the open tail
    obs <- c(tabulate(factor(x, levels = sup), nbins = length(sup))[keep], sum(!(x %in% sup[keep])))
    pr <- c(pmf[keep], max(0, 1 - sum(pmf[keep])))
    use <- pr > 0 | obs > 0
    chi <- suppressWarnings(stats::chisq.test(obs[use], p = pr[use], rescale.p = TRUE))
    expect_gt(chi$p.value, 0.001)
  }

  # exactly-K truncation is degenerate at K
  expect_true(all(rbtpois(1000, lambda = 4, k_min = 3, k_max = 3) == 3))

  # the C++ vectorized twin agrees on support (k_max = 0 means unbounded)
  x <- rbtpois_vec(rep(3, 5000), 2L, 5L)
  expect_true(all(x >= 2 & x <= 5))
  x <- rbtpois_vec(rep(2.5, 5000), 3L, 0L)
  expect_true(all(x >= 3))

  # rng_stream variant
  x <- rng_stream_rbtpois(size = 1000, lambda = 1, k_min = 0, k_max = 3)
  expect_true(all(x <= 3))
})


test_that("generate_at_least_K conditions the vectorized sc_step samplers", {
  skip_on_cran()
  set.seed(20260826)
  n_draws <- 10000
  lmat <- matrix(rep(1, 5 * n_draws), ncol = 5) # Lambda_total = 4 on (1, 5)

  for (k in c(1L, 3L)) {
    Z <- vztdraw_sc_step_regular_cpp(
      lambda_matrix = lmat,
      rate_matrix_t_min = 1, rate_matrix_t_max = 5,
      generate_at_least_K = k
    )
    check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = k)

    counts <- rowSums(!is.na(Z))
    sup <- k:max(counts)
    pmf <- dpois(sup, 4) / ppois(k - 1, 4, lower.tail = FALSE)
    obs <- tabulate(factor(counts, levels = sup), nbins = length(sup))
    chi <- suppressWarnings(stats::chisq.test(obs, p = pmf, rescale.p = TRUE))
    expect_gt(chi$p.value, 0.001)

    expect_gt(
      suppressWarnings(stats::ks.test(as.vector(Z[!is.na(Z)]), "punif", 1, 5))$p.value,
      0.001
    )
  }

  # general-grid path with irregular breaks
  Z <- vztdraw_sc_step(
    lambda_matrix = lmat[1:1000, ],
    time_breaks = c(1, 1.5, 3, 4, 4.2, 5),
    generate_at_least_K = 2
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = 2)

  # subinterval sampling conditions on the subinterval count
  Z <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat[1:1000, ],
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    t_min = 2, t_max = 3,
    generate_at_least_K = 2
  )
  check_ppp_sample_validity(Z, t_min = 2, t_max = 3, atleastk = 2)
})


test_that("generate_at_most_K conditions on a right-truncated count", {
  skip_on_cran()
  set.seed(20260827)
  n_draws <- 10000
  lmat <- matrix(rep(1, 5 * n_draws), ncol = 5) # Lambda_total = 4 on (1, 5)
  k2 <- 2L

  Z <- vdraw_sc_step(
    lambda_matrix = lmat, time_breaks = seq(1, 5, length.out = 6),
    generate_at_most_K = k2
  )
  counts <- rowSums(!is.na(Z))
  expect_true(all(counts <= k2))
  expect_true(any(counts == 0)) # N = 0 has positive mass under pure upper truncation

  sup <- 0:k2
  pmf <- dpois(sup, 4) / ppois(k2, 4)
  obs <- tabulate(factor(counts, levels = sup), nbins = length(sup))
  chi <- suppressWarnings(stats::chisq.test(obs, p = pmf, rescale.p = TRUE))
  expect_gt(chi$p.value, 0.001)

  # doubly truncated [2, 4]
  Z <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    generate_at_least_K = 2, generate_at_most_K = 4
  )
  counts <- rowSums(!is.na(Z))
  expect_true(all(counts >= 2 & counts <= 4))
  sup <- 2:4
  pmf <- dpois(sup, 4) / sum(dpois(sup, 4))
  obs <- tabulate(factor(counts, levels = sup), nbins = 3)
  chi <- suppressWarnings(stats::chisq.test(obs, p = pmf, rescale.p = TRUE))
  expect_gt(chi$p.value, 0.001)
})


test_that("exactly-K conditioning differs from reporting the first K of an at-least-K draw", {
  skip_on_cran()
  set.seed(20260828)
  n_draws <- 10000
  lmat <- matrix(rep(1, 5 * n_draws), ncol = 5) # constant rate 1 on (1, 5)
  K <- 2L
  # under a constant rate, given N = K the times are K iid U(1, 5);
  # the first event has CDF 1 - (1 - (t-1)/4)^K on (1, 5)
  cdf_min_of_K <- function(q) 1 - (1 - (q - 1) / 4)^K

  # exact conditioning on N = K: fits the closed form
  Z_exact <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    generate_at_least_K = K, generate_at_most_K = K
  )
  expect_true(all(rowSums(!is.na(Z_exact)) == K))
  expect_gt(
    suppressWarnings(stats::ks.test(Z_exact[, 1], cdf_min_of_K))$p.value,
    0.001
  )

  # reporting the first K of an N >= K draw: same count, DIFFERENT law
  # (the first event is the minimum of ztPois_K-many uniforms, biased early)
  Z_report <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    generate_at_least_K = K, report_first_K = K
  )
  expect_true(all(rowSums(!is.na(Z_report)) == K))
  expect_lt(
    suppressWarnings(stats::ks.test(Z_report[, 1], cdf_min_of_K))$p.value,
    1e-4
  )
})


test_that("report_first_K / report_last_K are reporting truncations", {
  skip_on_cran()
  set.seed(20260829)
  n_draws <- 5000
  lmat <- matrix(rep(1, 5 * n_draws), ncol = 5) # constant rate on (1, 5)

  Z_first <- vdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    report_first_K = 2
  )
  check_ppp_sample_validity(Z_first, t_min = 1, t_max = 5, atmostk = 2)

  Z_last <- vdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    report_last_K = 2
  )
  check_ppp_sample_validity(Z_last, t_min = 1, t_max = 5, atmostk = 2)

  # under a constant rate the process is symmetric under time reversal
  # about the interval midpoint, so reflecting the last-K sample must
  # reproduce the first-K law
  Z_reflected <- t(apply(6 - Z_last, 1, function(x) sort(x, na.last = TRUE)))
  compare_ppp_vectors(ppp1 = Z_first, ppp2 = Z_reflected, threshold = 0.1, showQQ = FALSE)

  # reporting also works on the conditioned (order-statistics) path
  Z <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    generate_at_least_K = 3, report_last_K = 2
  )
  expect_true(all(rowSums(!is.na(Z)) == 2))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmostk = 2)

  # atmost1 remains the alias of report_first_K = 1
  Z <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    atmost1 = TRUE
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = TRUE, atleast1 = TRUE)
})


test_that("scalar samplers honor the new options", {
  skip_on_cran()
  set.seed(20260830)

  x <- ztppp(rate = 0.2, t_min = 0, t_max = 10, generate_at_least_K = 4)
  check_ppp_vector_validity(x, t_min = 0, t_max = 10, atleastk = 4)

  x <- ztppp(rate = 2, t_min = 0, t_max = 10, generate_at_least_K = 2, generate_at_most_K = 4)
  check_ppp_vector_validity(x, t_min = 0, t_max = 10, atleastk = 2, atmostk = 4)

  counts <- replicate(5000, length(ztppp(rate = 0.5, t_min = 0, t_max = 2, generate_at_least_K = 2)))
  sup <- 2:max(counts)
  pmf <- dpois(sup, 1) / ppois(1, 1, lower.tail = FALSE)
  obs <- tabulate(factor(counts, levels = sup), nbins = length(sup))
  chi <- suppressWarnings(stats::chisq.test(obs, p = pmf, rescale.p = TRUE))
  expect_gt(chi$p.value, 0.001)

  x <- draw_sc_step(lambda_vector = rep(0.1, 5), time_breaks = 0:5, generate_at_least_K = 2)
  check_ppp_vector_validity(x, t_min = 0, t_max = 5, atleastk = 2)

  x <- draw_sc_step(lambda_vector = rep(2, 5), time_breaks = 0:5, report_first_K = 3)
  check_ppp_vector_validity(x, t_min = 0, t_max = 5, atmostk = 3)

  x <- draw_sc_step(lambda_vector = rep(2, 5), time_breaks = 0:5, report_last_K = 3)
  check_ppp_vector_validity(x, t_min = 0, t_max = 5, atmostk = 3)

  x <- draw_sc_step_regular(lambda_vector = rep(0.1, 5), t_min = 0, t_max = 5, generate_at_least_K = 2)
  check_ppp_vector_validity(x, t_min = 0, t_max = 5, atleastk = 2)

  x <- ztdraw_sc_linear(intercept = 0.1, slope = 0.01, t_min = 0, t_max = 5, generate_at_least_K = 3)
  check_ppp_vector_validity(x, t_min = 0, t_max = 5, atleastk = 3)

  x <- ztdraw_sc_loglinear(intercept = -2, slope = 0.01, t_min = 0, t_max = 5, generate_at_least_K = 3)
  check_ppp_vector_validity(x, t_min = 0, t_max = 5, atleastk = 3)

  x <- ztdraw_cumulative_intensity(
    Lambda = function(t) 0.2 * t, Lambda_inv = function(z) 5 * z,
    t_min = 0, t_max = 10, generate_at_least_K = 3
  )
  check_ppp_vector_validity(x, t_min = 0, t_max = 10, atleastk = 3)

  x <- draw(
    Lambda = function(t) 0.2 * t, Lambda_inv = function(z) 5 * z,
    t_min = 0, t_max = 10, generate_at_least_K = 2
  )
  check_ppp_vector_validity(x, t_min = 0, t_max = 10, atleastk = 2)
})


test_that("unimplemented and inconsistent options error", {
  lmat <- matrix(rep(1, 50), ncol = 5)

  # the vectorized thinning sampler supports the generation bounds
  Z <- vdraw_intensity(
    lambda = function(x, ...) 0.1 * x,
    lambda_maj_matrix = lmat,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    generate_at_least_K = 2
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = 2)

  # ... but the scalar thinning path does not
  expect_error(
    draw(
      lambda = function(t) 0.1 * t,
      line_majorizer_intercept = 1, line_majorizer_slope = 0,
      t_min = 0, t_max = 5,
      generate_at_least_K = 2
    ),
    "scalar thinning"
  )
  # flag/integer contradictions and exclusivity
  expect_error(
    vdraw_sc_step_regular_cpp(
      lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
      atmost1 = TRUE, report_first_K = 3
    ),
    "contradicts"
  )
  expect_error(
    vdraw_sc_step_regular_cpp(
      lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
      report_first_K = 2, report_last_K = 2
    ),
    "only one of"
  )
  expect_error(
    vdraw_sc_step(
      lambda_matrix = lmat, time_breaks = seq(1, 5, length.out = 6),
      generate_at_least_K = 5, generate_at_most_K = 2
    ),
    "exceeds"
  )
  expect_error(
    vztdraw_sc_step(
      lambda_matrix = lmat, time_breaks = seq(1, 5, length.out = 6),
      generate_at_least_K = NULL
    ),
    "at least one of"
  )
})


test_that("budget_cap caps the event budget on all paths", {
  set.seed(20260831)
  lmat <- matrix(rep(2, 5 * 100), ncol = 5) # Lambda_total = 40 on (1, 5)

  Z <- vdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    budget_cap = 3
  )
  expect_lte(ncol(Z), 3)

  Z <- vdraw_sc_step_regular(
    lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    budget_cap = 3
  )
  expect_lte(ncol(Z), 3)

  Z <- vdraw_sc_step(
    lambda_matrix = lmat, time_breaks = seq(1, 5, length.out = 6),
    budget_cap = 3
  )
  expect_lte(ncol(Z), 3)

  # the deprecated alias still works, with a warning
  expect_warning(
    Z <- vdraw_sc_step(
      lambda_matrix = lmat, time_breaks = seq(1, 5, length.out = 6),
      atmostB = 3
    ),
    "deprecated"
  )
  expect_lte(ncol(Z), 3)

  # the conditioned path never truncates below generate_at_least_K
  Z <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat / 40, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    generate_at_least_K = 5, budget_cap = 2
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = 5)
})


test_that("regular and general grids agree on identical equal-spaced breaks (same seed)", {
  lmat <- matrix(stats::rgamma(5 * 200, shape = 1), ncol = 5)
  breaks <- seq(2, 7, length.out = 6)

  set.seed(1)
  Z_reg <- vdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 2, rate_matrix_t_max = 7
  )
  set.seed(1)
  Z_gen <- vdraw_sc_step(lambda_matrix = lmat, time_breaks = breaks)
  expect_equal(Z_reg, Z_gen, tolerance = 1e-12)

  set.seed(2)
  Z_reg <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 2, rate_matrix_t_max = 7,
    generate_at_least_K = 2, generate_at_most_K = 4
  )
  set.seed(2)
  Z_gen <- vztdraw_sc_step(
    lambda_matrix = lmat, time_breaks = breaks,
    generate_at_least_K = 2, generate_at_most_K = 4
  )
  expect_equal(Z_reg, Z_gen, tolerance = 1e-12)
})
