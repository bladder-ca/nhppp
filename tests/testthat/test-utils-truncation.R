test_that("truncation argument resolvers work", {
  expect_identical(nhppp:::.resolve_atmostK(FALSE, NULL), 0L)
  expect_identical(nhppp:::.resolve_atmostK(TRUE, NULL), 1L)
  expect_identical(nhppp:::.resolve_atmostK(TRUE, 1), 1L)
  expect_identical(nhppp:::.resolve_atmostK(FALSE, 3), 3L)
  expect_error(nhppp:::.resolve_atmostK(TRUE, 3), "contradicts")
  expect_error(nhppp:::.resolve_atmostK(FALSE, 0), "positive integer")
  expect_error(nhppp:::.resolve_atmostK(FALSE, -1), "positive integer")

  expect_identical(nhppp:::.resolve_atleastK(FALSE, NULL), 0L)
  expect_identical(nhppp:::.resolve_atleastK(TRUE, NULL), 1L)
  expect_identical(nhppp:::.resolve_atleastK(TRUE, 1), 1L)
  expect_identical(nhppp:::.resolve_atleastK(FALSE, 5), 5L)
  expect_error(nhppp:::.resolve_atleastK(TRUE, 3), "contradicts")
  expect_error(nhppp:::.resolve_atleastK(FALSE, 0), "positive integer")

  expect_identical(nhppp:::.resolve_budget_cap(NULL, NULL), 0L)
  expect_identical(nhppp:::.resolve_budget_cap(7, NULL), 7L)
  expect_warning(b <- nhppp:::.resolve_budget_cap(NULL, 7), "deprecated")
  expect_identical(b, 7L)
  expect_warning(
    expect_error(nhppp:::.resolve_budget_cap(5, 7), "disagree"),
    "deprecated"
  )
  expect_error(nhppp:::.resolve_budget_cap(0, NULL), "positive integer")
})


test_that("rbtpois() samples the K-truncated Poisson distribution", {
  set.seed(20260825)
  n <- 20000
  for (fixture in list(
    list(lambda = 2.5, k = 1), list(lambda = 2.5, k = 3),
    list(lambda = 0.5, k = 2), list(lambda = 8, k = 12)
  )) {
    lam <- fixture$lambda
    k <- fixture$k
    x <- rbtpois(n = n, lambda = lam, k = k)
    expect_true(all(x >= k))

    # exact conditional pmf, tail bins pooled to keep expected counts up
    sup <- k:max(max(x), k + 10)
    pmf <- dpois(sup, lam) / ppois(k - 1, lam, lower.tail = FALSE)
    keep <- pmf * n >= 5
    keep[length(keep)] <- FALSE
    obs <- c(tabulate(factor(x, levels = sup), nbins = length(sup))[keep], sum(!(x %in% sup[keep])))
    pr <- c(pmf[keep], 1 - sum(pmf[keep]))
    chi <- suppressWarnings(stats::chisq.test(obs, p = pr, rescale.p = TRUE))
    expect_gt(chi$p.value, 0.001)
  }

  # the C++ vectorized twin agrees with theory too
  x <- rbtpois_vec(rep(2.5, n), 3L)
  expect_true(all(x >= 3))
  sup <- 3:25
  pmf <- dpois(sup, 2.5) / ppois(2, 2.5, lower.tail = FALSE)
  obs <- c(tabulate(factor(x, levels = sup[1:10]), nbins = 10), sum(x > sup[10]))
  pr <- c(pmf[1:10], 1 - sum(pmf[1:10]))
  chi <- suppressWarnings(stats::chisq.test(obs, p = pr, rescale.p = TRUE))
  expect_gt(chi$p.value, 0.001)

  # k = 1 is the zero-truncated distribution
  x1 <- rbtpois(n = n, lambda = 1.5, k = 1)
  x2 <- rztpois(n = n, lambda = 1.5)
  expect_gt(suppressWarnings(stats::ks.test(x1, x2))$p.value, 0.001)

  # rng_stream variant
  x <- rng_stream_rbtpois(size = 1000, lambda = 1, k = 4)
  expect_true(all(x >= 4))
})


test_that("atleastK conditions the vectorized sc_step samplers on >= K events", {
  set.seed(20260826)
  n_draws <- 10000
  lmat <- matrix(rep(1, 5 * n_draws), ncol = 5) # Lambda_total = 4 on (1, 5)

  for (k in c(1L, 3L)) {
    Z <- vztdraw_sc_step_regular_cpp(
      lambda_matrix = lmat,
      rate_matrix_t_min = 1, rate_matrix_t_max = 5,
      atleastK = k
    )
    check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = k)

    # counts follow the K-truncated Poisson(4) distribution
    counts <- rowSums(!is.na(Z))
    sup <- k:max(counts)
    pmf <- dpois(sup, 4) / ppois(k - 1, 4, lower.tail = FALSE)
    obs <- tabulate(factor(counts, levels = sup), nbins = length(sup))
    chi <- suppressWarnings(stats::chisq.test(obs, p = pmf, rescale.p = TRUE))
    expect_gt(chi$p.value, 0.001)

    # event times are uniform order statistics on (1, 5)
    expect_gt(
      suppressWarnings(stats::ks.test(as.vector(Z[!is.na(Z)]), "punif", 1, 5))$p.value,
      0.001
    )
  }

  # general-grid path (vztdraw_sc_step) with irregular breaks
  Z <- vztdraw_sc_step(
    lambda_matrix = lmat[1:1000, ],
    time_breaks = c(1, 1.5, 3, 4, 4.2, 5),
    atleastK = 2
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = 2)

  # subinterval sampling conditions on the subinterval count
  Z <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat[1:1000, ],
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    t_min = 2, t_max = 3,
    atleastK = 2
  )
  check_ppp_sample_validity(Z, t_min = 2, t_max = 3, atleastk = 2)
})


test_that("atmostK reports the earliest K events; combinations with atleastK work", {
  set.seed(20260827)
  n_draws <- 2000
  lmat <- matrix(rep(1, 5 * n_draws), ncol = 5)

  # plain sampler: at most K events, still a valid (sorted, in-range) sample
  Z <- vdraw_sc_step_regular_cpp(
    lambda_matrix = lmat,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    atmostK = 2
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmostk = 2)

  # first event of the atmostK-truncated sample has the same law as the
  # first event of the unrestricted sample
  Z_full <- vdraw_sc_step_regular_cpp(
    lambda_matrix = lmat,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5
  )
  expect_gt(
    suppressWarnings(stats::ks.test(Z[, 1][!is.na(Z[, 1])], Z_full[, 1][!is.na(Z_full[, 1])]))$p.value,
    0.001
  )

  # zt sampler: conditioning on >= 3, reporting the earliest 2 -> exactly 2
  Z <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    atmostK = 2, atleastK = 3
  )
  expect_true(all(rowSums(!is.na(Z)) == 2))
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmostk = 2)

  # atleastK = atmostK = K -> exactly K events per row
  Z <- vztdraw_sc_step(
    lambda_matrix = lmat,
    time_breaks = seq(1, 5, length.out = 6),
    atmostK = 3, atleastK = 3
  )
  expect_true(all(rowSums(!is.na(Z)) == 3))

  # atmost1 remains an alias of atmostK = 1
  Z <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    atmost1 = TRUE
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmost1 = TRUE, atleast1 = TRUE)
})


test_that("scalar samplers honor atleastK/atmostK", {
  set.seed(20260828)

  x <- ztppp(rate = 0.2, t_min = 0, t_max = 10, atleastK = 4)
  check_ppp_vector_validity(x, t_min = 0, t_max = 10, atleastk = 4)

  # count distribution of ztppp(atleastK = k)
  counts <- replicate(5000, length(ztppp(rate = 0.5, t_min = 0, t_max = 2, atleastK = 2)))
  sup <- 2:max(counts)
  pmf <- dpois(sup, 1) / ppois(1, 1, lower.tail = FALSE)
  obs <- tabulate(factor(counts, levels = sup), nbins = length(sup))
  chi <- suppressWarnings(stats::chisq.test(obs, p = pmf, rescale.p = TRUE))
  expect_gt(chi$p.value, 0.001)

  x <- draw_sc_step(lambda_vector = rep(0.1, 5), time_breaks = 0:5, atleastK = 2)
  check_ppp_vector_validity(x, t_min = 0, t_max = 5, atleastk = 2)

  x <- draw_sc_step(lambda_vector = rep(2, 5), time_breaks = 0:5, atmostK = 3)
  check_ppp_vector_validity(x, t_min = 0, t_max = 5, atmostk = 3)

  x <- draw_sc_step_regular(lambda_vector = rep(0.1, 5), t_min = 0, t_max = 5, atleastK = 2)
  check_ppp_vector_validity(x, t_min = 0, t_max = 5, atleastk = 2)

  x <- ztdraw_sc_linear(intercept = 0.1, slope = 0.01, t_min = 0, t_max = 5, atleastK = 3)
  check_ppp_vector_validity(x, t_min = 0, t_max = 5, atleastk = 3)

  x <- ztdraw_sc_loglinear(intercept = -2, slope = 0.01, t_min = 0, t_max = 5, atleastK = 3)
  check_ppp_vector_validity(x, t_min = 0, t_max = 5, atleastk = 3)

  x <- ztdraw_cumulative_intensity(
    Lambda = function(t) 0.2 * t, Lambda_inv = function(z) 5 * z,
    t_min = 0, t_max = 10, atleastK = 3
  )
  check_ppp_vector_validity(x, t_min = 0, t_max = 10, atleastk = 3)

  x <- draw(
    Lambda = function(t) 0.2 * t, Lambda_inv = function(z) 5 * z,
    t_min = 0, t_max = 10, atleastK = 2
  )
  check_ppp_vector_validity(x, t_min = 0, t_max = 10, atleastk = 2)
})


test_that("unimplemented and inconsistent options error", {
  lmat <- matrix(rep(1, 50), ncol = 5)

  # the vectorized thinning sampler now supports atleastK >= 2
  Z <- vdraw_intensity(
    lambda = function(x, ...) 0.1 * x,
    lambda_maj_matrix = lmat,
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    atleastK = 2
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = 2)

  # ... but the scalar thinning path does not
  expect_error(
    draw(
      lambda = function(t) 0.1 * t,
      line_majorizer_intercept = 1, line_majorizer_slope = 0,
      t_min = 0, t_max = 5,
      atleastK = 2
    ),
    "not been implemented"
  )
  expect_error(
    vdraw_cumulative_intensity(
      Lambda = function(t, ...) 0.2 * t, Lambda_inv = function(z, ...) 5 * z,
      t_min = 0, t_max = 10, atleastK = 2
    ),
    "not been implemented"
  )

  # flag/integer contradictions
  expect_error(
    vdraw_sc_step_regular_cpp(
      lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
      atmost1 = TRUE, atmostK = 3
    ),
    "contradicts"
  )
  expect_error(
    vztdraw_sc_step(lambda_matrix = lmat, time_breaks = seq(1, 5, length.out = 6), atleastK = 0),
    "positive integer"
  )
})


test_that("budget_cap caps the event budget on all paths", {
  set.seed(20260829)
  lmat <- matrix(rep(2, 5 * 100), ncol = 5) # Lambda_total = 40 on (1, 5)

  Z <- vdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    budget_cap = 3
  )
  expect_lte(ncol(Z), 3)

  # whole-range regular path used to ignore the cap silently
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

  # the zt path never truncates below atleastK
  Z <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat / 40, rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    atleastK = 5, budget_cap = 2
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = 5)
})


test_that("regular and general grids agree on identical equal-spaced breaks (same seed)", {
  lmat <- matrix(stats::rgamma(5 * 200, shape = 1), ncol = 5)
  breaks <- seq(2, 7, length.out = 6)

  set.seed(1); Z_reg <- vdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 2, rate_matrix_t_max = 7
  )
  set.seed(1); Z_gen <- vdraw_sc_step(lambda_matrix = lmat, time_breaks = breaks)
  expect_equal(Z_reg, Z_gen, tolerance = 1e-12)

  set.seed(2); Z_reg <- vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = 2, rate_matrix_t_max = 7, atleastK = 2
  )
  set.seed(2); Z_gen <- vztdraw_sc_step(lambda_matrix = lmat, time_breaks = breaks, atleastK = 2)
  expect_equal(Z_reg, Z_gen, tolerance = 1e-12)
})
