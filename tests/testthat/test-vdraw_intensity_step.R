test_that("vdraw_intensity_step() samples on irregular grids", {
  set.seed(20260826)
  n_draws <- 500
  lfun <- function(x, ...) 0.1 * x
  lmaj <- matrix(rep(1, 5 * n_draws), ncol = 5)
  breaks <- c(1, 1.5, 3, 4, 4.5, 5)

  Z <- vdraw_intensity_step(
    lambda = lfun, lambda_maj_matrix = lmaj, time_breaks = breaks
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5)

  # subinterval
  Z <- vdraw_intensity_step(
    lambda = lfun, lambda_maj_matrix = lmaj, time_breaks = breaks,
    t_min = 2, t_max = 4.2
  )
  check_ppp_sample_validity(Z, t_min = 2, t_max = 4.2)

  # per-row breaks; row 10 spans (10, 14] where lambda reaches 1.4, so the
  # majorizer must be at least that
  Z <- vdraw_intensity_step(
    lambda = lfun, lambda_maj_matrix = 1.5 * lmaj[1:10, ],
    time_breaks = matrix(rep(breaks, each = 10), nrow = 10) + 0:9
  )
  check_ppp_sample_validity(Z, t_min = 1 + 0:9, t_max = 5 + 0:9)

  # atmostK on the thinning path
  Z <- vdraw_intensity_step(
    lambda = lfun, lambda_maj_matrix = lmaj, time_breaks = breaks, report_first_K = 2
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atmostk = 2)

  # invalid majorizer errors
  expect_error(
    vdraw_intensity_step(
      lambda = function(x, ...) 2 * x, lambda_maj_matrix = lmaj,
      time_breaks = breaks
    ),
    "Majorizer"
  )
})


test_that("thinning with a tight majorizer reproduces the conditioned sc_step law", {
  # lambda == majorizer == constant 0.6: acceptance probability is 1, so the
  # conditioned thinned process must equal the conditioned piecewise-constant
  # process: counts ~ K-truncated Poisson(0.6 * 4), times uniform.
  set.seed(20260827)
  n_draws <- 10000
  rate <- 0.6
  lfun <- function(x, ...) rate * (x > 0)
  lmaj <- matrix(rep(rate, 5 * n_draws), ncol = 5)
  breaks <- c(1, 1.5, 3, 4, 4.5, 5)
  k <- 3L

  Z <- vdraw_intensity_step(
    lambda = lfun, lambda_maj_matrix = lmaj, time_breaks = breaks, generate_at_least_K = k
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = k)

  counts <- rowSums(!is.na(Z))
  lam <- rate * 4
  sup <- k:max(counts)
  pmf <- dpois(sup, lam) / ppois(k - 1, lam, lower.tail = FALSE)
  obs <- tabulate(factor(counts, levels = sup), nbins = length(sup))
  chi <- suppressWarnings(stats::chisq.test(obs, p = pmf, rescale.p = TRUE))
  expect_gt(chi$p.value, 0.001)

  expect_gt(
    suppressWarnings(stats::ks.test(as.vector(Z[!is.na(Z)]), "punif", 1, 5))$p.value,
    0.001
  )

  # upper-bound conditioning (rejection on the accepted count): with the
  # tight majorizer the counts must follow the right-truncated Poisson
  Z <- vdraw_intensity_step(
    lambda = lfun, lambda_maj_matrix = lmaj, time_breaks = breaks,
    generate_at_least_K = 1, generate_at_most_K = 3
  )
  counts <- rowSums(!is.na(Z))
  expect_true(all(counts >= 1 & counts <= 3))
  sup <- 1:3
  pmf <- dpois(sup, lam) / sum(dpois(sup, lam))
  obs <- tabulate(factor(counts, levels = sup), nbins = 3)
  chi <- suppressWarnings(stats::chisq.test(obs, p = pmf, rescale.p = TRUE))
  expect_gt(chi$p.value, 0.001)
})


test_that("conditioned thinning agrees across grids and majorizers", {
  set.seed(20260828)
  n_draws <- 2000
  lfun <- function(x, ...) 0.2 * x
  breaks <- seq(1, 5, length.out = 6)

  # regular vs general grid on equal-spaced breaks (distributional)
  Z_reg <- vdraw_intensity(
    lambda = lfun, lambda_maj_matrix = matrix(rep(1.2, 5 * n_draws), ncol = 5),
    rate_matrix_t_min = 1, rate_matrix_t_max = 5, generate_at_least_K = 2
  )
  Z_gen <- vdraw_intensity_step(
    lambda = lfun, lambda_maj_matrix = matrix(rep(1.2, 5 * n_draws), ncol = 5),
    time_breaks = breaks, generate_at_least_K = 2
  )
  check_ppp_sample_validity(Z_reg, t_min = 1, t_max = 5, atleastk = 2)
  check_ppp_sample_validity(Z_gen, t_min = 1, t_max = 5, atleastk = 2)
  compare_ppp_vectors(ppp1 = Z_reg, ppp2 = Z_gen, threshold = 0.1, showQQ = FALSE)

  # a looser majorizer must sample the same conditioned process
  Z_loose <- vdraw_intensity_step(
    lambda = lfun, lambda_maj_matrix = matrix(rep(3, 5 * n_draws), ncol = 5),
    time_breaks = breaks, generate_at_least_K = 2
  )
  compare_ppp_vectors(ppp1 = Z_gen, ppp2 = Z_loose, threshold = 0.1, showQQ = FALSE)

  # atleastK = atmostK -> exactly K accepted events per row
  Z <- vdraw_intensity_step(
    lambda = lfun, lambda_maj_matrix = matrix(rep(1.2, 5 * 200), ncol = 5),
    time_breaks = breaks, generate_at_least_K = 2, report_first_K = 2
  )
  expect_true(all(rowSums(!is.na(Z)) == 2))
})


test_that("exactly-K thinning salvages over-counts by uniform subsampling", {
  set.seed(20260901)
  n_draws <- 10000
  # Lambda_target = 2 * 4 = 8 with K = 2: two-sided rejection would accept a
  # row with probability P(N = 2 | N >= 2) of a Poisson(8), about 1%, while
  # the subsample salvage converges in essentially one round.
  rate <- 2
  lfun <- function(x, ...) rate * (x > 0)
  lmaj <- matrix(rep(rate, 5 * n_draws), ncol = 5) # tight majorizer
  breaks <- seq(1, 5, length.out = 6)
  K <- 2L
  # exact law given N = K under a constant rate: K iid U(1, 5);
  # first event CDF 1 - (1 - (t-1)/4)^K. A wrong salvage that kept the
  # earliest K (the report_first_K object) would fail this KS sharply.
  cdf_min_of_K <- function(q) 1 - (1 - (q - 1) / 4)^K

  Z <- vdraw_intensity_step(
    lambda = lfun, lambda_maj_matrix = lmaj, time_breaks = breaks,
    generate_at_least_K = K, generate_at_most_K = K
  )
  expect_true(all(rowSums(!is.na(Z)) == K))
  expect_gt(
    suppressWarnings(stats::ks.test(Z[, 1], cdf_min_of_K))$p.value,
    0.001
  )
  expect_gt(
    suppressWarnings(stats::ks.test(as.vector(Z), "punif", 1, 5))$p.value,
    0.001
  )

  # the regular-grid loop takes the same path
  Z <- vdraw_intensity(
    lambda = lfun, lambda_maj_matrix = lmaj[1:5000, ],
    rate_matrix_t_min = 1, rate_matrix_t_max = 5,
    generate_at_least_K = K, generate_at_most_K = K
  )
  expect_true(all(rowSums(!is.na(Z)) == K))
  expect_gt(
    suppressWarnings(stats::ks.test(Z[, 1], cdf_min_of_K))$p.value,
    0.001
  )
})


test_that("vztdraw_intensity_step() handles vectorized lambda arguments", {
  set.seed(20260829)
  N <- 300
  lfun <- function(x, a, ...) .2 * x^a$row_args$exponent
  l_args <- list(
    row_args = data.frame(exponent = seq(from = 0.5, to = 2, length.out = N))
  )
  lmaj <- matrix(5.5, nrow = N, ncol = 5) # max lambda = .2 * 5^2 = 5

  Z <- vdraw_intensity_step(
    lambda = lfun, lambda_args = l_args, lambda_maj_matrix = lmaj,
    time_breaks = c(1, 1.5, 3, 4, 4.5, 5), generate_at_least_K = 2
  )
  check_ppp_sample_validity(Z, t_min = 1, t_max = 5, atleastk = 2)
})
