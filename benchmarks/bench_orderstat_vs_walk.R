# Sequential Exp(1) walk vs order-statistics construction: mechanism cost at
# matched workloads. The comparison pairs the unconditioned sampler with the
# nearly-equivalent generate_at_least_K = 1 draw, so the two paths do the same
# amount of sampling work at moderate-to-large mean event counts. (The laws
# differ by the zero-truncation; at mean >= 20 events the difference is
# negligible, so read the ratios as mechanism cost, not same-task benchmarks.)
#
# Recorded results (2026-08-28, Apple Silicon, N = 1e5 rows, whole range):
#   C++ sc-step kernels, sequential : orderstat ratio
#     mean events  1: 1.67   4: 1.34   20: 1.09   100: 1.02
#   R-level vdraw_cumulative_intensity paths
#     mean events  1: 1.25   4: 1.11   20: 1.05   100: 0.96 (walk loses:
#     per-column R loop + padding to the worst-row qpois(1 - tol) width)
# Interpretation: the orderstat core carries a FIXED per-row overhead (the
# rbtpois truncated-Poisson inverse CDF, plus a sort in C++) that dominates at
# small N and washes out as N grows. The kernel-to-option routing (sequential
# wherever legal, orderstat only when a generation bound forces it) is the
# efficient assignment; do not add a workload-dependent switch without new
# evidence.
#
# Usage: devtools::load_all(); then call the functions below.

bench_cpp_orderstat_vs_walk <- function(N = 1e5, rates = c(0.2, 0.8, 4, 20),
                                        n_rep = 30) {
  for (rate in rates) {
    lmat <- matrix(rep(rate, 5 * N), ncol = 5)
    seqk <- system.time(for (i in seq_len(n_rep)) {
      vdraw_sc_step_regular_cpp(
        lambda_matrix = lmat, rate_matrix_t_min = 0, rate_matrix_t_max = 5
      )
    })["elapsed"]
    ostat <- system.time(for (i in seq_len(n_rep)) {
      vztdraw_sc_step_regular_cpp(
        lambda_matrix = lmat, rate_matrix_t_min = 0, rate_matrix_t_max = 5,
        generate_at_least_K = 1
      )
    })["elapsed"]
    cat(sprintf(
      "C++  mean events %5.1f | sequential %7.1f ms | orderstat %7.1f ms | ratio %.2f\n",
      rate * 5, 1000 * seqk / n_rep, 1000 * ostat / n_rep, ostat / seqk
    ))
  }
}

bench_r_orderstat_vs_walk <- function(N = 1e5, rates = c(0.2, 0.8, 4, 20),
                                      n_rep = 20) {
  for (rate in rates) {
    Lf <- function(t, ...) rate * t
    Lif <- function(z, ...) z / rate
    t0 <- rep(0, N)
    t1 <- rep(5, N)
    walk <- system.time(for (i in seq_len(n_rep)) {
      vdraw_cumulative_intensity(
        Lambda = Lf, Lambda_inv = Lif, t_min = t0, t_max = t1
      )
    })["elapsed"]
    ostat <- system.time(for (i in seq_len(n_rep)) {
      vdraw_cumulative_intensity(
        Lambda = Lf, Lambda_inv = Lif, t_min = t0, t_max = t1,
        generate_at_least_K = 1
      )
    })["elapsed"]
    count <- system.time(for (i in seq_len(n_rep)) {
      rbtpois_vec(rep(rate * 5, N), 1L, 0L)
    })["elapsed"]
    cat(sprintf(
      "R    mean events %5.1f | walk %7.1f ms | orderstat %7.1f ms (rbtpois %5.1f ms) | ratio %.2f\n",
      rate * 5, 1000 * walk / n_rep, 1000 * ostat / n_rep,
      1000 * count / n_rep, ostat / walk
    ))
  }
}
