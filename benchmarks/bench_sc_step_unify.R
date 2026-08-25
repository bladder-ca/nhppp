# Benchmark for the sc_step kernel unification (refactor/sc-step-unify).
# Run from the package root:  Rscript benchmarks/bench_sc_step_unify.R <label>
# Results saved to benchmarks/sc_step_unify_<label>.rds ; compare before/after with:
#   a <- readRDS("benchmarks/sc_step_unify_baseline.rds")
#   b <- readRDS("benchmarks/sc_step_unify_unified.rds")
#   merge(a[, c("name", "median")], b[, c("name", "median")], by = "name",
#         suffixes = c("_before", "_after"))

devtools::load_all(quiet = TRUE)
label <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(label)) label <- "baseline"

set.seed(20260825)
n_draws <- 10000
K <- 20

# heterogeneous rates: most rows sparse, a few rows dominate max(Lambda)
lmat <- matrix(rgamma(n_draws * K, shape = 0.5, rate = 2), nrow = n_draws)
hot <- sample.int(n_draws, 50)
lmat[hot, ] <- lmat[hot, ] * 40

t0 <- 100
t1 <- 110
breaks_shared <- seq(t0, t1, length.out = K + 1)

res <- bench::mark(
  "reg whole plain" = vdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = t0, rate_matrix_t_max = t1
  ),
  "reg sub plain" = vdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = t0, rate_matrix_t_max = t1,
    t_min = t0 + 2.5, t_max = t1 - 2.5
  ),
  "reg whole plain atmost1" = vdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = t0, rate_matrix_t_max = t1,
    atmost1 = TRUE
  ),
  "reg whole zt" = vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = t0, rate_matrix_t_max = t1
  ),
  "reg sub zt" = vztdraw_sc_step_regular_cpp(
    lambda_matrix = lmat, rate_matrix_t_min = t0, rate_matrix_t_max = t1,
    t_min = t0 + 2.5, t_max = t1 - 2.5
  ),
  "gen whole plain" = vdraw_sc_step(
    lambda_matrix = lmat, time_breaks = breaks_shared
  ),
  "gen sub plain" = vdraw_sc_step(
    lambda_matrix = lmat, time_breaks = breaks_shared,
    t_min = t0 + 2.5, t_max = t1 - 2.5
  ),
  "gen whole zt" = vztdraw_sc_step(
    lambda_matrix = lmat, time_breaks = breaks_shared
  ),
  iterations = 20,
  check = FALSE,
  memory = FALSE,
  filter_gc = FALSE
)

out <- as.data.frame(res[, c("expression", "median", "total_time", "n_itr")])
out$name <- vapply(res$expression, deparse1, character(1))
print(res[, c("expression", "median", "itr/sec", "n_itr")])
saveRDS(res, file.path("benchmarks", paste0("sc_step_unify_", label, ".rds")))
cat("saved benchmarks/sc_step_unify_", label, ".rds\n", sep = "")
