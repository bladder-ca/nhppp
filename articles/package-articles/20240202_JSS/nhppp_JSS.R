## ----setup-block, echo=FALSE, message=FALSE, results='hide'-----------------------------------------------------------------------
library("knitr")
suppressPackageStartupMessages({
  library("ggplot2")
})
library("bench")
library("latex2exp")
library("nhppp")
render_sweave()
opts_chunk$set(prompt=TRUE)
options(
  prompt = "R> ", continue = "+  ", width = 70,
  useFancyQuotes = FALSE, out.extra = "",
  cache = TRUE, cache.extra = rand_seed
)
# knitr::opts_chunk$set(warning = FALSE, message = FALSE)
set.seed(2024)

# helper functions and options for the live report generation
full_results <- FALSE
n_simulations <- 10^4
n_bins_wasserstein <- NULL
n_samples_wasserstein <- 1000
if (!full_results) {
  n_simulations <- 100
  n_bins_wasserstein <- 5
  n_samples_wasserstein <- 500
}


sanitize_lambdas <- function(x) {
  x <- gsub(pattern = "l_\\*", replacement = "$\\\\lambda_*$", x = x)
  x <- gsub(pattern = "l_max*", replacement = "$\\\\lambda_{*a}$", x = x)
  x <- gsub(pattern = "l_m2", replacement = "$\\\\lambda_{*c}$", x = x)
  x <- gsub(pattern = "l_m", replacement = "$\\\\lambda_{*b}$", x = x)
  x <- gsub(pattern = "Li_", replacement = "$\\\\Lambda^{-1}$", x = x)
  x <- gsub(pattern = "L_", replacement = "$\\\\Lambda$", x = x)
  return(x)
}


sanitize_strings_for_figures <- function(x) {
  sanitize_lambdas(x)
}


sanitize_strings_for_tables <- function(x) {
  pkglist <- c(
    "nhppp", "reda", "simEd", "NHPoisson", "poisson",
    "IndTestPP", "PtProcess", "spatstat.random"
  )
  for (pkg in pkglist) {
    x <- gsub(pattern = pkg, replacement = paste0("\\\\pkg{", pkg, "}"), x = x)
  }
  x <- sanitize_lambdas(x)
  return(x)
}

pprint <- function(x, digits = 2, use_lt = FALSE) {
  i <- 0
  for (x1 in x) {
    i <- i + 1
    if (use_lt && x1 < 0.5 * 10^(-digits)) {
      x1 <- paste0("<0.", paste0(rep("0", digits - 1), collapse = ""), "1")
    } else {
      x1 <- format(round(x1, digits = digits), nsmall = digits)
    }
    x[i] <- x1
  }
  return(x)
}


## ----install-development, eval=FALSE----------------------------------------------------------------------------------------------
# install.packages("devtools")
devtools::install_github("bladder-ca/nhppp-fast")


## ----install-release, eval=FALSE--------------------------------------------------------------------------------------------------
install.packages("nhppp")


## ----demo-pppt--------------------------------------------------------------------------------------------------------------------
library("nhppp")
ppp_sequential(range_t = c(7, 10), rate = 1, tol = 10^-6, atmost1 = FALSE)


## ----demo-pppt-rstream------------------------------------------------------------------------------------------------------------
library("rstream")
S <- new("rstream.mrg32k3a")
ppp_sequential(range_t = c(7, 10), rate = 1, rng_stream = S)


## ----demo-ppp_orderstat-----------------------------------------------------------------------------------------------------------
ppp_orderstat(range_t = c(3.14, 6.28), rate = 1/2)


## ----demo-ztppp-------------------------------------------------------------------------------------------------------------------
ztppp(range_t = c(0, 10), rate = 0.001, atmost1 = FALSE)


## ----demo-ppp_n-------------------------------------------------------------------------------------------------------------------
ppp_n(size = 4, range_t = c(0, 10))


## ----generic-function-example-----------------------------------------------------------------------------------------------------
l <- function(t) t
L <- function(t) 0.5 * t^2
Li <- function(z) sqrt(2 * z)
draw(
  lambda = l, lambda_maj = l(10), range_t = c(5, 10),
  atmost1 = FALSE, atleast1 = FALSE
) |> head(n = 5)

draw(
  Lambda = L, Lambda_inv = Li, range_t = c(5, 10),
  atmost1 = FALSE, atleast1 = FALSE
) |> head(n = 5)


## ----demo-draw_intensity----------------------------------------------------------------------------------------------------------
lambda_fun <- function(t) exp(0.02 * t)
draw_intensity(
  lambda = lambda_fun, # linear majorizer
  lambda_maj = c(intercept = 1.01, slope = 0.03),
  exp_maj = FALSE, range_t = c(0, 10)
) |> head (n = 5)
draw_intensity(
  lambda = lambda_fun, # log-linear majorizer
  lambda_maj = c(intercept = 0.01, slope = 0.03),
  exp_maj = TRUE, range_t = c(0, 10)
) |> head (n = 5)


## ----demo-draw_intensity_step-----------------------------------------------------------------------------------------------------
draw_intensity_step(
  lambda = lambda_fun,
  lambda_maj_vector = lambda_fun(1:10), # 1:10 (10 intensity values)
  times_vector = 0:10 # 0:10 (11 interval bounds)
) |> head(n = 5)


## ----demo-draw_cumulative_intensity_inversion-------------------------------------------------------------------------------------
Lambda_fun <- function(t) 50 * exp(0.02 * t) - 50
Lambda_inv_fun <- function(z) 50 * log((z + 50) / 50)
draw_cumulative_intensity_inversion(
  Lambda = Lambda_fun,
  Lambda_inv = Lambda_inv_fun,
  range_t = c(5, 10.5),
  range_L = Lambda_fun(c(5, 10.5))
) |> head(n = 5)


## ----demo-draw_cumulative_intensity_orderstats------------------------------------------------------------------------------------
draw_cumulative_intensity_orderstats(
  Lambda = Lambda_fun,
  Lambda_inv = Lambda_inv_fun,
  range_t = c(4.1, 7.6)
)


## ----demo-ztdraw_cumulative_intensity---------------------------------------------------------------------------------------------
ztdraw_cumulative_intensity(
  Lambda = Lambda_fun,
  Lambda_inv = Lambda_inv_fun,
  range_t = c(4.1, 7.6)
)


## ----demo-draw_sc_step------------------------------------------------------------------------------------------------------------
draw_sc_step(
  lambda_vector = 1:5, times_vector = c(0.5, 1, 2.4, 3.1, 4.9, 5.9),
  atmost1 = FALSE, atleast1 = FALSE
) |> head(n = 5)
draw_sc_step_regular(
  lambda_vector = 1:5, range_t = c(0.5, 5.9), atmost1 = FALSE,
  atleast1 = FALSE
) |> head(n = 5)


## ----demo-vdraw_sc_step_regular---------------------------------------------------------------------------------------------------
vdraw_sc_step_regular(
  lambda_matrix = matrix(runif(20), ncol = 5), range_t = c(1, 4),
  atmost1 = FALSE
)


## ----demo-zt-draw_sc_linear-------------------------------------------------------------------------------------------------------
draw_sc_linear(alpha = 3, beta = -0.5, range_t = c(0, 10)) |> head(n = 5)
ztdraw_sc_linear(alpha = 0.5, beta = 0.2, range_t = c(9.999, 10))


## ----demo-draw_sc_loglinear-------------------------------------------------------------------------------------------------------
draw_sc_loglinear(alpha = 1, beta = -0.02, range_t = c(8, 10))


## ----demo-ztdraw_sc_loglinear-----------------------------------------------------------------------------------------------------
ztdraw_sc_loglinear(alpha = 1, beta = -0.02, range_t = c(9, 10))


## ----example-functions------------------------------------------------------------------------------------------------------------
l <- function(t) (1 + sin(t)) * exp(0.2 * t)
L <- function(t) {
  exp(0.2 * t) * (0.2 * sin(t) - cos(t)) / 1.04 +
    exp(0.2 * t) / 0.2 - 4.038462
}
Li <- approxfun(
  x = L(seq(0, 6 * pi, 10^-3)),
  y = seq(0, 6 * pi, 10^-3), rule = 2
)


## ----example-K-Lipschitz-value, echo=FALSE, include=FALSE-------------------------------------------------------------------------
# to get a bound for K
abs_l_prime_fun <- function(t, w = 1) abs((0.2 + 0.2 * sin(w * t) + w * cos(w * t))) * exp(0.2 * t)
K_lipschitz <- abs_l_prime_fun(6 * pi)


## ----example-majorizers, echo=FALSE, include=FALSE--------------------------------------------------------------------------------
M <- 20
t_breaks <- seq(0, 6 * pi, length.out = M + 1)
l_m <- get_step_majorizer(
  fun = l, breaks = t_breaks,
  is_monotone = FALSE, K = K_lipschitz
)
l_star <- stats::approxfun(
  x = t_breaks[1:M], y = l_m, method = "constant",
  rule = 2
)

l_m2 <- c()
for (m in 1:M) {
  t_tmp <- seq(t_breaks[m], t_breaks[m + 1], length.out = 10000)
  y_tmp <- l(t_tmp)
  l_m2 <- c(l_m2, y_tmp[which(y_tmp == max(y_tmp))] + K_lipschitz * (t_tmp[2] - t_tmp[1]) / 2)
}
l_star2 <- stats::approxfun(
  x = t_breaks[1:M], y = l_m2, method = "constant",
  rule = 2
)


## ----example-function-plot, fig.cap="The $\\lambda(t)$ (left) and $\\Lambda(t)$ used in the illustration. Also shown three majorizing functions (left panel, marked a, b, c) that are used with the thinning algorithm in the analyses.", fig.height=4, fig.width=8, echo=FALSE----
plot_base <- ggplot(data = data.frame(x = seq(0, 6 * pi, length.out = 200)), aes(x)) +
  xlab("Time") +
  theme_bw() +
  theme(text = element_text(size = 18))
plot_l <- plot_base +
  geom_function(fun = l, xlim = c(0, 6 * pi), color = "red") +
  geom_function(
    fun = function(x) l(6 * pi),
    xlim = c(0, 6 * pi), n = 200, color = "blue", linetype = "longdash"
  ) +
  geom_step(
    data = data.frame(x = t_breaks, l_m = l_m[c(1:M, M)]), aes(y = l_m),
    color = "black"
  ) +
  geom_step(
    data = data.frame(x = t_breaks, l_m2 = l_m2[c(1:M, M)]), aes(y = l_m2),
    color = "black", linewidth = 1
  ) +
  geom_text(aes(x = 1, y = l(6 * pi) + 3, label = "a")) +
  geom_text(aes(x = 1, y = l_star(1) + 3, label = "b")) +
  geom_text(aes(x = 1, y = l_star2(1) + 3, label = "c")) +
  ylab("Intensity")
plot_L <- plot_base +
  geom_function(fun = L, xlim = c(0, 6 * pi), color = "red") +
  ylab("Integrated intensity")
gridExtra::grid.arrange(plot_l, plot_L, nrow = 1)


## ----define-functions-for-counts,echo=FALSE, include=FALSE, cache=TRUE------------------------------------------------------------
get_chi2_counts <- function(x, mu) {
  # O, E counts
  Q <- seq(0.0001, 0.9999, length.out = 100)
  E <- qpois(Q, mu)
  O <- quantile(x, Q)
  # Goodness of Fit p value
  GOF <- sum(((O - E)^2) / E)
  pval <- pchisq(GOF, length(O) - 1, lower.tail = F)
  return(list(chi2 = GOF, pval = pval))
}
get_CIs_counts <- function(x, mu, CIs = c(95, 90, 75, 50)) {
  pct <- c()
  for (CI in CIs) {
    pr <- 1 - CI / 100
    pct <- c(pct, pr / 2, 1 - pr / 2)
  }
  theoretical <- matrix(stats::qpois(pct, mu), byrow = TRUE, ncol = 2)
  empirical <- matrix(as.integer(quantile(x, pct)), byrow = TRUE, ncol = 2)
  return(list(theoretical = theoretical, empirical = empirical))
}
get_wasserstein_counts <- function(x, mu, tmp_seed = 123, B = 1000, L = NULL) {
  # theoretical distribution XL, XU counts
  XL <- stats::qpois(p = 0.00001, lambda = mu)
  XU <- stats::qpois(p = 0.99999, lambda = mu)

  # empirical distribution PMF
  tabx <- table(x)
  the_ints <- as.integer(names(tabx))
  xl <- min(the_ints)
  xu <- max(the_ints)
  empirical_pmf <- c()
  for (xi in min(xl, XL):max(xu, XU)) {
    tmp <- tabx[which(the_ints == xi)]
    if (length(tmp) == 0) tmp <- 0
    empirical_pmf <- c(empirical_pmf, tmp)
  }
  empirical_pmf <- matrix(empirical_pmf / sum(empirical_pmf), ncol = 1)
  theoretical_pmf <- matrix(stats::dpois(x = min(xl, XL):max(XU, xu), lambda = mu), ncol = 1)
  theoretical_pmf <- theoretical_pmf / sum(theoretical_pmf)

  cost_mat <- as.matrix(stats::dist(arrayInd(1:nrow(theoretical_pmf), .dim = nrow(theoretical_pmf))))
  w1 <- otinference::wassDist(a = empirical_pmf[, 1], b = theoretical_pmf[, 1], distMat = cost_mat, p = 1)

  L <- if (!is.null(L)) as.integer(L) else nrow(theoretical_pmf)
  current_seed <- .Random.seed
  set.seed(tmp_seed)
  pval <- otinference::binWDTest(x = empirical_pmf, y = theoretical_pmf, L = L, B = B)
  set.seed(current_seed)
  return(list(w1 = w1, pval = pval))
}

performance_metrics_counts <- function(x, mu, digits = 3, tmp_seed = 123, B = 500, L = 20) {
  mean_x <- mean(x)
  B_mu <- mean_x - mu
  var_x <- var(x)
  B_V <- var_x - mu
  gof <- get_chi2_counts(x = x, mu = mu)
  cis <- get_CIs_counts(x, mu, CIs = c(95, 90, 75, 50))
  WD <- get_wasserstein_counts(x = x, mu = mu, tmp_seed = tmp_seed, B = B, L = L)

  values <- c(
    `Sample mean` = pprint(mean_x, digits),
    `$B_\\mu$` = pprint(B_mu, digits),
    `$B_{\\mu, rel}$` = pprint(100 * B_mu / mu, digits),
    `Sample variance` = pprint(var_x, digits),
    `$B_V$` = pprint(B_V, digits),
    `$B_{V, rel}$` = pprint(100 * B_V / mu, digits),
    `Goodness of fit, $\\chi^2$ [$p$~value]` = paste0(pprint(gof$chi2, digits), " [", pprint(gof$pval, digits, use_lt = T), "]"),
    `$W_1$ [$p$~value]` = paste0(pprint(WD$w1, digits, use_lt = T), " [", pprint(WD$pval, digits, use_lt = T), "]"),
    `Bounds 95\\% CI` = paste0("[", paste0(cis$empirical[1, ], collapse = ", "), "]"),
    `Bounds 90\\% CI` = paste0("[", paste0(cis$empirical[2, ], collapse = ", "), "]"),
    `Bounds 75\\% CI` = paste0("[", paste0(cis$empirical[3, ], collapse = ", "), "]"),
    `Bounds 50\\% CI` = paste0("[", paste0(cis$empirical[4, ], collapse = ", "), "]")
  )
  names(values)[length(values) - 3] <- paste0("Equal tail 95\\% CI = [", paste0(cis$theoretical[1, ], collapse = ", "), "]")
  names(values)[length(values) - 2] <- paste0("Equal tail 90\\% CI = [", paste0(cis$theoretical[2, ], collapse = ", "), "]")
  names(values)[length(values) - 1] <- paste0("Equal tail 75\\% CI = [", paste0(cis$theoretical[3, ], collapse = ", "), "]")
  names(values)[length(values) - 0] <- paste0("Equal tail 50\\% CI = [", paste0(cis$theoretical[4, ], collapse = ", "), "]")

  return(values)
}


## ----simulate-nhppp-pkg, include=FALSE, echo=FALSE--------------------------------------------------------------------------------
simulate_nhppp_pkg <- function() {
  Thinning_const <- nhppp::draw_intensity(
    lambda = l,
    lambda_maj = l(6 * pi),
    range_t = c(0, 6 * pi),
    atmost1 = FALSE
  )
  Thinning_star <- nhppp::draw_intensity_step(
    lambda = l,
    lambda_maj_vector = l_star(t_breaks[1:M]),
    times_vector = t_breaks,
    atmost1 = FALSE
  )
  Thinning_star2 <- nhppp::draw_intensity_step(
    lambda = l,
    lambda_maj_vector = l_star2(t_breaks[1:M]),
    times_vector = t_breaks,
    atmost1 = FALSE
  )
  Inversion <- nhppp::draw_cumulative_intensity_inversion(
    Lambda = L,
    Lambda_inv = Li, # numbers same with this NULL
    range_t = c(0, 6 * pi),
    atmost1 = FALSE
  )
  OrderStats <- nhppp::draw_cumulative_intensity_orderstats(
    Lambda = L,
    Lambda_inv = Li, # numbers same with this NULL
    range_t = c(0, 6 * pi),
    atmost1 = FALSE
  )
  return(list(
    "Thinning l_*=l_max" = Thinning_const,
    "Thinning l_*=l_m" = Thinning_star,
    "Thinning l_*=l_m2" = Thinning_star2,
    "Inversion" = Inversion,
    "Order statistics" = OrderStats
  ))
}

nhppp_pkg_event_series <- parallel::mcmapply(
  function(ni) {
    simulate_nhppp_pkg()
  },
  1:n_simulations,
  mc.cores = parallel::detectCores() - 1
)

nhppp_simulated_counts <- apply(nhppp_pkg_event_series, 2, function(x) sapply(x, length))

metrics_counts_nhppp_pkg <- apply(nhppp_simulated_counts, 1, performance_metrics_counts,
  mu = L(6 * pi) - L(0), digits = 3,
  B = n_samples_wasserstein, L = n_bins_wasserstein
)


## ----performance-nhppp-pkg-counts, results='asis', echo=FALSE---------------------------------------------------------------------
print(
  xtable::xtable(metrics_counts_nhppp_pkg,
    type = "latex",
    caption = "Simulated total number of events with \\pkg{nhppp} functions for the illustration example. Equal tail $p$\\% CI: a confidence interval whose bounds are the $p/2$ and $(1-p/2)$ count percentiles of the respective cumulative distribution function.",
    label = "tab:nhppp-results-counts"
  ),
  math.style.negative = TRUE,
  sanitize.text.function = sanitize_strings_for_tables,
  scalebox = .77
)


## ----functions-to-plot-ecdfs, echo=FALSE, include=FALSE---------------------------------------------------------------------------
plot_ecdf <- function(index, simulated_counts, Lambda = L) {
  x <- simulated_counts[[index]]

  t_vec <- as.integer(seq(stats::qpois(0.0001, Lambda(6 * pi) - Lambda(0)), stats::qpois(0.9999, L(6 * pi) - L(0)), length.out = 1000))
  density_vec <- stats::dpois(t_vec, Lambda(6 * pi) - Lambda(0))
  pct_vec <- stats::ppois(t_vec, Lambda(6 * pi) - Lambda(0))

  p <- ggplot(NULL) +
    geom_step(aes(x = t_vec, y = pct_vec), linetype = "solid", color = "red", linewidth = 0.5) +
    labs(x = "Count", y = "Cumulative density") +
    coord_cartesian(xlim = c(120, 225)) +
    theme_bw() +
    theme(text = element_text(size = 15)) +
    stat_ecdf(aes(x = simulated_counts[index, ]), linetype = "solid", color = "black") +
    ggtitle(TeX(sanitize_strings_for_figures(rownames(simulated_counts)[index])))
  return(p)
}


## ----ecdf-nhppp-pkg-counts, fig.cap="Theoretical (red) and empirical (black) cumulative distribution functions for event counts in the illustration example with \\pkg{nhppp} functions. The unsigned area between the theoretical and empirical curves equals the Wasserstein-1 distance in Table~\\ref{tab:nhppp-results-counts}.", fig.height=8, fig.width=12, echo=FALSE----
epdf_nhppp_counts <- gridExtra::grid.arrange(
  grobs = lapply(1:nrow(nhppp_simulated_counts),
    plot_ecdf,
    simulated_counts = nhppp_simulated_counts
  ),
  ncol = 3
)


## ----simulate-R-packages, echo=FALSE, cache=TRUE----------------------------------------------------------------------------------
simulate_r_pkgs <- function() {
  start <- 0
  stop <- 6 * pi

  reda_thinning_lmax <- reda::simEvent(
    rho = l,
    rhoMax = l(stop),
    origin = start, endTime = stop,
    recurrent = TRUE,
    method = "thinning"
  )

  reda_inversion <- reda::simEvent(
    rho = l,
    origin = start, endTime = stop,
    recurrent = TRUE,
    method = "inversion"
  )

  simEd_thinning_lmax <- simEd::thinning(
    maxTime = ceiling(stop),
    intensityFcn = l,
    majorizingFcn = function(x) l(stop),
    majorizingFcnType = NULL,
    seed = NA,
    maxTrials = Inf,
    plot = FALSE
  )

  t_seq_2 <- seq(start, stop, length.out = ceiling(stop))

  IndTestPP_thinning <- IndTestPP::simNHPc(
    lambda = l(t_seq_2),
    fixed.seed = NULL,
    algor = "Thinning"
  )$posNH

  IndTestPP_inversion <- IndTestPP::simNHPc(
    lambda = l(t_seq_2),
    fixed.seed = NULL,
    algor = "Inversion"
  )$posNH


  return(list(
    "reda thinning (l_*=l_max)" = reda_thinning_lmax,
    "reda inversion" = reda_inversion,
    "simEd thinning (l_*=l_max)" = simEd_thinning_lmax,
    "IndTestPP thinning (no l_*)" = IndTestPP_thinning,
    "IndTestPP inversion" = IndTestPP_inversion
  ))
}


old_seed <- .Random.seed
set.seed(2024)
cl <- parallel::makeCluster(parallel::detectCores() - 1)
start <- 0
stop <- 6 * pi
parallel::clusterExport(cl, c("simulate_r_pkgs", "l"))
r_pkg_event_series <- parallel::parSapply(cl,
  X = 1:n_simulations,
  FUN = function(i) {
    i <- simulate_r_pkgs()
  }
)
parallel::stopCluster(cl)
set.seed(.Random.seed)
r_pkg_times <- apply(r_pkg_event_series, 1, unlist)

r_pkgs_simulated_counts <- apply(t(r_pkg_event_series), 1, function(x) sapply(x, length))
metrics_counts_r_pkgs <- apply(r_pkgs_simulated_counts, 1, performance_metrics_counts,
  mu = L(6 * pi) - L(0), digits = 3,
  B = n_samples_wasserstein, L = n_bins_wasserstein
)
colnames(metrics_counts_r_pkgs) <- c(
  "reda thinning, l_*=l_max",
  "reda inversion",
  "simEd thinning, l_*=l_max",
  "IndTestPP thinning, no l_*",
  "IndTestPP inversion"
)


## ----r-pkgs-pkg-counts, fig.cap="Theoretical (red) and empirical (black) cumulative distribution functions for event counts in the illustration example with the \\proglang{R} packages in Table~\\ref{tab:R-packages}. The unsigned area between the theoretical and empirical curves equals the Wasserstein-1 distance in Table~\\ref{tab:nhppp-results-counts}.", fig.height=8, fig.width=12, echo=FALSE----
epdf_r_pkgs_counts <- gridExtra::grid.arrange(
  grobs = lapply(1:nrow(r_pkgs_simulated_counts),
    plot_ecdf,
    simulated_counts = r_pkgs_simulated_counts
  ),
  ncol = 3
)


## ----performance-r-packages-1, results='asis', echo=FALSE-------------------------------------------------------------------------
print(
  xtable::xtable(metrics_counts_r_pkgs[, 1:5],
    type = "latex",
    caption = "Simulated total number of events with the \\proglang{R} packages of Table~\\ref{tab:R-packages} for the illustration example. Equal tail $p$\\% CI: a confidence interval whose bounds are the $p/2$ and $(1-p/2)$ count percentiles of the respective cumulative distribution function.",
    label = "tab:r-packages-results-counts1",
    align = c("r", "p{1in}", "p{1in}", "p{1in}", "p{1in}", "p{1.1in}")
  ),
  math.style.negative = TRUE,
  sanitize.text.function = identity,
  sanitize.colnames.function = sanitize_strings_for_tables,
  sanitize.rownames.function = identity,
  scalebox = 0.77
)


## ----define-functions-for-times, echo=FALSE, include=TRUE-------------------------------------------------------------------------
get_chi2_times <- function(x, Lambda) {
  n_breaks <- 70
  xmids <- hist(x, breaks = n_breaks, plot = FALSE)$mids # histogram time breaks
  E <- sapply(seq_along(xmids), function(i) {
    (Lambda(xmids[i + 1]) - Lambda(xmids[i])) / (Lambda(6 * pi) - Lambda(0))
  })
  E <- E[-length(E)]
  O <- sapply(seq_along(xmids), function(i) {
    sum(x >= xmids[i] & x <= xmids[i + 1]) / length(x)
  })
  O <- O[-length(O)]
  gof <- sum(((O - E)^2) / E)
  pval <- pchisq(gof, length(O) - 1, lower.tail = FALSE)
  return(list(gof = gof, pval = pval, O = O, E = E))
}
get_wasserstein_times <- function(O, E, tmp_seed = 123, B = 1000, n_breaks = NULL) {
  # empirical distribution PMF
  empirical_pmf <- matrix(O / sum(O), ncol = 1)
  theoretical_pmf <- matrix(E / sum(E), ncol = 1)

  cost_mat <- as.matrix(stats::dist(arrayInd(1:nrow(theoretical_pmf), .dim = nrow(theoretical_pmf))))
  w1 <- otinference::wassDist(a = empirical_pmf[, 1], b = theoretical_pmf[, 1], distMat = cost_mat, p = 1)

  if (is.null(n_breaks)) {
    n_breaks <- length(O)
  }
  current_seed <- .Random.seed
  set.seed(tmp_seed)
  pval <- otinference::binWDTest(x = empirical_pmf, y = theoretical_pmf, L = n_breaks, B = B)
  set.seed(current_seed)
  return(list(w1 = w1, pval = pval))
}

performance_metrics_times <- function(index, simulated_times, Lambda = L, digits = 3, tmp_seed = 123, B = 500, n_breaks = 5) {
  x <- simulated_times[[index]]
  x <- x[!is.na(x)]

  chi2 <- get_chi2_times(x = x, Lambda = Lambda)
  WD <- get_wasserstein_times(O = chi2$O, E = chi2$E, B = B, n_breaks = n_breaks)

  return(c(
    `Goodness of fit, $\\chi^2$ [$p$~value]` = paste0(pprint(chi2$gof, digits), " [", pprint(chi2$pval, digits, use_lt = T), "]"),
    `$W_1$ [$p$~value]` = paste0(pprint(WD$w1, digits, use_lt = T), " [", pprint(WD$pval, digits, use_lt = T), "]")
  ))
}

nhppp_pkg_times <- apply(nhppp_pkg_event_series, 1, unlist)

times_hist <- function(index, simulated_times, lambda = l, Lambda = L) {
  x <- simulated_times[[index]]
  x <- x[!is.na(x)]
  p <- ggplot(NULL) +
    geom_histogram(aes(x = x, y = after_stat(density)), bins = 50, color = "black", fill = "grey") +
    geom_line(aes(x = x, y = lambda(x) / (Lambda(6 * pi) - Lambda(0))), linewidth = 0.5, color = "red") +
    theme_bw() +
    theme(text = element_text(size = 15)) +
    labs(
      y = "Density", x = "Time",
      title = TeX(sanitize_strings_for_figures(names(simulated_times)[index]))
    )
  return(p)
}


times_ecdf <- function(index, simulated_times, lambda = l, Lambda = L) {
  x <- simulated_times[[index]]
  x <- x[!is.na(x)]
  p <- ggplot(NULL) +
    stat_ecdf(aes(x = x), linetype = "solid", color = "black") +
    geom_line(aes(x = x, y = Lambda(x) / (Lambda(6 * pi) - Lambda(0))), linewidth = 0.5, color = "red") +
    labs(x = "Count", y = "Cumulative density") +
    coord_cartesian(xlim = c(0, 6 * pi)) +
    ggtitle(TeX(sanitize_strings_for_figures(names(simulated_times)[index]))) +
    theme_bw() +
    theme(text = element_text(size = 15))
  return(p)
}


## ----get-metrics-for-times, echo=FALSE, include=FALSE-----------------------------------------------------------------------------
metrics_times_nhppp_pkg <- t(
  sapply(seq_along(nhppp_pkg_times),
    performance_metrics_times,
    simulated_times = nhppp_pkg_times,
    n_breaks = n_bins_wasserstein,
    B = n_samples_wasserstein
  )
)
rownames(metrics_times_nhppp_pkg) <- names(nhppp_pkg_times)

metrics_times_r_pkgs <- t(
  sapply(seq_along(r_pkg_times),
    performance_metrics_times,
    simulated_times = r_pkg_times,
    n_breaks = n_bins_wasserstein,
    B = n_samples_wasserstein
  )
)
rownames(metrics_times_r_pkgs) <- names(r_pkg_times)


## ----performance-nhppp-pkg-times, results='asis', echo=FALSE----------------------------------------------------------------------
print(
  xtable::xtable(metrics_times_nhppp_pkg,
    type = "latex",
    caption = "Goodness of fit of simulated event times with \\pkg{nhppp} functions for the example.",
    label = "tab:nhppp-gof-times"
  ),
  math.style.negative = TRUE,
  sanitize.text.function = sanitize_strings_for_tables,
  scalebox = 1
)


## ----performance-r-pkg-times, results='asis', echo=FALSE--------------------------------------------------------------------------
print(
  xtable::xtable(metrics_times_r_pkgs,
    type = "latex",
    caption = "Goodness of fit of simulated event times with \\proglang{R} functions in Table~\\ref{tab:R-packages}.",
    label = "tab:r-pkgs-gof-times"
  ),
  math.style.negative = TRUE,
  sanitize.text.function = sanitize_strings_for_tables,
  scalebox = 1
)


## ----epdf-nhppp-pkg-times,fig.cap="Simulated event times with \\pkg{nhppp}. Left column: histogram (gray) and theoretical distribution (red) of event times; right column: empirical (black) and theoretical (red) cumulative distribution function. The unsigned area between the empirical and cumulative distribution functions is the $W_1$ distance in Table~\\ref{tab:nhppp-gof-times}.", fig.width=8, fig.height=20, fig.align='center', out.height = "\\textheight", out.extra = "keepaspectratio=true", echo=FALSE----
epdf_nhppp_pkg_times <- gridExtra::grid.arrange(
  grobs = c(
    lapply(seq_along(nhppp_pkg_times),
      times_hist,
      simulated_times = nhppp_pkg_times
    ),
    lapply(seq_along(nhppp_pkg_times),
      times_ecdf,
      simulated_times = nhppp_pkg_times
    )
  ),
  as.table = FALSE,
  ncol = 2
)


## ----epdf-r-pkgs-times,fig.cap="Simulated event times with the \\proglang{R} packages in Table~\\ref{tab:R-packages}. Left column: histogram (gray) and theoretical distribution (red) of event times; right column: empirical (black) and theoretical (red) cumulative distribution function. The unsigned area between the empirical and cumulative distribution functions is the $W_1$ distance in Table~\\ref{tab:r-pkgs-gof-times}.", fig.width=8, fig.height=20, fig.align='center', out.height = "\\textheight", out.extra = "keepaspectratio=true", echo=FALSE----
epdf_r_pkg_times <- gridExtra::grid.arrange(
  grobs = c(
    lapply(seq_along(r_pkg_times),
      times_hist,
      simulated_times = r_pkg_times
    ),
    lapply(seq_along(r_pkg_times),
      times_ecdf,
      simulated_times = r_pkg_times
    )
  ),
  as.table = FALSE,
  ncol = 2
)


## ----compute-times, echo=FALSE, include=FALSE, cache=TRUE-------------------------------------------------------------------------
comptimes_all_samples <- bench::mark(
  "(1a) nhppp: thinning (majorizer a)" = nhppp::draw_intensity(
    lambda = l, lambda_maj = l(6 * pi), range_t = c(0, 6 * pi), atmost1 = FALSE
  ),
  "(1b) nhppp: thinning (majorizer b)" = nhppp::draw_intensity_step(
    lambda = l, lambda_maj_vector = l_star(t_breaks[1:M]),
    times_vector = t_breaks, atmost1 = FALSE
  ),
  "(1c) nhppp: thinning (majorizer c)" = nhppp::draw_intensity_step(
    lambda = l, lambda_maj_vector = l_star2(t_breaks[1:M]),
    times_vector = t_breaks, atmost1 = FALSE
  ),
  "(1d) nhppp: inversion (with inverse)" =
    nhppp::draw_cumulative_intensity_inversion(
      Lambda = L, Lambda_inv = Li, range_t = c(0, 6 * pi), atmost1 = FALSE
    ),
  "(1e) nhppp: inversion (no inverse)" =
    nhppp::draw_cumulative_intensity_inversion(
      Lambda = L, Lambda_inv = NULL, range_t = c(0, 6 * pi), atmost1 = FALSE
    ),
  "(1f) nhppp: order statistics (with inverse)" =
    nhppp::draw_cumulative_intensity_orderstats(
      Lambda = L, Lambda_inv = Li, range_t = c(0, 6 * pi), atmost1 = FALSE
    ),
  "(1g) nhppp: order statistics (no inverse)" =
    nhppp::draw_cumulative_intensity_orderstats(
      Lambda = L, Lambda_inv = NULL, range_t = c(0, 6 * pi), atmost1 = FALSE
    ),
  "(2a) reda: inversion" = reda::simEvent(
    rho = l, origin = 0,
    endTime = 6 * pi, recurrent = TRUE, method = "inversion"
  ),
  "(2b) reda: thinning (majorizer a)" = reda::simEvent(
    rho = l, rhoMax = l(6 * pi), origin = 0,
    endTime = 6 * pi, recurrent = TRUE, method = "thinning"
  ),
  "(3a) IndTestPP: inversion" = IndTestPP::simNHPc(
    lambda =
      l(seq(0, 6 * pi, length.out = ceiling(6 * pi))), fixed.seed = NULL,
    algor = "Inversion"
  )$posNH,
  "(3b) IndTestPP: thinning (no majorizer)" = IndTestPP::simNHPc(
    lambda =
      l(seq(0, 6 * pi, length.out = ceiling(6 * pi))), fixed.seed = NULL,
    algor = "Thinning"
  )$posNH,
  "(4) simEd: thinning (majorizer a)" = simEd::thinning(
    maxTime = ceiling(6 * pi),
    intensityFcn = l, majorizingFcn = function(i) l(6 * pi), majorizingFcnType = NULL,
    seed = 2024, maxTrials = Inf, plot = FALSE
  ),
  check = FALSE, filter_gc = TRUE,
  max_iterations = n_simulations, memory = FALSE
)

comptimes_one_sample <- bench::mark(
  "(1a) nhppp: thinning (majorizer a)" = nhppp::draw_intensity(
    lambda = l, lambda_maj = l(6 * pi), range_t = c(0, 6 * pi), atmost1 = TRUE
  ),
  "(1b) nhppp: thinning (majorizer b)" = nhppp::draw_intensity_step(
    lambda = l, lambda_maj_vector = l_star(t_breaks[1:M]),
    times_vector = t_breaks, atmost1 = TRUE
  ),
  "(1c) nhppp: thinning (majorizer c)" = nhppp::draw_intensity_step(
    lambda = l, lambda_maj_vector = l_star2(t_breaks[1:M]),
    times_vector = t_breaks, atmost1 = TRUE
  ),
  "(1d) nhppp: inversion (with inverse)" =
    nhppp::draw_cumulative_intensity_inversion(
      Lambda = L, Lambda_inv = Li, range_t = c(0, 6 * pi), atmost1 = TRUE
    ),
  "(1e) nhppp: inversion (no inverse)" =
    nhppp::draw_cumulative_intensity_inversion(
      Lambda = L, Lambda_inv = NULL, range_t = c(0, 6 * pi), atmost1 = TRUE
    ),
  "(1f) nhppp: order statistics (with inverse)" =
    nhppp::draw_cumulative_intensity_orderstats(
      Lambda = L, Lambda_inv = Li, range_t = c(0, 6 * pi), atmost1 = TRUE
    ),
  "(1g) nhppp: order statistics (no inverse)" =
    nhppp::draw_cumulative_intensity_orderstats(
      Lambda = L, Lambda_inv = NULL, range_t = c(0, 6 * pi), atmost1 = TRUE
    ),
  "(2a) reda: inversion" = reda::simEvent(
    rho = l, origin = 0,
    endTime = 6 * pi, recurrent = FALSE, method = "inversion"
  ),
  "(2b) reda: thinning (majorizer a)" = reda::simEvent(
    rho = l, rhoMax = l(6 * pi), origin = 0,
    endTime = 6 * pi, recurrent = FALSE, method = "thinning"
  ),
  "(3a) IndTestPP: inversion" = IndTestPP::simNHPc(
    lambda =
      l(seq(0, 6 * pi, length.out = ceiling(6 * pi))), fixed.seed = NULL,
    algor = "Inversion"
  )$posNH[1],
  "(3b) IndTestPP: thinning (no majorizer)" = IndTestPP::simNHPc(
    lambda =
      l(seq(0, 6 * pi, length.out = ceiling(6 * pi))), fixed.seed = NULL,
    algor = "Thinning"
  )$posNH[1],
  "(4) simEd: thinning (majorizer a)" = simEd::thinning(
    maxTime = ceiling(6 * pi),
    intensityFcn = l, majorizingFcn = function(i) l(6 * pi), majorizingFcnType = NULL,
    seed = 2024, maxTrials = Inf, plot = FALSE
  )[1],
  check = FALSE, filter_gc = TRUE,
  max_iterations = n_simulations, memory = FALSE
)

plot_comptimes <- function(benchdata, title = "") {
  autoplot(benchdata, type = c("ridge")) +
    labs(x = "", y = "") +
    ggtitle("") +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8)
    ) +
    # scale_x_log10(limits = c(10^-6, 0.150), expand = c(0, 0)) +
    scale_y_discrete(
      limits = rev,
      labels = function(x) stringr::str_wrap(sanitize_strings_for_figures(x), width = 45)
    ) +
    annotate("text",
      x = benchdata$median,
      y = c(length(benchdata$median):1),
      label = gsub(" ", "", benchdata$median, fixed = TRUE),
      size = 3, vjust = 1.5, hjust = 0.5
    ) +
    ggtitle(title) +
    theme(
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      plot.margin = margin(t = 0, r = 1, b = 0, l = 0, "cm")
    )
}


## ----comptimes_all_samples,fig.cap="Computation times when drawing all events in interval. Unit \\texttt{us} is $\\mu s$ (microsecond).", fig.height=7, fig.width=10, echo=FALSE, warning=FALSE, message=FALSE----
plot_comptimes(comptimes_all_samples)


## ----comptimes_one_sample,fig.cap="Computation times when drawing the first event in interval. Unit \\texttt{us} is $\\mu s$ (microsecond).", fig.height=7, fig.width=10, echo=FALSE, warning=FALSE, message=FALSE----
plot_comptimes(comptimes_one_sample)


## ----vectorized-vs-not, include=FALSE, echo=FALSE, cache=TRUE---------------------------------------------------------------------
non_vec_fun <- function(x = n_simulations, atmost1 = FALSE) {
  lapply(X = 1:x, FUN = function(x) draw_sc_step_regular(lambda_vector = l_star(t_breaks[1:M]), range_t = c(0, 6 * pi), atmost1 = atmost1))
}
Lmat <- matrix(rep(l_star(t_breaks[1:M]), n_simulations), nrow = n_simulations, byrow = TRUE)
Lmat <- nhppp:::mat_cumsum_columns(Lmat)

comptimes_vec <- bench::mark(
  nonvectorized_all = non_vec_fun(x = n_simulations, atmost1 = FALSE),
  vectorized_all = vdraw_sc_step_regular(Lambda_matrix = Lmat, range_t = c(0, 6 * pi), atmost1 = FALSE),
  nonvectorized_one = non_vec_fun(x = n_simulations, atmost1 = TRUE),
  vectorized_one = vdraw_sc_step_regular(Lambda_matrix = Lmat, range_t = c(0, 6 * pi), atmost1 = TRUE),
  check = FALSE, filter_gc = TRUE,
  max_iterations = n_simulations, memory = FALSE
)


## ----app-majorizer----------------------------------------------------------------------------------------------------------------
get_step_majorizer(
  fun = abs, breaks = -5:5, is_monotone = FALSE,
  K = 1
)

