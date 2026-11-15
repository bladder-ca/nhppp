#' Vectorized simulation from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) given the cumulative intensity function and its inverse
#'
#' @description  Sample NHPPP times using the cumulative intensity function and its inverse.
#' @param Lambda (function, double vector) an increasing function
#'        which is the integrated rate of the NHPPP.
#'        It should take a vectorized argument t for times and an optional arguments list.
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`, also in vectorized form
#'        It should take a vectorized argument z and an optional arguments list.
#' @param t_min (scalar | vector | column matrix) the lower bound of the interval for each sampled point process
#'        The length of this argument is the number of point processes that should be drawn.
#' @param t_max (scalar | vector | column matrix) the upper bound of the interval for each sampled point process
#'        The length of this argument is the number of point processes that should be drawn.
#' @param Lambda_args (list) arguments for BOTH `Lambda` and `Lambda_inv`, with
#'        up to two elements: `shared` (named list of row-invariant arguments)
#'        and `row_args` (data.frame or data.table with one row per point
#'        process). When the structured container is used it is passed as the
#'        second positional argument of both functions; the name of the second
#'        formal is up to you. A flat list keeps the released behavior
#'        (`Lambda(t, Lambda_args = ...)`, named) with a deprecation warning.
#' @param Lambda_inv_args (list) deprecated; pass one structured `Lambda_args`
#'        container used by both `Lambda` and `Lambda_inv`.
#' @param tol the tolerange for the calulations.
#' @param atmost1 boolean, report at most 1 event time per sampled point
#'        process (alias for `report_first_K = 1`).
#' @param report_first_K `NULL` or a positive integer: report only the
#'        earliest K event times (reporting truncation).
#' @param report_last_K `NULL` or a positive integer: report only the latest
#'        K event times (reporting truncation).
#' @param atleast1 boolean, condition on at least 1 event (alias for
#'        `generate_at_least_K = 1`).
#' @param generate_at_least_K `NULL` or a positive integer: condition the
#'        sampled process on at least K events in `(t_min, t_max)`.
#' @param generate_at_most_K `NULL` or a positive integer: condition the
#'        sampled process on at most K events in `(t_min, t_max)`. May be
#'        combined with `generate_at_least_K` (K1 <= K2); equal bounds
#'        condition on exactly K events. Any generation bound switches the
#'        sampler from sequential inversion to the order-statistics
#'        construction on the `Lambda` scale with a doubly-truncated
#'        Poisson number of events.
#' @param output (string) `"matrix"` (default) returns the NA-padded event
#'        matrix, one row per point process. `"long"` returns the long event
#'        format `list(id, time, n_draws)` with one entry per event: `id` is
#'        the 1-based point-process index (ascending; times ascending within
#'        `id`); a point process with no events contributes no entries, and
#'        `n_draws` distinguishes "no events" from "not sampled". On the
#'        conditioned (order-statistics) path the long format is built with
#'        no dense intermediate, and `Lambda_inv` is called on the event
#'        VECTOR with the container's `row_args` subset to one row per event
#'        (aligned by `id`); the unconditioned path converts its dense walk
#'        at the end.
#'
#' @return a matrix of event times with one row per sampled point process,
#'        or the long event format if `output = "long"`.
#' @examples
#' Z <- vdraw_cumulative_intensity(
#'   Lambda = function(t) t^1.5,
#'   Lambda_inv = function(z) z^(1 / 1.5),
#'   t_min = 0,
#'   t_max = rep(2, 10)
#' )
#'
#' # arguments in the structured container; conditioned on exactly two events
#' Z <- vdraw_cumulative_intensity(
#'   Lambda = function(t, a) t^a$shared$p,
#'   Lambda_inv = function(z, a) z^(1 / a$shared$p),
#'   t_min = 0,
#'   t_max = rep(2, 10),
#'   Lambda_args = list(shared = list(p = 1.5)),
#'   generate_at_least_K = 2, generate_at_most_K = 2
#' )
#' @export
#'
vdraw_cumulative_intensity <- function(Lambda,
                                       Lambda_inv,
                                       t_min,
                                       t_max,
                                       Lambda_args = NULL,
                                       Lambda_inv_args = NULL,
                                       tol = 10^-6,
                                       atmost1 = FALSE,
                                       report_first_K = NULL,
                                       report_last_K = NULL,
                                       atleast1 = FALSE,
                                       generate_at_least_K = NULL,
                                       generate_at_most_K = NULL,
                                       output = c("matrix", "long")) {
  rep_ <- .resolve_reporting(atmost1, report_first_K, report_last_K)
  gen_ <- .resolve_generation(atleast1, generate_at_least_K, generate_at_most_K)
  long_ <- .resolve_output(output)
  range_t <- cbind(as.vector(t_min), as.vector(t_max))
  N_rows <- nrow(range_t)

  fa_ <- .resolve_fun_args(Lambda_args, n_draws = N_rows, arg_name = "Lambda_args")
  # structured containers get the positional-conditional convention, one
  # container delivered to both Lambda and Lambda_inv; flat containers (and
  # any use of Lambda_inv_args) keep the released named-argument behavior
  use_legacy_call <- !is.null(Lambda_inv_args) || fa_$mode %in% c("flat", "legacy_va")
  if (!is.null(Lambda_inv_args)) {
    warning(
      "`Lambda_inv_args` is deprecated; pass one structured `Lambda_args` container ",
      "(elements `shared`/`row_args`), delivered to both `Lambda` and `Lambda_inv`",
      call. = FALSE
    )
  } else if (fa_$mode == "flat") {
    warning(
      "passing a flat `Lambda_args` list is deprecated; use the structured container ",
      "(elements `shared`/`row_args`), delivered positionally to both `Lambda` and `Lambda_inv`",
      call. = FALSE
    )
  }
  range_L <- if (use_legacy_call) {
    Lambda(range_t, Lambda_args = Lambda_args)
  } else {
    .call_with_args(Lambda, range_t, fa_$container)
  }

  if (gen_$at_least > 0L || gen_$at_most > 0L) {
    # Order statistics on the Lambda scale: conditional on the number of
    # events N in (t_min, t_max), the Lambda-transformed times are N
    # ascending uniforms on (Lambda(t_min), Lambda(t_max)). Draw N from the
    # doubly-truncated Poisson, then the ascending uniforms as normalized
    # Exp(1) spacings (S_1 / S_(N+1), ..., S_N / S_(N+1)), which vectorizes
    # across rows with unequal N.
    mu <- range_L[, 2] - range_L[, 1]
    N_events <- rbtpois_vec(mu, gen_$at_least, gen_$at_most)
    # the 1 - tol count quantile is the same approximation knob as the
    # unconditioned path; it never truncates below generate_at_least_K
    N_events <- as.integer(
      pmin(N_events, pmax(gen_$at_least, stats::qpois(p = 1 - tol, lambda = mu)))
    )
    if (long_) {
      # true long construction: no dense intermediate. The spacings walk is
      # done on the event vector with a segmented cumsum (one segment of
      # N_i + 1 exponentials per point process).
      seg_len <- N_events + 1L
      ends <- cumsum(seg_len) # global index of S_(N_i + 1), per process
      cs <- cumsum(stats::rexp(n = sum(seg_len), rate = 1))
      offs <- c(0, cs[ends])[seq_len(N_rows)] # cumsum before each segment
      S_total <- cs[ends] - offs
      id <- rep(seq_len(N_rows), times = N_events)
      if (length(id) == 0L) {
        return(list(id = integer(0), time = numeric(0), n_draws = N_rows))
      }
      pos <- sequence(N_events) # event index within its process
      gidx <- rep(c(0L, ends[-N_rows]), times = N_events) + pos
      warped <- (cs[gidx] - offs[id]) / S_total[id] * mu[id] + range_L[id, 1]
      # reporting truncations, per process, before the inversion
      if (rep_$first > 0L) {
        keep <- pos <= rep_$first
        id <- id[keep]
        warped <- warped[keep]
      } else if (rep_$last > 0L) {
        keep <- pos > N_events[id] - rep_$last
        id <- id[keep]
        warped <- warped[keep]
      }
      times <- if (use_legacy_call) {
        Lambda_inv(warped, Lambda_inv_args = Lambda_inv_args)
      } else {
        # align per-process arguments with the event vector
        .call_with_args(Lambda_inv, warped, .subset_fun_args(fa_, rows = id))
      }
      return(list(id = id, time = as.vector(times), n_draws = N_rows))
    }
    N_cols <- max(1L, N_events)
    warped_t <- matrix(stats::rexp(n = (N_cols + 1L) * N_rows, rate = 1), ncol = N_cols + 1L)
    matrix_cumsum_columns_inplace(warped_t)
    S_total <- warped_t[cbind(seq_len(N_rows), N_events + 1L)]
    warped_t <- warped_t[, seq_len(N_cols), drop = FALSE] * (mu / S_total) + range_L[, 1]
    warped_t[col(warped_t) > N_events] <- NA
  } else {
    N_cols <- max(stats::qpois(p = 1 - tol, lambda = 1 * (range_L[, 2] - range_L[, 1])))
    if (rep_$first > 0L && rep_$first < N_cols) {
      N_cols <- rep_$first
    }

    warped_t <- matrix(stats::rexp(n = N_cols * N_rows, rate = 1), ncol = N_cols)
    matrix_cumsum_columns_inplace(warped_t)
    warped_t <- warped_t + range_L[, 1]
    for (col in 1:N_cols) {
      in_range_L <- (warped_t[, col] <= range_L[, 2])
      if (col > 1 && all(!in_range_L)) {
        warped_t <- warped_t[, 1:(col - 1), drop = FALSE]
        break
      }
      warped_t[!in_range_L, col] <- NA
    }
  }
  Z <- if (use_legacy_call) {
    Lambda_inv(warped_t, Lambda_inv_args = Lambda_inv_args)
  } else {
    .call_with_args(Lambda_inv, warped_t, fa_$container)
  }
  Z <- .report_slice(Z, rep_)
  if (long_) {
    # the unconditioned R-level walk is inherently blocked (dense); only the
    # returned object is long
    return(.long_from_matrix(Z))
  }
  return(Z)
}
