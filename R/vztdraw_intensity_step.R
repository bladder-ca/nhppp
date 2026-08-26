#' Vectorized sampling from a K-truncated NHPPP given the intensity function,
#' with piecewise constant majorizers over arbitrary interval bounds (R)
#'
#' @description
#' Thinning sampler conditional on the accepted event count lying in
#' `[generate_at_least_K, generate_at_most_K]` (`generate_at_least_K = 1`
#' alone is the zero-truncated process), with majorizer intervals of
#' arbitrary length (`time_breaks`, as in `vdraw_sc_step()`).
#'
#' Conditioning is by rejection: candidate realizations are proposed from the
#' majorizer conditioned on at least K1 majorizer events (only the lower
#' bound may be pushed into the proposal — at least K1 accepted events
#' implies at least K1 majorizer events, whereas an upper bound on the
#' majorizer count would over-restrict and bias the accepted law), thinned
#' against `lambda`, and rows whose surviving count falls outside `[K1, K2]`
#' are resampled until the condition holds. The per-round acceptance
#' probability degrades in the strictness of the bounds and in the looseness
#' of the majorizer, so a tight majorizer matters much more here than in the
#' unconditional sampler. There is no iteration cap.
#'
#' @inheritParams vdraw_intensity_step
#' @param generate_at_least_K non-negative integer: condition on at least K
#'        accepted events. The default 1 is the zero-truncated process.
#' @keywords internal
vztdraw_intensity_step <- function(
    lambda = NULL,
    lambda_args = NULL,
    Lambda_maj_matrix = NULL,
    lambda_maj_matrix = NULL,
    time_breaks = NULL,
    t_min = NULL,
    t_max = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    report_first_K = NULL,
    report_last_K = NULL,
    generate_at_least_K = 1,
    generate_at_most_K = NULL,
    budget_cap = NULL) {
  rep_ <- .resolve_reporting(atmost1, report_first_K, report_last_K)
  gen_ <- .resolve_generation(FALSE, generate_at_least_K, generate_at_most_K)
  if (gen_$at_least == 0L && gen_$at_most == 0L) {
    stop("at least one of `generate_at_least_K`/`generate_at_most_K` must be set for the conditioned (zt) samplers")
  }
  budget_cap <- .resolve_budget_cap(budget_cap, NULL)

  args <- .prep_vdraw_sc_step_args(
    lambda_matrix = lambda_maj_matrix,
    Lambda_matrix = Lambda_maj_matrix,
    time_breaks = time_breaks,
    t_min = t_min,
    t_max = t_max
  )
  n_draws <- nrow(args$rate)
  tb <- args$time_breaks
  use_subinterval <- !is.null(args$subinterval)
  sub <- if (use_subinterval) args$subinterval else matrix(0, 1, 2)

  has_vector_args <- !is.null(lambda_args$vector_arguments)
  if (has_vector_args) {
    stopifnot(data.table::is.data.table(lambda_args$vector_arguments))
    original_vector_arguments <- lambda_args$vector_arguments
  }

  draw_round <- function(rows) {
    la <- lambda_args
    if (has_vector_args) {
      la$vector_arguments <- original_vector_arguments[rows, , drop = FALSE]
    }
    l_ <- if (is.null(la)) lambda else function(X, ...) lambda(X, la)
    .Call(
      `_nhppp_vdraw_intensity_step_general`, l_,
      args$rate[rows, , drop = FALSE], args$is_cumulative,
      if (nrow(tb) == 1) tb else tb[rows, , drop = FALSE],
      if (nrow(sub) == 1) sub else sub[rows, , drop = FALSE],
      use_subinterval, tol, 0L, 0L, gen_$at_least, budget_cap
    )
  }

  fails_condition <- function(counts) {
    (counts < gen_$at_least) | (gen_$at_most > 0L & counts > gen_$at_most)
  }

  Z <- draw_round(seq_len(n_draws))
  needs_redraw <- fails_condition(rowSums(!is.na(Z)))

  while (any(needs_redraw)) {
    Z_add <- draw_round(which(needs_redraw))

    diff_cols <- ncol(Z_add) - ncol(Z)
    if (diff_cols > 0) {
      Z <- cbind(Z, matrix(NA_real_, nrow = nrow(Z), ncol = diff_cols))
    }
    if (diff_cols < 0) {
      Z_add <- cbind(Z_add, matrix(NA_real_, nrow = nrow(Z_add), ncol = -diff_cols))
    }

    Z[needs_redraw, ] <- Z_add
    needs_redraw <- fails_condition(rowSums(!is.na(Z)))
  }

  return(.report_slice(Z, rep_))
}
