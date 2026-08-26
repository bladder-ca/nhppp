#' Vectorized sampling from a K-truncated NHPPP given the intensity function,
#' with piecewise constant majorizers over arbitrary interval bounds (R)
#'
#' @description
#' Thinning sampler conditional on observing at least `atleastK` accepted
#' events (`atleastK = 1` is the zero-truncated process), with majorizer
#' intervals of arbitrary length (`time_breaks`, as in `vdraw_sc_step()`).
#'
#' Conditioning is by rejection: candidate realizations are proposed from the
#' majorizer conditioned on at least K majorizer events (exact, because at
#' least K accepted events implies at least K majorizer events), thinned
#' against `lambda`, and rows with fewer than K surviving events are
#' resampled until the condition holds. The per-round acceptance probability
#' degrades in K and in the looseness of the majorizer, so a tight majorizer
#' matters much more here than in the unconditional sampler. There is no
#' iteration cap.
#'
#' @inheritParams vdraw_intensity_step
#' @param atleastK positive integer: condition on at least K accepted events.
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
    atmostK = NULL,
    atleastK = 1,
    budget_cap = NULL) {
  atmostK <- .resolve_atmostK(atmost1, atmostK)
  atleastK <- .resolve_atleastK(atleast1 = FALSE, atleastK = atleastK)
  if (atleastK < 1L) stop("`atleastK` must be >= 1 for the truncated (zt) samplers")
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
      use_subinterval, tol, 0L, atleastK, budget_cap
    )
  }

  Z <- draw_round(seq_len(n_draws))
  needs_redraw <- rowSums(!is.na(Z)) < atleastK

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
    needs_redraw <- rowSums(!is.na(Z)) < atleastK
  }

  # accepted times are sorted within a row: the first K columns hold the
  # earliest K events of the conditioned realization
  if (atmostK > 0L && ncol(Z) > atmostK) {
    Z <- Z[, seq_len(atmostK), drop = FALSE]
  }
  return(Z)
}
