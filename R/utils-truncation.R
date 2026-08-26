# Resolution of the reporting / generation-conditioning / budget options
# shared by the samplers.
#
# Two orthogonal option classes:
# - REPORTING (`report_first_K`, `report_last_K`): act on one realization,
#   returning only the earliest / latest min(N, K) events; the count law is
#   unchanged. At most one may be set. `atmost1` is the alias for
#   `report_first_K = 1`.
# - GENERATION conditioning (`generate_at_least_K`, `generate_at_most_K`):
#   change the sampled law to X | K1 <= N <= K2 (order-statistics sampling
#   path). Both may be set (K1 <= K2); K1 = K2 conditions on exactly K
#   events. `atleast1` is the alias for `generate_at_least_K = 1`.
# - `budget_cap` (formerly `atmostB`): computational cap on the event budget
#   of the vectorized kernels — an approximation knob (jointly with the
#   `1 - tol` quantile bound), not an exact contract; it never truncates the
#   count below `generate_at_least_K`.

#' @noRd
.check_pos_int <- function(x, name) {
  x <- as.integer(x)
  if (length(x) != 1L || is.na(x) || x < 1L) {
    stop("`", name, "` must be a positive integer scalar")
  }
  return(x)
}

#' Resolve the reporting options into the C++ arguments
#'
#' @description Returns `list(first = , last = )` as integers with 0 meaning
#' "off". At most one of the two may be active; `atmost1` is the alias for
#' `report_first_K = 1`.
#' @noRd
.resolve_reporting <- function(atmost1 = FALSE, report_first_K = NULL, report_last_K = NULL) {
  first <- if (is.null(report_first_K)) 0L else .check_pos_int(report_first_K, "report_first_K")
  last <- if (is.null(report_last_K)) 0L else .check_pos_int(report_last_K, "report_last_K")
  if (isTRUE(atmost1)) {
    if (first == 0L && last == 0L) {
      first <- 1L
    } else if (first != 1L || last != 0L) {
      stop("`atmost1 = TRUE` contradicts the `report_first_K`/`report_last_K` settings; specify one form")
    }
  }
  if (first > 0L && last > 0L) {
    stop("only one of `report_first_K` and `report_last_K` may be set")
  }
  return(list(first = first, last = last))
}

#' Resolve the generation-conditioning options into the C++ arguments
#'
#' @description Returns `list(at_least = , at_most = )` as integers with 0
#' meaning "off". `atleast1` is the alias for `generate_at_least_K = 1`.
#' @noRd
.resolve_generation <- function(atleast1 = FALSE, generate_at_least_K = NULL, generate_at_most_K = NULL) {
  at_least <- if (is.null(generate_at_least_K)) 0L else .check_pos_int(generate_at_least_K, "generate_at_least_K")
  at_most <- if (is.null(generate_at_most_K)) 0L else .check_pos_int(generate_at_most_K, "generate_at_most_K")
  if (isTRUE(atleast1)) {
    if (at_least == 0L) {
      at_least <- 1L
    } else if (at_least != 1L) {
      stop("`atleast1 = TRUE` contradicts `generate_at_least_K = ", at_least, "`; specify one form")
    }
  }
  if (at_least > 0L && at_most > 0L && at_least > at_most) {
    stop("`generate_at_least_K = ", at_least, "` exceeds `generate_at_most_K = ", at_most, "`")
  }
  return(list(at_least = at_least, at_most = at_most))
}

#' Resolve `budget_cap`, honoring the deprecated `atmostB` alias
#' @noRd
.resolve_budget_cap <- function(budget_cap = NULL, atmostB = NULL) {
  if (!is.null(atmostB)) {
    warning("`atmostB` is deprecated; use `budget_cap`", call. = FALSE)
    if (is.null(budget_cap)) {
      budget_cap <- atmostB
    } else if (budget_cap != atmostB) {
      stop("`atmostB` (deprecated) and `budget_cap` disagree; use only `budget_cap`")
    }
  }
  if (is.null(budget_cap)) {
    return(0L)
  }
  return(.check_pos_int(budget_cap, "budget_cap"))
}

# Note: `generate_at_least_K > report_first_K` (or `> report_last_K`) is
# deliberately allowed — conditioning on at least K events and reporting only
# M < K of them is well-defined (the conditioned count N is drawn first; the
# reporting slice is taken from the N order statistics).

#' Apply resolved reporting options to a left-aligned NA-padded event matrix
#' @param Z matrix of ascending event times per row, NA-padded on the right
#' @param rep_ result of `.resolve_reporting()`
#' @noRd
.report_slice <- function(Z, rep_) {
  if (rep_$first > 0L) {
    if (ncol(Z) > rep_$first) {
      Z <- Z[, seq_len(rep_$first), drop = FALSE]
    }
    return(Z)
  }
  if (rep_$last > 0L) {
    counts <- rowSums(!is.na(Z))
    n_col_out <- max(1L, min(rep_$last, max(counts)))
    out <- matrix(NA_real_, nrow = nrow(Z), ncol = n_col_out)
    for (i in seq_len(nrow(Z))) {
      if (counts[i] > 0L) {
        keep <- min(rep_$last, counts[i])
        out[i, seq_len(keep)] <- Z[i, (counts[i] - keep + 1L):counts[i]]
      }
    }
    return(out)
  }
  return(Z)
}

#' Apply resolved reporting options to an ascending event-time vector
#' @noRd
.report_slice_vector <- function(x, rep_) {
  if (rep_$first > 0L && length(x) > rep_$first) {
    return(x[seq_len(rep_$first)])
  }
  if (rep_$last > 0L && length(x) > rep_$last) {
    return(x[(length(x) - rep_$last + 1L):length(x)])
  }
  return(x)
}
