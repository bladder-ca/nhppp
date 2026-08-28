#' Long event format
#'
#' @description Internal helpers for the long ("`output = "long"`") event
#' representation: `list(id, time, n_draws)` with one entry per event.
#' `id` is the 1-based point-process index, ascending; `time` is ascending
#' within `id`. A point process with zero events contributes no entries (its
#' id is absent); `n_draws` distinguishes "no events" from "not sampled".
#' No `NA` appears in long output.
#' @name utils_long
#' @keywords internal
NULL

#' Resolve the `output` argument to the kernel flag
#' @noRd
.resolve_output <- function(output) {
  output <- match.arg(output, c("matrix", "long"))
  identical(output, "long")
}

#' Convert an NA-padded event matrix to the long event format
#' @noRd
.long_from_matrix <- function(Z) {
  if (!is.matrix(Z)) {
    Z <- matrix(Z, nrow = 1)
  }
  counts <- rowSums(!is.na(Z))
  # rows are left-aligned and ascending, so transposing groups the events
  # by point process in ascending time order
  tZ <- t(Z)
  list(
    id = rep(seq_len(nrow(Z)), times = counts),
    time = as.vector(tZ[!is.na(tZ)]),
    n_draws = nrow(Z)
  )
}

#' Convert the long event format back to an NA-padded event matrix
#' @noRd
.matrix_from_long <- function(x) {
  counts <- tabulate(x$id, nbins = x$n_draws)
  Z <- matrix(NA_real_, nrow = x$n_draws, ncol = max(1L, counts))
  if (length(x$time) > 0) {
    Z[cbind(x$id, sequence(counts))] <- x$time
  }
  Z
}
