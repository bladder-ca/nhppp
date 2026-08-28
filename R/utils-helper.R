#' Helper functions
#'
#' Small utility functions. Not to be exported to the user.

#' @description helper function that augments
#' `test_that::expect_no_error()` to expect no error.
#' Copied from the `R6` source code.
#' @param expr Expression.
#' @return NULL
#' @keywords internal
expect_no_error <- function(expr) {
  err <- FALSE
  tryCatch(force(expr),
    error = function(e) {
      err <<- TRUE
    }
  )
  testthat::expect(!err, "Expected no error, but had error.")
  invisible(NULL)
}

#' Read code from text file as string
#'
#' @param codeFile Path to file
#' @return \code{codeFile} contents as a character string
#' @keywords internal
read_code <- function(codeFile) {
  paste(readLines(file.path(codeFile)), collapse = "\n")
}





#' Check the validity of a ppp vector.
#'
#' @description Standard checks for a vector of ordered times. Check
#' that the `times` vector is sorted, has unique values, has all values
#' in `[t_min, t_max]`, and has length `size` (if applicable).
#'
#' @param times (vector, double) the times to be checked
#' @param t_min (double) the start of the time nterval
#' @param t_max (double) optional: the end of the time interval
#' @param size (double) optional: the size of the vector
#' @param atmost1 (boolean) optional: at most one sample returned
#' @param atleast1 (boolean) optional: at least one sample returned
#' @param atmostk (integer) optional: at most `k` samples returned
#' @param atleastk (integer) optional: at least `k` samples returned
#' @return None
#' @keywords internal
check_ppp_vector_validity <- function(times, t_min, t_max = NULL, size = NULL,
                                      atmost1 = FALSE, atleast1 = FALSE,
                                      atmostk = NULL, atleastk = NULL) {
  times <- times[!is.na(times)]
  if (atleast1) {
    testthat::expect_true(length(times) >= 1)
  }
  if (!is.null(atleastk)) {
    testthat::expect_true(length(times) >= atleastk)
  }
  if (!is.null(atmostk)) {
    testthat::expect_true(length(times) <= atmostk)
  }
  if (length(times) != 0) {
    testthat::expect_identical(times, sort(times))
    testthat::expect_identical(times, unique(times))
    testthat::expect_true(min(times, Inf) >= t_min)
    if (!is.null(t_max)) {
      testthat::expect_true(max(times, -Inf) <= t_max)
    }
    if (!is.null(size)) {
      testthat::expect_equal(length(times), size)
    }
    if (atmost1) {
      testthat::expect_true(length(times) <= 1)
    }
  }
}



#' Check the validity of ppp samples arranged in matrix format
#'
#' @description Standard checks for a matrix of ordered times
#' (event series in rows, times in columns). Check
#' that the times in the columns are sorted, have unique values
#' in `[t_min, t_max]`, and has length `size` (if applicable).
#'
#' @param times (vector, double | matrix) the times to be checked as vectors or matrices (time-vectors in rows)
#' @param t_min (double | vector) the start of the time nterval
#' @param t_max (double| vector) optional: the end of the time interval; if a vector, its length should match the number of rows of `times`.
#' @param size (double) optional: the size of the vector
#' @param atmost1 (boolean) optional: at most one sample returned
#' @param atleast1 (boolean) optional: at least one sample returned
#' @param atmostk (integer) optional: at most `k` samples returned per row
#' @param atleastk (integer) optional: at least `k` samples returned per row
#' @return None
#' @keywords internal
check_ppp_sample_validity <- function(times, t_min, t_max = NULL, size = NULL,
                                      atmost1 = FALSE, atleast1 = FALSE,
                                      atmostk = NULL, atleastk = NULL) {
  if (is.list(times) && !is.data.frame(times) &&
    all(c("id", "time", "n_draws") %in% names(times))) {
    return(check_ppp_long_validity(
      x = times, t_min = t_min, t_max = t_max, size = size,
      atmost1 = atmost1, atleast1 = atleast1, atmostk = atmostk, atleastk = atleastk
    ))
  }
  if (!is.matrix(times)) {
    check_ppp_vector_validity(
      times = times, t_min = t_min, t_max = t_max, size = size,
      atmost1 = atmost1, atleast1 = atleast1, atmostk = atmostk, atleastk = atleastk
    )
  } else {
    # All checks are whole-matrix operations with one expectation each --
    # per-row testthat expectations cost milliseconds of reporter overhead
    # apiece and dominate the suite runtime on 10^4-row fixtures.
    n <- nrow(times)
    K <- ncol(times)
    if (length(t_min) == 1) {
      t_min <- rep(t_min, n)
    }
    stopifnot(n == length(t_min))
    if (!is.null(t_max)) {
      if (length(t_max) == 1) {
        t_max <- rep(t_max, n)
      }
      stopifnot(n == length(t_max))
    }
    .which_rows <- function(ok) {
      w <- which(!ok)
      paste("failing rows:", paste(w[seq_len(min(10, length(w)))], collapse = ", "))
    }
    if (K > 1) {
      left <- times[, -K, drop = FALSE]
      right <- times[, -1, drop = FALSE]
      # strictly increasing within each row (sorted + unique in one check);
      # the NA-alignment check below makes NA differences safe to skip here
      d <- right - left
      sorted_ok <- rowSums(!(d > 0 | is.na(d))) == 0
      testthat::expect_true(all(sorted_ok), info = .which_rows(sorted_ok))
      # events are left-aligned: no non-NA entry to the right of an NA
      aligned_ok <- rowSums(is.na(left) & !is.na(right)) == 0
      testthat::expect_true(all(aligned_ok), info = .which_rows(aligned_ok))
    }
    # bounds: t_min / t_max have length nrow(times) and recycle down columns
    lower_ok <- rowSums(times < t_min, na.rm = TRUE) == 0
    testthat::expect_true(all(lower_ok), info = .which_rows(lower_ok))
    if (!is.null(t_max)) {
      upper_ok <- rowSums(times > t_max, na.rm = TRUE) == 0
      testthat::expect_true(all(upper_ok), info = .which_rows(upper_ok))
    }
    cnt <- rowSums(!is.na(times))
    if (atleast1) {
      testthat::expect_true(all(cnt >= 1), info = .which_rows(cnt >= 1))
    }
    if (!is.null(atleastk)) {
      testthat::expect_true(all(cnt >= atleastk), info = .which_rows(cnt >= atleastk))
    }
    if (!is.null(atmostk)) {
      testthat::expect_true(all(cnt <= atmostk), info = .which_rows(cnt <= atmostk))
    }
    if (atmost1) {
      testthat::expect_true(all(cnt <= 1), info = .which_rows(cnt <= 1))
    }
    if (!is.null(size)) {
      # empty rows are exempt, matching the vector check's nonzero-length guard
      size_ok <- cnt == 0 | cnt == size
      testthat::expect_true(all(size_ok), info = .which_rows(size_ok))
    }
  }
}

#' Check the validity of ppp samples in the long event format
#'
#' @description Standard checks for a long-format sample
#' `list(id, time, n_draws)`: ids in range and ascending, times ascending
#' within id, all times in `[t_min, t_max]`, and the per-process count
#' contracts. All checks are whole-vector operations.
#'
#' @param x (list) long event sample with elements `id`, `time`, `n_draws`
#' @param t_min (double | vector) the start of the time interval
#' @param t_max (double | vector) optional: the end of the time interval
#' @param size (double) optional: the per-process event count (empty
#'        processes are exempt, matching the matrix check)
#' @param atmost1 (boolean) optional: at most one event per process
#' @param atleast1 (boolean) optional: at least one event per process
#' @param atmostk (integer) optional: at most `k` events per process
#' @param atleastk (integer) optional: at least `k` events per process
#' @return None
#' @keywords internal
check_ppp_long_validity <- function(x, t_min, t_max = NULL, size = NULL,
                                    atmost1 = FALSE, atleast1 = FALSE,
                                    atmostk = NULL, atleastk = NULL) {
  .which_rows <- function(ok) {
    w <- which(!ok)
    paste("failing rows:", paste(w[seq_len(min(10, length(w)))], collapse = ", "))
  }
  n <- x$n_draws
  testthat::expect_true(is.numeric(x$id) && is.double(x$time) && length(n) == 1)
  testthat::expect_identical(length(x$id), length(x$time))
  testthat::expect_true(!anyNA(x$id) && !anyNA(x$time))
  testthat::expect_true(all(x$id >= 1L & x$id <= n))
  if (length(x$id) > 1) {
    di <- diff(x$id)
    testthat::expect_true(all(di >= 0)) # ids grouped and ascending
    testthat::expect_true(all(diff(x$time) > 0 | di > 0)) # ascending within id
  }
  t_min_e <- if (length(t_min) == 1) t_min else t_min[x$id]
  testthat::expect_true(all(x$time >= t_min_e))
  if (!is.null(t_max)) {
    t_max_e <- if (length(t_max) == 1) t_max else t_max[x$id]
    testthat::expect_true(all(x$time <= t_max_e))
  }
  cnt <- tabulate(x$id, nbins = n)
  if (atleast1) {
    testthat::expect_true(all(cnt >= 1), info = .which_rows(cnt >= 1))
  }
  if (!is.null(atleastk)) {
    testthat::expect_true(all(cnt >= atleastk), info = .which_rows(cnt >= atleastk))
  }
  if (!is.null(atmostk)) {
    testthat::expect_true(all(cnt <= atmostk), info = .which_rows(cnt <= atmostk))
  }
  if (atmost1) {
    testthat::expect_true(all(cnt <= 1), info = .which_rows(cnt <= 1))
  }
  if (!is.null(size)) {
    size_ok <- cnt == 0 | cnt == size
    testthat::expect_true(all(size_ok), info = .which_rows(size_ok))
  }
  invisible(NULL)
}

#' Check that two ppp vectors Q-Q agree
#'
#' @description Compare that the deciles of two vectors have absolute difference
#' over average ratios less than `threshold`
#'
#' @param ppp1 (vector, double) the first vector
#' @param ppp2 (vector, double) the second vector
#' @param threshold (double) optional: the cutoff for a large absolute threshold
#' @param showQQ (boolean) optional: show the QQ plot if the absolute value of the
#' Difference vs Average ratio in any decile is bigger than the `threshold`
#' @return None
#' @keywords internal
compare_ppp_vectors <- function(ppp1,
                                ppp2,
                                threshold = 0.15, showQQ = TRUE) {
  res <- stats::qqplot(ppp1, ppp2, plot.it = FALSE)
  r1 <- res[[1]]
  r2 <- res[[2]]
  step <- floor(length(r1) / 10)

  decile_check <- logical(0)
  for (i in 1:9) {
    indices <- ((i - 1) * step + 1):(i * step)
    DvsA <- 2 * (r1[indices] - r2[indices]) / (r1[indices] + r2[indices])
    tmp <- stats::t.test(x = DvsA)
    decile_check[i] <- abs(tmp$estimate) < threshold
  }
  indices <- ((9) * step + 1):length(r1)
  DvsA <- 2 * (r1[indices] - r2[indices]) / (r1[indices] + r2[indices])
  tmp <- stats::t.test(x = DvsA)
  decile_check[10] <- abs(tmp$estimate) < threshold

  if (!all(decile_check)) {
    stats::qqplot(r1, r2, plot.it = TRUE)
    graphics::lines(rep(min(c(r1, r2)), 2), rep(max(c(r1, r2)), 2))
  }
  testthat::expect_true(all(decile_check))
}
