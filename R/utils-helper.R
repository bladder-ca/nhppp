#' Helper functions
#'
#' Small utility functions. Not to be exported to the user.

#' @description helper function that augments
#' `test_that::expect_no_error()` to expect no error.
#' Copied from the `R6` source code.
#' @param expr Expression.
#' @return NULL
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
#' @return None
check_ppp_vector_validity <- function(times, t_min, t_max = NULL, size = NULL, atmost1 = FALSE, atleast1 = FALSE) {
  times <- times[!is.na(times)]
  if (atleast1) {
    testthat::expect_true(length(times) >= 1)
  }
  if (length(times != 0)) {
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



#' Check the validity of ppp samples
#'
#' @description Standard checks for a vector of ordered times. Check
#' that the `times` vector is sorted, has unique values, has all values
#' in `[t_min, t_max]`, and has length `size` (if applicable).
#'
#' @param times (vector, double | matrix) the times to be checked as vectors or matrices (time-vectors in rows)
#' @param t_min (double) the start of the time nterval
#' @param t_max (double) optional: the end of the time interval
#' @param size (double) optional: the size of the vector
#' @param atmost1 (boolean) optional: at most one sample returned
#' @param atleast1 (boolean) optional: at least one sample returned
#' @return None
check_ppp_sample_validity <- function(times, t_min, t_max = NULL, size = NULL, atmost1 = FALSE, atleast1 = FALSE) {
  if (!is.matrix(times)) {
    check_ppp_vector_validity(times = times, t_min = t_min, t_max = t_max, size = size, atmost1 = atmost1, atleast1 = atleast1)
  } else {
    for (i in 1:nrow(times)) {
      testthat::expect_identical(times[i, !is.na(times[i, ])], sort(times[i, ], na.last = NA))
      check_ppp_vector_validity(times = times[i, ], t_min = t_min, t_max = t_max, size = size, atmost1 = atmost1, atleast1 = atleast1)
    }
  }
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
