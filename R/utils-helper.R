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
#' in [t_min, t_max], and has length `size` (if applicable).
#'
#' @param times (vector, double) the times to be checked
#' @param t_min (double) the start of the time nterval
#' @param t_max (double) optional: the end of the time interval
#' @param size (double) optional: the size of the vector
#' @return NULL
check_ppp_sample_validity <- function(times, t_min, t_max = NULL, size = NULL, only1 = FALSE, zero_truncated = FALSE) {
  testthat::expect_identical(times, sort(times))
  testthat::expect_identical(times, unique(times))
  testthat::expect_true(min(times) >= t_min)
  if (!is.null(t_max)) {
    testthat::expect_true(max(times) <= t_max)
  }
  if (!is.null(size)) {
    testthat::expect_equal(length(times), size)
  }
  if (only1) {
    testthat::expect_true(length(times) <= 1)
  }
  if (zero_truncated) {
    testthat::expect_true(length(times) >= 1)
  }
}
