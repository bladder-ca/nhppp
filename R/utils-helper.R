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
