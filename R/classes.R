new_Interval <- function(range_t) {
  structure(range_t, class = "Interval")
}




new_lambdaStepRegular <- function(lambda_mat, range_t) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))

  structure(
    x,
    levels = levels,
    class = "factor"
  )
}
