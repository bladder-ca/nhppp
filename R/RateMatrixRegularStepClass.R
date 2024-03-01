#' Create new RateMatrixRegularStep object
#' @param x (matrix) numeric values for the rate matrix
#' @param cumulative (logical) is the rate matrix cumulative?
#' @return a RateMatrixRegularStep object
#' @export
new_RateMatrixRegularStep <- function(x, cumulative = logical()) {
  structure(x, cumulative = cumulative, class = "RateMatrixRegularStep")
}


#' Validate a RateMatrixRegularStep object
#' @param x (RateMatrixRegularStep) RateMatrixRegularStep object
#' @return RateMatrixRegularStep object if valid
#' @export
validate_RateMatrixRegularStep <- function(x) {
  x <- unclass(x)
  if(!is.matrix(x)) {
    stop("Rate must be a matrix", call. = FALSE)
  }
  if(!all(x>=0)) {
    stop("Rate values should be non-negative", call. = FALSE)
  }
  if(!all(!is.na(x))) {
    stop("Rate values should include NAs", call. = FALSE)
  }
  if(!all(!is.nan(x))) {
    stop("Rate values should include NaNs", call. = FALSE)
  }
  if(!attr(x, "cumulative") %in% c(TRUE, FALSE)) {
    stop("Rate attribute `cumulative` should be `TRUE` or `FALSE`", call. = FALSE)
  }
  if(attr(x, "cumulative")) {
    values_increase <- TRUE
    if(ncol(x)>1) {
      for(cc in 2:ncol(x)) {
        values_increase <- values_increase & all(x[,cc] >= x[,cc-1])
      }
    }
    if(!values_increase) {
      stop("Rate is cumulative but the column values do not increase")
    }
  }
  if(mode(x)!= "numeric") {
    mode(x) <- "numeric"
  }
  invisible(x)
}

#' Make a RateMatrixRegularStep object cumulative
#' @param x (RateMatrixRegularStep) RateMatrixRegularStep object
#' @param interval_duration (double, vector) the length of the intervals
#' @return a RateMatrixRegularStep object (cumulative = TRUE)
#' @export
make_cumulative_RateMatrixRegularStep <- function(x, interval_duration) {
  if(attr(x, "cumulative")) {
    return(x)
  }
  x <- unclass(x)
  x <- mat_cumsum_columns(x) * interval_duration
  return(new_RateMatrixRegularStep(x, cumulative = TRUE))
}

#' Make a RateMatrixRegularStep object instantaneous
#' @param x (RateMatrixRegularStep) RateMatrixRegularStep object
#' @param interval_duration (double, vector) the length of the intervals
#' @return a RateMatrixRegularStep object (cumulative = FALSE)
#' @export
make_instantaneous_RateMatrixRegularStep <- function(x, interval_duration) {
  if(!attr(x, "cumulative")) {
    return(x)
  }
  x <- unclass(x)
  x <- mat_diff_columns(x) / interval_duration
  return(new_RateMatrixRegularStep(x, cumulative = FALSE))
}
