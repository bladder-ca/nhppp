#' Generate Interval (S3 class) object
#' @param x (vector, matrix) numeric values for the interval
#' @return an Interval object
#' @export
new_Interval <- function(x) {
  if(is.vector(x)) {
    x <- structure(x, vectorized = FALSE, duration = x[2]- x[1], class = "Interval")
    return(x)
  }
  if(is.matrix(x)) {
    x <- structure(x, vectorized = TRUE, duration = x[,2]- x[,1], class = "Interval")
    return(x)
  }
}

#' Validate an Interval object
#' @param x (Interval) Interval object
#' @return Interval object if valid
#' @export
validate_Interval <- function(x){
  stopifnot("Interval" %in% class(x))
  if(sum(is.na(x))>0) {
    stop("Interval contains NAs", call. = FALSE)
  }
  if(sum(is.nan(x))>0) {
    stop("Interval contains NaNs", call. = FALSE)
  }
  if(is.matrix(x)) {
    if(ncol(x) != 2) {
    stop("Invalid number of columns in Interval matrix",
         call. = FALSE)
    }
    if(sum(x[,2] <= x[,1]) != 0) {
      stop("Invalid Interval matrix values",
         call. = FALSE)
    }
  } else {
    if(length(x) != 2) {
      stop("Invalid Interval vector length",
           call. = FALSE)
    }
    if(x[1] >= x[2]) {
      stop("Invalid Interval vector values",
           call. = FALSE)
    }
  }
  invisible(x)
}


