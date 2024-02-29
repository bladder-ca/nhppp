new_RateMatrix <- function(x, cumulative = logical()) {
  structure(x, cumulative = cumulative, class = "RateMatrix")
}

validate_RateMatrix <- function(x) {
  x <- unclass(x)
  if(!is.matrix(x) {
    stop("Rate must be a matrix", call. = FALSE)
  }
  if(!all(x>=0)) {
    stop("Rate values should be non-negative", call. = FALSE)
  }
  if(!attr(x, "cumulative") %in% (TRUE, FALSE)) {
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

make_cumulative_RateMatrix <- function(x, interval_duration) {
  if(attr(x, "cumulative")) {
    return(x)
  }
  x <- unclass(x)
  x <- mat_cumsum_columns(x) * interval_duration
  return(new_Rate(x, cumulative = TRUE))
} 

make_instantaneous_RateMatrix <- function(x, interval_duration) {
  if(!attr(x, "cumulative")) {
    return(x)
  }
  x <- unclass(x)
  x <- mat_diff_columns(x) / interval_duration
  return(new_Rate(x, cumulative = FALSE))
} 
