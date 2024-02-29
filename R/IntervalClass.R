

#' Interval S3 class 
new_Interval <- function(x) {
  if(is.vector(x)) {
    return(
      structure(x, vectorized = FALSE, duration = x[2]-x[1], class = "Interval"))
  }
  if(is.matrix(x)) {
    return(structure(x, vectorized = TRUE, duration = x[,2]-x[,1], class = "Interval"))
  }
}

validate_Interval <- function(x){
  x <- unclass(x)
  if(is.vector(x)) {
    if(length(x) != 2) {
    stop("Invalid Interval vector length", 
         call. = FALSE)
    }
    if(x[1] >= x[2]) {
     stop("Invalid Interval vector values", 
         call. = FALSE) 
    }
  }
  if(is.matrix(x) {
    if(ncol(x) != 2) {
    stop("Invalid number of columns in Interval matrix", 
         call. = FALSE)
    }
    if(sum(x[2] <= x[1]) != 0) {
      stop("Invalid Interval matrix values", 
         call. = FALSE) 
    }
  }
  invisible(x)
}
