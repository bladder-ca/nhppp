#' @export
vdraw_sc_step_regular_cpp <- function(
    lambda= NULL,
    Lambda = NULL,
    range_t  = c(0, 10),
    tol = 10^-6,
    atmost1 = FALSE){
  if(!is.null(lambda) && is.null(Lambda)) {
    rate <- lambda
    is_cumulative_rate <- FALSE
  } else if(is.null(lambda) && !is.null(Lambda)) {
   rate <- Lambda
   is_cumulative_rate <- TRUE
  } else {
    stop("lambda and Lambda cannot both be `NULL`")
  }
  if(!is.matrix(range_t)) {
    range_t = matrix(rep(range_t, each = nrow(rate)), ncol = 2)
  } else if(all.equal(dim(range_t), c(1,2))) {
    range_t <- range_t[rep(1, nrow(rate)),]
  } else {
    stop("`range_t` should have as many rows as `lambda` or `Lambda`")
  }

  return(
    .Call(`_nhppp_vdraw_sc_step_regular`,
          rate, is_cumulative_rate, range_t, tol, atmost1)
  )
}
