#' @section Random numbers:
#' All samplers, in R and in C++, draw from R's global random number
#' generator, so `set.seed()` makes results reproducible. For stream-based
#' reproducibility or common random numbers, install a stream generator as R's
#' global generator before sampling, e.g. with the `rstream` package:
#' `s <- methods::new("rstream.mrg32k3a"); rstream::rstream.RNG(s)`.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom Rcpp sourceCpp
#' @useDynLib nhppp, .registration = TRUE
## usethis namespace: end
NULL
