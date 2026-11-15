# Structured argument containers for user-supplied functions
# (`lambda`, `Lambda`, `Lambda_inv`).
#
# The container keeps ALL of the function's arguments in one place, with two
# explicit channels:
#
#   lambda_args = list(
#     shared   = list(shape = 1.4, basis = B),  # row-invariant, any type;
#                                               #   stored once, never replicated
#     row_args = pop[, c("exponent")]           # one row per point process;
#   )                                           #   data.frame OR data.table,
#                                               #   accepted as-is (read-only),
#                                               #   nrow == n_draws validated,
#                                               #   auto-subset by rejection loops
#
# Delivery is structure-preserving and positional: the user function is
# called as f(t) when the container is NULL, else f(t, a) where `a` is
# exactly the container passed in, except `a$row_args` is already row-subset.
# The second formal's name is the user's choice.
#
# Back-compatibility:
# - a flat list with neither recognized key is treated as all-shared and
#   delivered as-is (the released `lambda_args = list(exponent = 1)` form);
# - a `$vector_arguments` key triggers a deprecation warning and EXACT legacy
#   behavior (container delivered as-is, `$vector_arguments` subset in loops);
# - mixing recognized and unrecognized top-level keys errors (ambiguous).

#' Normalize an args container into a mode descriptor
#'
#' @param args the container (`NULL`, flat list, structured list, or legacy
#'        `vector_arguments` list)
#' @param n_draws number of point processes (rows); used to validate
#'        `row_args`. `NULL` skips the validation (scalar callers).
#' @param arg_name the sampler-argument name, for error messages
#' @return list(mode = "none"|"flat"|"structured"|"legacy_va", container = args)
#' @noRd
.resolve_fun_args <- function(args, n_draws = NULL, arg_name = "lambda_args") {
  if (is.null(args)) {
    return(list(mode = "none", container = NULL))
  }
  if (!is.list(args) || is.data.frame(args)) {
    stop("`", arg_name, "` must be a named list (see the documented `shared`/`row_args` structure)")
  }

  keys <- names(args)
  recognized <- keys %in% c("shared", "row_args")

  if (!is.null(args$vector_arguments)) {
    warning(
      "the `vector_arguments` element of `", arg_name,
      "` is deprecated; use the `row_args` element instead",
      call. = FALSE
    )
    if (!is.data.frame(args$vector_arguments)) {
      stop("`", arg_name, "$vector_arguments` must be a data.frame or data.table")
    }
    if (!is.null(n_draws) && nrow(args$vector_arguments) != n_draws) {
      stop(
        "`", arg_name, "$vector_arguments` has ", nrow(args$vector_arguments),
        " rows but ", n_draws, " point processes are being sampled"
      )
    }
    return(list(mode = "legacy_va", container = args))
  }

  if (any(recognized)) {
    if (!all(recognized)) {
      stop(
        "`", arg_name, "` mixes the recognized elements (`shared`, `row_args`) with other ",
        "top-level elements (", paste(sQuote(keys[!recognized]), collapse = ", "),
        "); put row-invariant arguments inside `shared`"
      )
    }
    if (!is.null(args$shared) && (!is.list(args$shared) || is.data.frame(args$shared))) {
      stop("`", arg_name, "$shared` must be a named list")
    }
    if (!is.null(args$row_args)) {
      if (!is.data.frame(args$row_args)) {
        stop("`", arg_name, "$row_args` must be a data.frame or data.table (one row per point process)")
      }
      if (!is.null(n_draws) && nrow(args$row_args) != n_draws) {
        stop(
          "`", arg_name, "$row_args` has ", nrow(args$row_args),
          " rows but ", n_draws, " point processes are being sampled"
        )
      }
    }
    return(list(mode = "structured", container = args))
  }

  # flat list: all-shared, delivered as-is (released behavior, no warning)
  return(list(mode = "flat", container = args))
}

#' The container to deliver to the user function for a row subset
#'
#' @param resolved result of `.resolve_fun_args()`
#' @param rows integer row indices, or `NULL` for all rows
#' @noRd
.subset_fun_args <- function(resolved, rows = NULL) {
  container <- resolved$container
  if (is.null(rows)) {
    return(container)
  }
  if (resolved$mode == "structured" && !is.null(container$row_args)) {
    container$row_args <- container$row_args[rows, , drop = FALSE]
  } else if (resolved$mode == "legacy_va") {
    container$vector_arguments <- container$vector_arguments[rows, , drop = FALSE]
  }
  return(container)
}

#' Reduce a user function + container to the 1-argument callable the C++
#' thinning kernels require
#'
#' @param f the user function
#' @param container the (possibly row-subset) args container, or `NULL`
#' @noRd
.wrap_fun <- function(f, container) {
  if (is.null(container)) {
    return(f)
  }
  return(function(X, ...) f(X, container))
}

#' Call a user function with the positional-conditional convention
#' @noRd
.call_with_args <- function(f, x, container) {
  if (is.null(container)) {
    return(f(x))
  }
  return(f(x, container))
}
