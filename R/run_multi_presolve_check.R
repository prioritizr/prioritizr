#' @include internal.R run_presolve_check.R
NULL

#' Run presolve check on multiple optimization problems
#'
#' This internal function is used to perform the presolve checks.
#'
#' @param x `list` of [`OptimizationProblem-class`] objects.
#'
#' @return
#' A `list` with containing a (`$msg`) `character` vector with information on
#' the presolve checks and (`$pass`) `logical` value indicating if the
#' checks were passed.
#'
#' @noRd
run_multi_presolve_check <- function(x) {
  # assert that arguments are valid
  assert(
    inherits(x, "list"),
    .internal = TRUE,
    call = call
  )

  # run checks
  res <- lapply(x, run_presolve_check)

  # extract problem names
  nms <- names(x)

  # if needed, set default names
  if (is.null(nms)) {
    nms <- paste0("Problem ", seq_along(x)) # nocov
  }

  # prepare message
  msg <- unlist(
    lapply(seq_along(res), function(i) {
      if (isTRUE(res[[i]]$pass)) return(NULL)
      c(cli::cli_fmt(cli::cli_h2(nms[[i]])), res[[i]]$msg)
    }),
    recursive = FALSE, use.names = TRUE
  )

  # return result
  list(
    pass = all(vapply(res, FUN.VALUE = logical(1), `[[`, "pass")),
    msg = msg
  )
}
