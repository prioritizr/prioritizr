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
  res <- lapply(x, run_presolve_check, header_level = 3)

  # extract problem names
  nms <- names(x)

  # if needed, set default names
  if (is.null(nms)) {
    nms <- as.character(seq_along(x)) # nocov
  }

  # prepare message
  msg <- unlist(
    lapply(seq_along(res), function(i) {
      if (isTRUE(res[[i]]$pass)) return(NULL)
      out <- c(
        cli::cli_fmt(cli::cli_h2(paste0("Problem: ", nms[[i]]))),
        ## note we exclude first element because it is an empty character
        ## designed to provide extra spacing, and extra space is not needed here
        res[[i]]$msg[-1]
      )
      if (!identical(i, length(res))) {
        out <- c(out, "")
      }
      out
    }),
    recursive = FALSE, use.names = TRUE
  )

  # return result
  list(
    pass = all(vapply(res, FUN.VALUE = logical(1), `[[`, "pass")),
    msg = msg
  )
}
