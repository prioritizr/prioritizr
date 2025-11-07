# ---
# repo: prioritizr/prioritizr
# file: standalone-assertions_solvers.R
# dependencies: standalone-assertions_handlers.R
# imports: [assertthat (>= 0.2.0)]
# ---

# Please note that you will need to manually add the following packages to the
# Suggests field in the DESCRIPTION file:
#
# gurobi (>= 8.0-1),
# rcbc (>= 0.1.0.9001),
# cplexAPI (>= 1.4.0),
# lpsymphony (>= 1.17.0),
# Rsymphony (>= 0.1-31),
# highs (>= 0.1-10)

#' Any solvers installed?
#'
#' Test if any solvers are installed.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{Rsymphony}, \pkg{lpsymphony}, \pkg{gurobi}.
#'
#' @return `logical` value indicating if any solvers are installed.
#'
#' @noRd
any_solvers_installed <- function() {
  !is.null(default_solver_name())
}

assertthat::on_failure(any_solvers_installed) <- function(call, env) {
  c(
    "No optimization solvers are installed.",
    "x" = "You must install a solver to generate prioritizations.",
    "i" = "See {.topic solvers} for options."
  )
}

#' Default solver name
#'
#' This function returns the name of the default solver. If no solvers are
#' detected on the system, then a `NULL` object is returned.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{gurobi}, \pkg{cplexAPI}, \pkg{rcbc}, \pkg{highs},
#'   \pkg{lpsymphony}, \pkg{Rsymphony}.
#'
#' @return `character` indicating the name of the default solver.
#'
#' @noRd
default_solver_name <- function() {
  if (requireNamespace("gurobi", quietly = TRUE)) {
    return("gurobi")
  } else if (requireNamespace("cplexAPI", quietly = TRUE)) {
    return("cplexAPI")
  } else if (requireNamespace("rcbc", quietly = TRUE)) {
    return("rcbc")
  } else if (requireNamespace("highs", quietly = TRUE)) {
    return("highs")
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    return("lpsymphony")
  } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
    return("Rsymphony")
  } else {
    return(NULL)
  }
}
