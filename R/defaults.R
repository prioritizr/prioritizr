#' @include solvers.R objectives.R decisions.R
NULL

#' @rdname solvers
#' @export
default_solver <- function() {
  if (assert_that(requireNamespace("gurobi", quietly = TRUE))) {
    return(gurobi_solver())
  } else if (assert_that(requireNamespace("lpsymphony", quietly = TRUE))) {
    return(lpsymphony_solver())
  } else if (assert_that(requireNamespace("Rsymphony", quietly = TRUE))) {
    return(rsymphony_solver())
  } else {
    return(lpsolve_solver())
  }
}

#' @rdname decisions
default_decision <- function() {
  binary_decision()
}
