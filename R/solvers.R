#' @include Solver-proto.R
NULL

#' Problem solvers
#'
#' Specify the software and configurations used to solve a conservation planning
#' problem. Below is a list of different solvers that can be added to a
#' \code{\link{ConservationProblem-class}} object. By default the best available 
#' software currently installed on the system will be used. 
#'
#' \describe{
#'
#'  \item{\code{default_solver}}{This solver uses the best software
#'    currently installed on the system.}
#'
#'   \item{\code{\link{add_gurobi_solver}}}{\href{http://gurobi.com}{Gurobi} is a
#'     state-of-the-art commercial optimization software with an R package
#'     interface. It is by far the fastest of the solvers available in this
#'     package, however, it is also the only solver that is not freely
#'     available. That said, licenses are available to academics at no cost. The
#'     \code{gurobi} package is distributed with the Gurobi software suite.
#'     This solver uses the \code{gurobi} package to solve problems.}
#'
#'   \item{\code{\link{add_rsymphony_solver}}}{
#'     \href{https://projects.coin-or.org/SYMPHONY}{SYMPHONY} is an open-source
#'     integer programming solver that is part of the Computational
#'     Infrastructure for Operations Research (COIN-OR) project, an initiative
#'     to promote development of open-source tools for operations research (a
#'     field that includes linear programming). The \code{Rsymphony} package
#'     provides an interface to COIN-OR and is available on CRAN. This solver
#'     uses the \code{Rsymphony} package to solve problems.}
#'
#'  \item{\code{\link{add_lpsymphony_solver}}}{The \code{lpsymphony} package provides a
#'    different interface to the COIN-OR software suite. Unlike the
#'    \code{Rsymhpony} package, the \code{lpsymphony} package is distributed
#'    through
#'    \href{http://bioconducto/packages/release/bioc/html/lpsymphony.html}{Bioconductor}.
#'    On Windows and Mac, \code{lpsymphony} may be easier to install. 
#'    This solver uses the \code{lpsymphony} package
#'    to solve.}
#'
#' }
#'
#'
#' @name solvers
NULL

#' @export
add_default_solver <- function(x, ...) {
  if (requireNamespace("gurobi", quietly = TRUE)) {
    return(add_gurobi_solver(x, ...))
  } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
    return(add_rsymphony_solver(x, ...))
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    return(add_lpsymphony_solver(x, ...))
  } else {
    stop("no optimization problem solvers found on system.")
  }
}
