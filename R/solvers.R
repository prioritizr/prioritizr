#' @include Solver-proto.R
NULL

#' Problem solvers
#'
#' Specify the software and configuration used to solve a conservation planning
#' [problem()]. By default, the best available
#' software currently installed on the system will be used.
#'
#' @details The following solvers can be used to find solutions for a
#'   conservation planning [problem()]:
#'
#'   \describe{
#'
#'   \item{`add_default_solver`}{This solver uses the best software
#'     currently installed on the system.}
#'
#'   \item{[add_gurobi_solver()]}{[*Gurobi*](https://www.gurobi.com/)
#'     is a state-of-the-art commercial optimization software with an R package
#'     interface. It is by far the fastest of the solvers available for
#'     generating prioritizations, however, it is not freely available. That
#'    said, licenses are available to academics at no cost. The
#'     \pkg{gurobi} package is distributed with the *Gurobi* software
#'     suite. This solver uses the \pkg{gurobi} package to solve problems.}
#'
#'   \item{[add_cplex_solver()]}{[*IBM CPLEX*](https://www.ibm.com/analytics/cplex-optimizer)
#'     is a commercial optimization software. It is faster than the open
#'     source solvers available for generating prioritizations, however, it
#'     is not freely available.
#'     Similar to the [*Gurobi*](https://www.gurobi.com/)
#'     software, licenses are available to academics at no cost.
#'     This solver uses the \pkg{cplexAPI} package to solve problems using
#'     *IBM CPLEX*.}
#'
#'   \item{[add_rsymphony_solver()]}{
#'     [*SYMPHONY*](https://projects.coin-or.org/SYMPHONY) is an
#'     open-source integer programming solver that is part of the Computational
#'     Infrastructure for Operations Research (COIN-OR) project, an initiative
#'     to promote development of open-source tools for operations research (a
#'     field that includes linear programming). The \pkg{Rsymphony} package
#'     provides an interface to COIN-OR and is available on CRAN. This solver
#'     uses the \pkg{Rsymphony} package to solve problems.}
#'
#'   \item{[add_lpsymphony_solver()]}{The \pkg{lpsymphony} package
#'     provides a different interface to the COIN-OR software suite. Unlike the
#'     \pkg{Rsymhpony} package, the \pkg{lpsymphony} package is distributed
#'     through
#'     [Bioconductor](https://doi.org/doi:10.18129/B9.bioc.lpsymphony).
#'     The \pkg{lpsymphony} package may be easier to install on Windows or
#'     Max OSX systems than the \pkg{Rsymphony} package.}
#'
#' }
#'
#' @name solvers
#'
#' @seealso [constraints],  [decisions],
#'  [objectives], [penalties],
#'  [portfolios], [problem()],
#'  [targets].
#'
#' @examples
#' \dontrun{
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create basic problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions()
#'
#' # create vector to store plot titles
#' titles <- c()
#'
#' # create empty stack to store solutions
#' s <- stack()
#'
#' # create problem with added rsymphony solver and limit the time spent
#' # searching for the optimal solution to 2 seconds
#' if (require("Rsymphony")) {
#'   titles <- c(titles, "Rsymphony (2s)")
#'   p1 <- p %>% add_rsymphony_solver(time_limit = 2)
#'   s <- addLayer(s, solve(p1))
#' }
#'
#' # create problem with added rsymphony solver and limit the time spent
#' # searching for the optimal solution to 5 seconds
#' if (require("Rsymphony")) {
#'   titles <- c(titles, "Rsymphony (5s)")
#'   p2 <- p %>% add_rsymphony_solver(time_limit = 5)
#'   s <- addLayer(s, solve(p2))
#' }
#'
#' # if the gurobi is installed: create problem with added gurobi solver
#' if (require("gurobi")) {
#'   titles <- c(titles, "gurobi (5s)")
#'   p3 <- p %>% add_gurobi_solver(gap = 0.1, presolve = 2, time_limit = 5)
#'   s <- addLayer(s, solve(p3))
#' }
#'
#' # if the cplexAPI is installed: create problem with added cplex solver
#' if (require("cplexAPI")) {
#'   titles <- c(titles, "cplexAPI (5s)")
#'   p4 <- p %>% add_cplex_solver(gap = 0.1, time_limit = 5)
#'   s <- addLayer(s, solve(p4))
#' }
#'
#' # if the lpsymphony is installed: create problem with added lpsymphony solver
#' # note that this solver is skipped on Linux systems due to instability
#' # issues
#' if (require("lpsymphony") &
#'     isTRUE(Sys.info()[["sysname"]] != "Linux")) {
#'   titles <- c(titles, "lpsymphony")
#'   p5 <- p %>% add_lpsymphony_solver(gap = 0.1, time_limit = 10)
#'   s <- addLayer(s, solve(p5))
#' }
#'
#' # plot solutions
#' plot(s, main = titles, axes = FALSE, box = FALSE)
#' }
#'
NULL
