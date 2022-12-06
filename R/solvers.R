#' @include Solver-proto.R
NULL

#' Problem solvers
#'
#' Specify the software and configuration used to solve a conservation planning
#' [problem()]. By default, the best available
#' software currently installed on the system will be used.
#' For information on the performance of different solvers,
#' please see Schuster _et al._ (2020) for benchmarks comparing the
#' run time and solution quality of some of these solvers when applied to
#' different sized datasets.
#'
#' @details The following solvers can be used to find solutions for a
#'   conservation planning [problem()]:
#'
#'   \describe{
#'
#'   \item{[add_default_solver()]}{This solver uses the best software
#'     currently installed on the system.}
#'
#'   \item{[add_gurobi_solver()]}{[*Gurobi*](https://www.gurobi.com/)
#'     is a state-of-the-art commercial optimization software with an R package
#'     interface. We recommend using this solver if at all possible.
#'     It is by far the fastest of the solvers available for
#'     generating prioritizations, however, it is not freely available. That
#'     said, licenses are available to academics at no cost. The
#'     \pkg{gurobi} package is distributed with the *Gurobi* software
#'     suite. This solver uses the \pkg{gurobi} package to solve problems.}
#'
#'   \item{[add_cplex_solver()]}{[*IBM CPLEX*](  https://www.ibm.com/products/ilog-cplex-optimization-studio/cplex-optimizer)
#'     is a commercial optimization software. It is faster than the open
#'     source solvers available for generating prioritizations, however, it
#'     is not freely available.
#'     Similar to the [*Gurobi*](https://www.gurobi.com/)
#'     software, licenses are available to academics at no cost.
#'     This solver uses the \pkg{cplexAPI} package to solve problems using
#'     *IBM CPLEX*.}
#'
#'  \item{[add_cbc_solver()]}{[*CBC*](https://github.com/coin-or/Cbc) is an
#'    open-source mixed integer programming solver that is part of the
#'     Computational Infrastructure for Operations Research (COIN-OR) project.
#'     Preliminary benchmarks indicate that it is the fastest open source
#'     solver currently supported.
#'     We recommend using this solver if both *Gurobi* and *IBM CPLEX* are
#'     unavailable.
#'     It requires the \pkg{rcbc} package, which is currently only available on
#'     [GitHub](https://github.com/dirkschumacher/rcbc).
#'  }
#'
#'  \item{[add_highs_solver()]}{[*HiGHS*](https://highs.dev/) is an open
#'    source optimization software. Although this solver can have
#'    comparable performance to the *CBC* solver for particular problems and is
#'    generally faster than the *SYMPHONY* based solvers (see below), it
#'    sometimes can take much longer than the *CBC* solver for particular
#'    problems.
#'  }
#'
#'   \item{[add_lpsymphony_solver()]}{
#'     [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) is an
#'     open-source mixed integer programming solver that is also part of the
#'     COIN-OR project. Although both *SYMPHONY* and *CBC* are part of
#'     the COIN-OR project, they are different software.
#'     The \pkg{lpsymphony} package provides an interface to the *SYMPHONY*
#'     software, and is distributed through
#'     [Bioconductor](https://doi.org/doi:10.18129/B9.bioc.lpsymphony).
#'     We recommend using this solver if the *CBC* and *HiGHS* solvers cannot
#'     be installed.
#'     This solver can use parallel processing to solve problems, so it is
#'     faster than \pkg{Rsymphony} package interface (see below).
#'  }
#'
#'   \item{[add_rsymphony_solver()]}{
#'     This solver provides an alternative interface to the
#'     [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) solver using
#'     the \pkg{Rsymphony} package.
#'     Unlike other solvers, the \pkg{Rsymphony} package can be installed
#'     directly from the Comprehensive R Archive Network (CRAN).
#'     It is also the slowest of the available solvers.}
#'
#' }
#'
#' @name solvers
#'
#' @family overviews
#'
#' @references
#' Schuster R, Hanson JO, Strimas-Mackey M, and Bennett JR (2020). Exact
#' integer linear programming solvers outperform simulated annealing for
#' solving conservation planning problems. *PeerJ*, 8: e9258.
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
#'   add_proportion_decisions()
#'
#' # create vector to store plot titles
#' titles <- c()
#'
#' # create empty stack to store solutions
#' s <- stack()
#'
#' # if gurobi is installed: create problem with added gurobi solver
#' if (require("gurobi")) {
#'   titles <- c(titles, "gurobi")
#'   p1 <- p %>% add_gurobi_solver(verbose = FALSE)
#'   s <- addLayer(s, solve(p1))
#' }
#'
#' # if cplexAPI is installed: create problem with added CPLEX solver
#' if (require("cplexAPI")) {
#'   titles <- c(titles, "CPLEX")
#'   p2 <- p %>% add_cplex_solver(verbose = FALSE)
#'   s <- addLayer(s, solve(p2))
#' }
#'
#' # if rcbc is installed: create problem with added CBC solver
#' if (require("rcbc")) {
#'   titles <- c(titles, "CBC")
#'   p3 <- p %>% add_cbc_solver(verbose = FALSE)
#'   s <- addLayer(s, solve(p3))
#' }
#'
#' # if highs is installed: create problem with added HiGHs solver
#' if (require("highs")) {
#'   titles <- c(titles, "HiGHS")
#'   p4 <- p %>% add_highs_solver(verbose = FALSE)
#'   s <- addLayer(s, solve(p4))
#' }
#'
#' # create problem with added rsymphony solver
#' if (require("Rsymphony")) {
#'   titles <- c(titles, "Rsymphony")
#'   p5 <- p %>% add_rsymphony_solver(verbose = FALSE)
#'   s <- addLayer(s, solve(p25))
#' }
#'
#' # if lpsymphony is installed: create problem with added lpsymphony solver
#' if (require("lpsymphony")) {
#'   titles <- c(titles, "lpsymphony")
#'   p6 <- p %>% add_lpsymphony_solver(verbose = FALSE)
#'   s <- addLayer(s, solve(p6))
#' }
#'
#' # plot solutions
#' plot(s, main = titles, axes = FALSE, box = FALSE)
#' }
#'
NULL
