% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_rsymphony_solver.R
\name{add_rsymphony_solver}
\alias{add_rsymphony_solver}
\title{Add a \emph{SYMPHONY} solver with \emph{Rsymphony}}
\usage{
add_rsymphony_solver(
  x,
  gap = 0.1,
  time_limit = .Machine$integer.max,
  first_feasible = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{x}{\code{\link[=problem]{problem()}} (i.e. \code{\linkS4class{ConservationProblem}}) object.}

\item{gap}{\code{numeric} gap to optimality. This gap is relative
and expresses the acceptable deviance from the optimal objective.
For example, a value of 0.01 will result in the solver stopping when
it has found a solution within 1\% of optimality.
Additionally, a value of 0 will result in the solver stopping
when it has found an optimal solution.
The default value is 0.1 (i.e. 10\% from optimality).}

\item{time_limit}{\code{numeric} time limit (seconds) for generating solutions.
The solver will return the current best solution when this time limit is
exceeded. The default value is the largest integer value
(i.e. \code{.Machine$integer.max}), effectively meaning that solver
will keep running until a solution within the optimality gap is found.}

\item{first_feasible}{\code{logical} should the first feasible solution be
be returned? If \code{first_feasible} is set to \code{TRUE}, the solver
will return the first solution it encounters that meets all the
constraints, regardless of solution quality. Note that the first feasible
solution is not an arbitrary solution, rather it is derived from the
relaxed solution, and is therefore often reasonably close to optimality.
Defaults to \code{FALSE}.}

\item{verbose}{\code{logical} should information be printed while solving
optimization problems? Defaults to \code{TRUE}.}
}
\value{
Object (i.e. \code{\linkS4class{ConservationProblem}}) with the solver
added to it.
}
\description{
Specify that the \href{https://projects.coin-or.org/SYMPHONY}{\emph{SYMPHONY}}
software (Ralphs & Güzelsoy 2005) -- using the \pkg{Rsymphony} package --
should be used to solve a conservation planning \code{\link[=problem]{problem()}}.
This function can also be used to customize the behavior of the solver.
It requires the \pkg{Rsymphony} package to be installed.
}
\details{
\href{https://projects.coin-or.org/SYMPHONY}{\emph{SYMPHONY}} is an
open-source mixed integer programming solver that is part of the
Computational
Infrastructure for Operations Research (COIN-OR) project.
The \pkg{Rsymphony} package
provides an interface to COIN-OR and -- unlike dependencies for other
solvers -- is available on \emph{CRAN}.
For information on the performance of different solvers,
please see Schuster \emph{et al.} (2020) for benchmarks comparing the
run time and solution quality of different solvers when applied to
different sized datasets.
}
\examples{
\dontrun{
# load data
data(sim_pu_raster, sim_features)

# create problem
p <- problem(sim_pu_raster, sim_features) \%>\%
     add_min_set_objective() \%>\%
     add_relative_targets(0.1) \%>\%
     add_binary_decisions() \%>\%
     add_rsymphony_solver(time_limit = 10, verbose = FALSE)

# generate solution
s <- solve(p)

# plot solution
plot(s, main = "solution", axes = FALSE, box = FALSE)
}
}
\references{
Ralphs TK and Güzelsoy M (2005) The SYMPHONY callable library for mixed
integer programming. In The Next Wave in Computing, Optimization, and
Decision Technologies (pp. 61--76). Springer, Boston, MA.

Schuster R, Hanson JO, Strimas-Mackey M, and Bennett JR (2020). Exact
integer linear programming solvers outperform simulated annealing for
solving conservation planning problems. \emph{PeerJ}, 8: e9258.
}
\seealso{
\link{solvers}.
}
