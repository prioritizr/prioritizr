# Add a *SYMPHONY* solver with *lpsymphony*

Specify that the [*SYMPHONY*](https://github.com/coin-or/SYMPHONY)
software – using the lpsymphony package – should be used to solve a
conservation planning problem (Ralphs & Güzelsoy 2005). This function
can also be used to customize the behavior of the solver. It requires
the lpsymphony package to be installed (see below for installation
instructions).

## Usage

``` r
add_lpsymphony_solver(
  x,
  gap = 0.1,
  time_limit = .Machine$integer.max,
  first_feasible = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) or
  [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
  object.

- gap:

  `numeric` gap to optimality. This gap is relative and expresses the
  acceptable deviance from the optimal objective. For example, a value
  of 0.01 will result in the solver stopping when it has found a
  solution within 1% of optimality. Additionally, a value of 0 will
  result in the solver stopping when it has found an optimal solution.
  The default value is 0.1 (i.e., 10% from optimality).

- time_limit:

  `numeric` time limit (seconds) for generating solutions. The solver
  will return the current best solution when this time limit is
  exceeded. The default value is the largest integer value (i.e.,
  `.Machine$integer.max`), effectively meaning that solver will keep
  running until a solution within the optimality gap is found.

- first_feasible:

  `logical` should the first feasible solution be be returned? If
  `first_feasible` is set to `TRUE`, the solver will return the first
  solution it encounters that meets all the constraints, regardless of
  solution quality. Note that the first feasible solution is not an
  arbitrary solution, rather it is derived from the relaxed solution,
  and is therefore often reasonably close to optimality. Defaults to
  `FALSE`.

- verbose:

  `logical` should information be displayed while solving optimization
  problems? Defaults to `TRUE`.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the solver added to it.

## Details

[*SYMPHONY*](https://github.com/coin-or/SYMPHONY) is an open-source
mixed integer programming solver that is part of the Computational
Infrastructure for Operations Research (COIN-OR) project. This solver is
provided because it may be easier to install on some systems than the
Rsymphony package. Additionally – although the lpsymphony package
doesn't provide the functionality to specify the number of threads for
solving a problem – the lpsymphony package will solve problems using
parallel processing (unlike the Rsymphony package). As a consequence,
this solver will likely generate solutions much faster than the
[`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md).
Although formal benchmarks examining the performance of this solver have
yet to be completed, please see Schuster *et al.* (2020) for benchmarks
comparing the run time and solution quality of the Rsymphony solver.

## Installation

The lpsymphony package is distributed through
[Bioconductor](https://doi.org/doi:10.18129/B9.bioc.lpsymphony). To
install the lpsymphony package, please use the following code:

    if (!require(remotes)) install.packages("remotes")
    remotes::install_bioc("lpsymphony")

## References

Ralphs TK and Güzelsoy M (2005) The SYMPHONY callable library for mixed
integer programming. In The Next Wave in Computing, Optimization, and
Decision Technologies (pp. 61–76). Springer, Boston, MA.

Schuster R, Hanson JO, Strimas-Mackey M, and Bennett JR (2020). Exact
integer linear programming solvers outperform simulated annealing for
solving conservation planning problems. *PeerJ*, 8: e9258.

## See also

Other functions for adding solvers:
[`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md),
[`add_cplex_solver()`](https://prioritizr.net/reference/add_cplex_solver.md),
[`add_default_solver()`](https://prioritizr.net/reference/add_default_solver.md),
[`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md),
[`add_highs_solver()`](https://prioritizr.net/reference/add_highs_solver.md),
[`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)

## Examples

``` r
# \dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create problem
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.05) %>%
  add_proportion_decisions() %>%
  add_lpsymphony_solver(time_limit = 5, verbose = FALSE)

# generate solution
s <- solve(p)

# plot solution
plot(s, main = "solution", axes = FALSE)

# }
```
