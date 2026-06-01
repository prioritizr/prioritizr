# Add a *SYMPHONY* solver with *Rsymphony*

Specify that the [*SYMPHONY*](https://github.com/coin-or/SYMPHONY)
software – using the Rsymphony package – should be used to solve a
conservation planning problem (Ralphs & Güzelsoy 2005). This function
can also be used to customize the behavior of the solver. It requires
the Rsymphony package to be installed.

## Usage

``` r
add_rsymphony_solver(
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

  `numeric` value denoting the optimality gap. This gap is relative and
  expresses the acceptable deviance from optimality. For example, a
  value of 0.01 will result in the solver stopping when it has found a
  solution within 1% of optimality. Additionally, a value of 0 will
  result in the solver stopping when it has found an optimal solution.
  The default value is 0.1 (i.e., 10% from optimality).

- time_limit:

  `numeric` value denoting the time limit (seconds) for generating
  solutions. The solver will return the current best solution when this
  time limit is exceeded. The default value is the largest integer value
  (i.e., `.Machine$integer.max`), effectively meaning that solver will
  keep running until a solution within the optimality gap is found.

- first_feasible:

  `logical` value indicating if the first feasible solution should be
  returned? If `first_feasible = TRUE`, then the solver will return the
  first solution it encounters that meets all the constraints,
  regardless of solution quality. Note that the first feasible solution
  is not an arbitrary solution, rather it is derived from the relaxed
  problem, and is therefore often reasonably close to optimality.
  Defaults to `FALSE`.

- verbose:

  `logical` value indicating if information should be displayed during
  the optimization process. Defaults to `TRUE`.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md) or
[`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
object with the solver added to it.

## Details

[*SYMPHONY*](https://github.com/coin-or/SYMPHONY) is an open-source
mixed integer programming solver that is part of the Computational
Infrastructure for Operations Research (COIN-OR) project. The Rsymphony
package provides an interface to COIN-OR and – unlike dependencies for
other solvers – is available on *CRAN*. For information on the
performance of different solvers, please see Schuster *et al.* (2020)
for benchmarks comparing the run time and solution quality of different
solvers when applied to different sized datasets.

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
[`add_lsymphony_solver`](https://prioritizr.net/reference/add_lsymphony_solver.md)

## Examples

``` r
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create problem
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_rsymphony_solver(time_limit = 10, verbose = FALSE)

# generate solution
s <- solve(p)

# plot solution
plot(s, main = "solution", axes = FALSE)
```
