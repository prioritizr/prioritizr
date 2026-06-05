# Add default solver

Specify that the best solver currently available should be used to solve
a conservation planning problem.

## Usage

``` r
add_default_solver(x, ...)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) or
  [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
  object.

- ...:

  arguments passed to the solver.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md) or
[`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
object with the solver added to it.

## Details

Ranked from best to worst, the available solvers that can be used are:
[`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md),
[`add_cplex_solver()`](https://prioritizr.net/reference/add_cplex_solver.md),
[`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md),
[`add_highs_solver()`](https://prioritizr.net/reference/add_highs_solver.md),
[`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md),
and finally
[`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md).
For information on the performance of different solvers, please see
Schuster *et al.* (2020).

## References

Schuster R, Hanson JO, Strimas-Mackey M, and Bennett JR (2020). Exact
integer linear programming solvers outperform simulated annealing for
solving conservation planning problems. *PeerJ*, 8: e9258.

## See also

See [solvers](https://prioritizr.net/reference/solvers.md) for an
overview of all functions for adding a solver.

Other functions for adding solvers:
[`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md),
[`add_cplex_solver()`](https://prioritizr.net/reference/add_cplex_solver.md),
[`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md),
[`add_highs_solver()`](https://prioritizr.net/reference/add_highs_solver.md),
[`add_lsymphony_solver`](https://prioritizr.net/reference/add_lsymphony_solver.md),
[`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)

## Examples

``` r
# set seed for reproducibility
set.seed(600)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create minimal problem with default portfolio
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.05) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve problem
s <- solve(p)

# plot solution
plot(s)
```
