# Add a *SCIP* solver

Specify that the [*SCIP*](https://www.scipopt.org/) (Solving Constraint
Integer Programs) software should be used to solve a conservation
planning problem (Achterberg *et al.* 2008, Achterberg 2009). This
function can also be used to customize the behavior of the solver. It
requires the scip package to be installed.

## Usage

``` r
add_scip_solver(
  x,
  gap = 0.1,
  time_limit = .Machine$integer.max,
  presolve = TRUE,
  threads = 1,
  first_feasible = FALSE,
  verbose = TRUE,
  control = list()
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

- presolve:

  `integer` value indicating how intensively the solver should try to
  simplify the problem before solving it. Available options are: (-1)
  automatically determine the intensity of pre-solving, (0) disable
  pre-solving, (1) conservative level of pre-solving, and (2) very
  aggressive level of pre-solving . Defaults to 2.

- threads:

  `integer` value denoting the number of threads to use during
  optimization. Broadly speaking, we recommend setting `threads` to be
  no higher than the number of computational cores minus one or two
  (e.g., `threads = parallel::detectCores(TRUE) - 2`). This is because
  setting `threads` to be equal to the number of computational cores
  means that the solver and is fighting for resources with other
  software (e.g., Dropbox, iCloud, OneDrive, software updates, antivirus
  software, internet browsers) and, in turn, can result in computational
  bottlenecks that slow run times. Additionally, when setting `threads`
  to be a value greater than 1, we recommend checking memory (RAM) usage
  during the optimization process to ensure that the solver does not use
  up the majority of available memory. This is because solving
  optimization problems with multiple threads can involve creating
  multiple copies of the problem (e.g., `threads = 5` may mean 5 copies)
  and exhausting most of the available memory will drastically slow run
  times. Defaults to 1.

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

- control:

  `list` with additional parameters for tuning the optimization process.
  For example, `control = list(mem_limit = 200)` could be used to set
  the `mem_limit` parameter. See
  [`scip::scip_control()`](https://bnaras.github.io/scip/reference/scip_control.html)
  for information on the parameters.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md) or
[`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
object with the solver added to it.

## Details

[*SCIP*](https://www.scipopt.org/) is an open source optimization
software. It is not recommended to use this solver because it tends to
have the slowest performance.

## References

Achterberg T, Berthold T, Koch T, and Wolter K (2008) Integration of AI
and OR techniques in constraint programming for combinatorial
optimization problems, CPAIOR 2008, LNCS 5015, pp. 6–20.

Achterberg T (2009) SCIP: solving constraint integer programs
*Mathematical programming computation*, 1: 1–41.

## See also

Other functions for adding solvers:
[`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md),
[`add_cplex_solver()`](https://prioritizr.net/reference/add_cplex_solver.md),
[`add_default_solver()`](https://prioritizr.net/reference/add_default_solver.md),
[`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md),
[`add_highs_solver()`](https://prioritizr.net/reference/add_highs_solver.md),
[`add_lsymphony_solver`](https://prioritizr.net/reference/add_lsymphony_solver.md),
[`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)

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
  add_scip_solver(gap = 0, verbose = FALSE)

# generate solution
s <- solve(p)

# plot solution
plot(s, main = "solution", axes = FALSE)
```
