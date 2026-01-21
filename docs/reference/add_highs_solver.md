# Add a *HiGHS* solver

Specify that the [*HiGHS*](https://highs.dev/) software should be used
to solve a conservation planning problem (Huangfu and Hall 2018). This
function can also be used to customize the behavior of the solver. It
requires the highs package to be installed.

## Usage

``` r
add_highs_solver(
  x,
  gap = 0.1,
  time_limit = .Machine$integer.max,
  presolve = TRUE,
  threads = 1,
  verbose = TRUE,
  control = list()
)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

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

- presolve:

  `logical` attempt to simplify the problem before solving it? Defaults
  to `TRUE`.

- threads:

  `integer` number of threads to use for the optimization algorithm. The
  default value is 1.

- verbose:

  `logical` should information be printed while solving optimization
  problems? Defaults to `TRUE`.

- control:

  `list` with additional parameters for tuning the optimization process.
  For example, `control = list(simplex_strategy = 1)` could be used to
  set the `simplex_strategy` parameter. See the [online
  documentation](https://ergo-code.github.io/HiGHS/dev/options/definitions/)
  for information on the parameters.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the solver added to it.

## Details

[*HiGHS*](https://highs.dev/) is an open source optimization software.
Although this solver can have comparable performance to the *CBC* solver
(i.e.,
[`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md))
for particular problems and is generally faster than the *SYMPHONY*
based solvers (i.e.,
[`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md),
[`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)),
it can sometimes take much longer than the *CBC* solver for particular
problems. This solver is recommended if the
[`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md),
[`add_cplex_solver()`](https://prioritizr.net/reference/add_cplex_solver.md),
[`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md)
cannot be used.

## References

Huangfu Q and Hall JAJ (2018). Parallelizing the dual revised simplex
method. *Mathematical Programming Computation*, 10: 119-142.

## See also

Other functions for adding solvers:
[`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md),
[`add_cplex_solver()`](https://prioritizr.net/reference/add_cplex_solver.md),
[`add_default_solver()`](https://prioritizr.net/reference/add_default_solver.md),
[`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md),
[`add_lsymphony_solver`](https://prioritizr.net/reference/add_lsymphony_solver.md),
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
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_highs_solver(gap = 0, verbose = FALSE)

# generate solution
s <- solve(p)

# plot solution
plot(s, main = "solution", axes = FALSE)

# }
```
