# Add a *CBC* solver

Specify that the [*CBC*](https://github.com/coin-or/Cbc) (COIN-OR branch
and cut) software should be used to solve a conservation planning
problem (Forrest & Lougee-Heimer 2005). This function can also be used
to customize the behavior of the solver. It requires the rcbc package to
be installed (only [available on
GitHub](https://github.com/dirkschumacher/rcbc), see below for
installation instructions).

## Usage

``` r
add_cbc_solver(
  x,
  gap = 0.1,
  time_limit = .Machine$integer.max,
  presolve = 2,
  threads = 1,
  first_feasible = FALSE,
  start_solution = NULL,
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
  simplify the problem before solving it. Available options include (0)
  disable pre-solving, (1) conservative level of pre-solving, and (2)
  very aggressive level of pre-solving . Defaults to 2.

- threads:

  `integer` value denoting the number of threads to use during
  optimization. The default value is 1.

- first_feasible:

  `logical` value indicating if the first feasible solution should be
  returned? If `first_feasible = TRUE`, then the solver will return the
  first solution it encounters that meets all the constraints,
  regardless of solution quality. Note that the first feasible solution
  is not an arbitrary solution, rather it is derived from the relaxed
  problem, and is therefore often reasonably close to optimality.
  Defaults to `FALSE`.

- start_solution:

  `NULL` or object containing the starting solution for the solver. This
  is can be useful because specifying a starting solution can speed up
  the optimization process. To specify a starting solution,
  `start_solution` should be in the same format as the planning units
  (i.e., a `numeric`, `matrix`, `data.frame`,
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
  or [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  object). See the Start solution format section for more information.
  Defaults to `NULL` such that no starting solution is used.

- verbose:

  `logical` value indicating if information should be displayed during
  the optimization process. Defaults to `TRUE`.

- control:

  `list` with additional parameters for tuning the optimization process.
  For example, `control = list(strategy = 2)` could be used to set the
  `strategy` parameter. See the [online
  documentation](https://www.gams.com/latest/docs/S_CBC.html#CBC_OPTIONS_LIST)
  for information on the parameters.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md) or
[`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
object with the solver added to it.

## Details

[*CBC*](https://github.com/coin-or/Cbc) is an open-source mixed integer
programming solver that is part of the Computational Infrastructure for
Operations Research (COIN-OR) project. This solver seems to have much
better performance than the other open-source solvers (i.e.,
[`add_highs_solver()`](https://prioritizr.net/reference/add_highs_solver.md),
[`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md),
[`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md))
(see the *Solver benchmarks* vignette for details). As such, it is
strongly recommended to use this solver if the *Gurobi* and *IBM CPLEX*
solvers are not available.

## Installation

The rcbc package is required to use this solver. Since the rcbc package
is not available on the the Comprehensive R Archive Network (CRAN), it
must be installed from [its GitHub
repository](https://github.com/dirkschumacher/rcbc). To install the rcbc
package, please use the following code:

    if (!require(remotes)) install.packages("remotes")
    remotes::install_github("dirkschumacher/rcbc")

Note that you may also need to install several dependencies – such as
the [Rtools software](https://cran.r-project.org/bin/windows/Rtools/) or
system libraries – prior to installing the rcbc package. For further
details on installing this package, please consult the [online package
documentation](https://dirkschumacher.github.io/rcbc/).

## Start solution format

Broadly speaking, `start_solution` must be in the same format as the
planning unit data in `x`. Further details on the correct format are
described below.

- `x` has `numeric` planning units:

  Here `start_solution` must be a `numeric` vector with each element
  corresponding to a different planning unit. It should have the same
  number of planning units as those in `x`. Additionally, any planning
  units with missing cost (`NA`) values should also have missing (`NA`)
  values in the `start_solution`.

- `x` has `matrix` planning units:

  Here `start_solution` must be a `matrix` vector with each row
  corresponding to a different planning unit, and each column correspond
  to a different management zone. It should have the same number of
  planning units and zones as those in `x`. Additionally, any planning
  units with missing cost (`NA`) values for a particular zone should
  also have a missing (`NA`) values in `start_solution`.

- `x` has
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  planning units:

  Here `start_solution` be a
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object where different cells correspond to different planning units
  and layers correspond to a different management zones. It should have
  the same dimensionality (rows, columns, layers), resolution, extent,
  and coordinate reference system as the planning units in `x`.
  Additionally, any planning units with missing cost (`NA`) values for a
  particular zone should also have missing (`NA`) values in
  `start_solution`.

- `x` has `data.frame` planning units:

  Here `start_solution` must be a `data.frame` with each column
  corresponding to a different zone, each row corresponding to a
  different planning unit, and cell values corresponding to the solution
  value. This means that if a `data.frame` object containing the
  solution also contains additional columns, then these columns will
  need to be subsetted prior to using this function (see below for
  example with
  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) data).
  Additionally, any planning units with missing cost (`NA`) values for a
  particular zone should also have missing (`NA`) values in
  `start_solution`.

- `x` has [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  planning units:

  Here `start_solution` must be a
  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
  with each column corresponding to a different zone, each row
  corresponding to a different planning unit, and cell values
  corresponding to the solution value. This means that if the
  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
  containing the solution also contains additional columns, then these
  columns will need to be subsetted prior to using this function (see
  below for example). Additionally, `start_solution` must also have the
  same coordinate reference system as the planning unit data.
  Furthermore, any planning units with missing cost (`NA`) values for a
  particular zone should also have missing (`NA`) values in
  `start_solution`.

## References

Forrest J and Lougee-Heimer R (2005) CBC User Guide. In Emerging theory,
Methods, and Applications (pp. 257–277). INFORMS, Catonsville, MD.
[doi:10.1287/educ.1053.0020](https://doi.org/10.1287/educ.1053.0020) .

## See also

Other functions for adding solvers:
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
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0, verbose = FALSE)

# generate solution %>%
s1 <- solve(p1)

# plot solution
plot(s1, main = "solution", axes = FALSE)


# create a similar problem with boundary length penalties and
# specify the solution from the previous run as a starting solution
p2 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(10) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0, start_solution = s1, verbose = FALSE)

# generate solution
s2 <- solve(p2)

# plot solution
plot(s2, main = "solution with boundary penalties", axes = FALSE)
```
