# Evaluate objective value of solution

Calculate the objective value of a solution to a conservation planning
problem.

## Usage

``` r
eval_objective_summary(x, solution, include_penalties = TRUE)

# S3 method for class 'ConservationProblem'
eval_objective_summary(x, solution, include_penalties = TRUE)

# S3 method for class 'MultiConservationProblem'
eval_objective_summary(x, solution, include_penalties = TRUE)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) or
  [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
  object.

- solution:

  `numeric`, `matrix`, `data.frame`,
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
  or [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  object. Note that `solution` must have the same format as the planning
  unit data in `x`. See the Solution format section for more
  information.

- include_penalties:

  `logical` should penalties be included when calculating objectives
  values? Defaults to `TRUE`.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object describing the performance of the solution. It contains the
following columns.

- problem:

  `character` name of problem. Note that this column is only present if
  `x` is a
  [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
  object.

- value:

  `numeric` objective value.

## Details

The mathematical objective function of an optimization problem describes
the performance metric that is minimized or maximized during
optimization. In a conservation planning
[`problem()`](https://prioritizr.net/reference/problem.md),
[objectives](https://prioritizr.net/reference/objectives.md) specify the
primary metric should be maximized or minimized (e.g.,
[`add_min_set_objective()`](https://prioritizr.net/reference/add_min_set_objective.md)
specify that costs should be minimized) and
[penalties](https://prioritizr.net/reference/penalties.md) can
(optionally) be used to specify additional metrics that should be
maximized or minimized during optimization (e.g.,
[`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md)
specify that spatial fragmentation should be minimized). Given this, the
mathematical objective function of a conservation planning
[`problem()`](https://prioritizr.net/reference/problem.md) is calculated
based on a weighted sum of the
[objectives](https://prioritizr.net/reference/objectives.md) and
[penalties](https://prioritizr.net/reference/penalties.md) (i.e., where
the weights are the `penalty` values specified in the
[penalties](https://prioritizr.net/reference/penalties.md) function).

## See also

Other functions for summarizing solutions:
[`eval_asym_connectivity_summary()`](https://prioritizr.net/reference/eval_asym_connectivity_summary.md),
[`eval_boundary_summary()`](https://prioritizr.net/reference/eval_boundary_summary.md),
[`eval_connectivity_summary()`](https://prioritizr.net/reference/eval_connectivity_summary.md),
[`eval_cost_summary()`](https://prioritizr.net/reference/eval_cost_summary.md),
[`eval_feature_representation_summary()`](https://prioritizr.net/reference/eval_feature_representation_summary.md),
[`eval_n_summary()`](https://prioritizr.net/reference/eval_n_summary.md),
[`eval_target_coverage_summary()`](https://prioritizr.net/reference/eval_target_coverage_summary.md)

## Examples

``` r
# set seed for reproducibility
set.seed(500)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# build conservation problem with boundary penalties
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve the problem
s1 <- solve(p1)

# print solution
print(s1)
#> class       : SpatRaster
#> size        : 10, 10, 1  (nrow, ncol, nlyr)
#> resolution  : 0.1, 0.1  (x, y)
#> extent      : 0, 1, 0, 1  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857)
#> source(s)   : memory
#> varname     : sim_pu_raster
#> name        : layer
#> min value   :     0
#> max value   :     1

# calculate objective value including penalties
v1 <- eval_objective_summary(p1, s1, include_penalties = TRUE)
print(v1)
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1 1987.

# calculate objective value excluding penalties
v2 <- eval_objective_summary(p1, s1, include_penalties = FALSE)
print(v2)
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1 1987.
```
