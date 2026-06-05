# Number of planning units

Extract the number of planning units in an object.

## Usage

``` r
number_of_planning_units(x, ...)

# S3 method for class 'ConservationProblem'
number_of_planning_units(x, ...)

# S3 method for class 'MultiConservationProblem'
number_of_planning_units(x, ...)

# S3 method for class 'OptimizationProblem'
number_of_planning_units(x, ...)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md),
  [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
  or
  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  object.

- ...:

  not used.

## Value

An `integer` value.

## Details

The planning units for an object corresponds to the number of entries
(e.g., rows, cells) for the planning unit data that do not have missing
(`NA`) values for every zone. For example, a single-layer raster dataset
might have 90 cells and only two of these cells contain non-missing
(`NA`) values. As such, this dataset would have two planning units.

## Examples

``` r
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create problem
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions()

# print number of planning units
print(number_of_planning_units(p))
#> [1] 90

# define budget for multi-objective problem
b <- 0.3 * terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]]

# create multi-objective problem
mp <-
  multi_problem(
    obj1 =
      problem(sim_pu_raster, sim_features[[1:2]]) %>%
      add_max_wtd_sum_objective(budget = b) %>%
      add_relative_targets(0.2) %>%
      add_binary_decisions(),
    obj2 =
      problem(sim_pu_raster, sim_features[[3:5]]) %>%
      add_min_shortfall_objective(budget = b) %>%
      add_relative_targets(0.8) %>%
      add_binary_decisions()
  )
#> ℹ `add_max_wtd_sum_objective()` has severe limitations - use with caution.

# print number of planning units
print(number_of_planning_units(mp))
#> [1] 90
```
