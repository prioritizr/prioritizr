# Number of features

Extract the number of features in an object.

## Usage

``` r
number_of_features(x, ...)

# S3 method for class 'ConservationProblem'
number_of_features(x, ...)

# S3 method for class 'MultiObjConservationProblem'
number_of_features(x, ...)

# S3 method for class 'OptimizationProblem'
number_of_features(x, ...)

# S3 method for class 'ZonesSpatRaster'
number_of_features(x, ...)

# S3 method for class 'ZonesRaster'
number_of_features(x, ...)

# S3 method for class 'ZonesCharacter'
number_of_features(x, ...)

# S3 method for class 'MultiObjConservationProblem'
number_of_problems(x, ...)
```

## Arguments

- x:

  A [`problem()`](https://prioritizr.net/reference/problem.md),
  [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md),
  or [`zones()`](https://prioritizr.net/reference/zones.md) object.

- ...:

  not used.

## Value

An `integer` value.

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
  add_relative_targets(0.2) %>%
  add_binary_decisions()

# print number of features
print(number_of_features(p))
#> [1] 5

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

# print number of features
print(number_of_features(mp))
#> [1] 5
# }
```
