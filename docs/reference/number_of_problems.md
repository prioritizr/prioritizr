# Number of problems

Extract the number of conservation problems in an object.

## Usage

``` r
number_of_problems(x, ...)

# S3 method for class 'ConservationProblem'
number_of_problems(x, ...)
```

## Arguments

- x:

  A [`problem()`](https://prioritizr.net/reference/problem.md),or
  [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
  object.

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

# print number of problems
print(number_of_problems(mp))
#> [1] 2
# }
```
