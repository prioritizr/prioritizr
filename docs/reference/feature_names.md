# Feature names

Extract the names of the features in an object.

## Usage

``` r
feature_names(x, ...)

# S3 method for class 'ConservationProblem'
feature_names(x, ...)

# S3 method for class 'MultiObjConservationProblem'
feature_names(x, ...)

# S3 method for class 'ZonesRaster'
feature_names(x, ...)

# S3 method for class 'ZonesSpatRaster'
feature_names(x, ...)

# S3 method for class 'ZonesCharacter'
feature_names(x, ...)

# S3 method for class 'MultiObjConservationProblem'
problem_names(x, ...)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md),
  [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md),
  or [`Zones()`](https://prioritizr.net/reference/zones.md) object.

- ...:

  not used.

## Value

A `character` vector of feature names.

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

# print feature names
print(feature_names(p))
#> [1] "feature_1" "feature_2" "feature_3" "feature_4" "feature_5"

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
print(feature_names(mp))
#>        obj1        obj1        obj2        obj2        obj2 
#> "feature_1" "feature_2" "feature_3" "feature_4" "feature_5" 
```
