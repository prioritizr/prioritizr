# Number of features

Extract the number of features in an object.

## Usage

``` r
number_of_features(x, ...)

# S3 method for class 'ConservationProblem'
number_of_features(x, ...)

# S3 method for class 'OptimizationProblem'
number_of_features(x, ...)

# S3 method for class 'ZonesSpatRaster'
number_of_features(x, ...)

# S3 method for class 'ZonesRaster'
number_of_features(x, ...)

# S3 method for class 'ZonesCharacter'
number_of_features(x, ...)
```

## Arguments

- x:

  A [`problem()`](https://prioritizr.net/reference/problem.md),
  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md),
  or [`zones()`](https://prioritizr.net/reference/zones.md) object.

- ...:

  not used.

## Value

An `integer` number of features.

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
# }
```
