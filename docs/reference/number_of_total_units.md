# Number of total units

Extract the number of total units in an object.

## Usage

``` r
number_of_total_units(x, ...)

# S3 method for class 'ConservationProblem'
number_of_total_units(x, ...)

# S3 method for class 'MultiObjConservationProblem'
number_of_total_units(x, ...)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) or
  [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
  object.

- ...:

  not used.

## Value

An `integer` number of total units.

## Details

The total units for an object corresponds to the total number of entries
(e.g., rows, cells) for the planning unit data. For example, a
single-layer raster dataset might have 90 cells and only two of these
cells contain non-missing (`NA`) values. As such, this dataset would
have 90 total units and two planning units.

## Examples

``` r
# \dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_features <- get_sim_zones_features()

# create problem with one zone
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions()

# print number of planning units
print(number_of_planning_units(p1))
#> [1] 90

# print number of total units
print(number_of_total_units(p1))
#> [1] 100

# create problem with multiple zones
p2 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
  add_binary_decisions()

# print number of planning units
print(number_of_planning_units(p2))
#> [1] 90

# print number of total units
print(number_of_total_units(p2))
#> [1] 100

# define budget for multi-objective problem
b <- 0.3 * terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]]

# create multi-objective problem
 mp <-
  multi_problem(
   obj1 =
     problem(sim_pu_raster, sim_features[[1:2]]) %>%
     add_max_utility_objective(budget = b) %>%
     add_relative_targets(0.2) %>%
     add_binary_decisions(),
   obj2 =
     problem(sim_pu_raster, sim_features[[3:5]]) %>%
     add_min_shortfall_objective(budget = b) %>%
     add_relative_targets(0.8) %>%
     add_binary_decisions()
  )

# print number of total units
print(number_of_total_units(mp))
#> [1] 100
# }
```
