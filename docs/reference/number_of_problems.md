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

An `integer` number of problems.

## Examples

``` r
# \dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create multi-objective problem
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
#> Error in add_max_utility_objective(., budget = b): ℹ In argument to `budget`.
#> Caused by error:
#> ! object 'b' not found

# print number of problems
print(number_of_problems(mp))
#> Error in number_of_problems(mp): ℹ In argument to `x`.
#> Caused by error:
#> ! object 'mp' not found
# }
```
