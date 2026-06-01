# Compile a multi-objective optimization problem

Compile multiple
[`OptimizationProblem`](https://prioritizr.net/reference/OptimizationProblem-class.md)
objects for multi-objective optimization.

## Usage

``` r
multi_compile(x, ...)

# S3 method for class 'MultiObjConservationProblem'
multi_compile(x, ...)

# S3 method for class 'list'
multi_compile(x, ...)
```

## Arguments

- x:

  [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
  or `list` of
  [`OptimizationProblem`](https://prioritizr.net/reference/OptimizationProblem-class.md)
  objects.

- ...:

  arguments passed to
  [`compile()`](https://prioritizr.net/reference/compile.md).

## Value

A `list` containing a (`$obj`) `numeric` matrix with the coefficients
for each of the objectives (i.e., rows correspond to different
objectives and columns correspond to different decision variables),
(`$modelsense`) `character` vector indicating if each objective should
be maximized or minimized (i.e., each element corresponds to a different
objective), and a (`$opt`)
[`OptimizationProblem`](https://prioritizr.net/reference/OptimizationProblem-class.md)
object with all of the constraints present in `x` (note that the
objective coefficients in the returned object are all zero).

## See also

See [`compile()`](https://prioritizr.net/reference/compile.md) to create
an
[`OptimizationProblem`](https://prioritizr.net/reference/OptimizationProblem-class.md)
object.

## Examples

``` r
# import data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# define a total conservation budget (30% of total cost)
budget <- terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.3

# create multi-objective conservation planning problem
mp <-
  multi_problem(
    keystone_obj =
      problem(sim_pu_raster, sim_features[[1:3]]) %>%
      add_min_shortfall_objective(budget) %>%
      add_relative_targets(0.4) %>%
      add_binary_decisions(),
    iconic_obj =
      problem(sim_pu_raster, sim_features[[4:5]]) %>%
      add_min_shortfall_objective(budget) %>%
      add_relative_targets(0.45) %>%
      add_binary_decisions()
  )

# compile into multi-objective optimization problem
mo <- multi_compile(mp)

# print multi-objective optimization problem
print(mo)
#> $obj
#>              [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#> keystone_obj    0    0    0    0    0    0    0    0    0     0     0     0
#> iconic_obj      0    0    0    0    0    0    0    0    0     0     0     0
#>              [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23]
#> keystone_obj     0     0     0     0     0     0     0     0     0     0     0
#> iconic_obj       0     0     0     0     0     0     0     0     0     0     0
#>              [,24] [,25] [,26] [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34]
#> keystone_obj     0     0     0     0     0     0     0     0     0     0     0
#> iconic_obj       0     0     0     0     0     0     0     0     0     0     0
#>              [,35] [,36] [,37] [,38] [,39] [,40] [,41] [,42] [,43] [,44] [,45]
#> keystone_obj     0     0     0     0     0     0     0     0     0     0     0
#> iconic_obj       0     0     0     0     0     0     0     0     0     0     0
#>              [,46] [,47] [,48] [,49] [,50] [,51] [,52] [,53] [,54] [,55] [,56]
#> keystone_obj     0     0     0     0     0     0     0     0     0     0     0
#> iconic_obj       0     0     0     0     0     0     0     0     0     0     0
#>              [,57] [,58] [,59] [,60] [,61] [,62] [,63] [,64] [,65] [,66] [,67]
#> keystone_obj     0     0     0     0     0     0     0     0     0     0     0
#> iconic_obj       0     0     0     0     0     0     0     0     0     0     0
#>              [,68] [,69] [,70] [,71] [,72] [,73] [,74] [,75] [,76] [,77] [,78]
#> keystone_obj     0     0     0     0     0     0     0     0     0     0     0
#> iconic_obj       0     0     0     0     0     0     0     0     0     0     0
#>              [,79] [,80] [,81] [,82] [,83] [,84] [,85] [,86] [,87] [,88] [,89]
#> keystone_obj     0     0     0     0     0     0     0     0     0     0     0
#> iconic_obj       0     0     0     0     0     0     0     0     0     0     0
#>              [,90] [,91] [,92] [,93] [,94] [,95]
#> keystone_obj     0     1     1     1     0     0
#> iconic_obj       0     0     0     0     1     1
#> 
#> $modelsense
#> [1] "min" "min"
#> 
#> $opt
#> An optimization problem (<OptimizationProblem>)
#> • model sense: min
#> • dimensions:  7, 95, 635 (rows, columns, cells)
#> • variables:   90 (B), 5 (C)
#> 
```
