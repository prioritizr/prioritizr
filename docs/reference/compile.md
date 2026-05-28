# Compile a problem

Compile a conservation planning problem into an mixed integer linear
programming problem.

## Usage

``` r
compile(x, ...)

# S3 method for class 'ConservationProblem'
compile(x, compressed_formulation = NA, ...)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- ...:

  not used.

- compressed_formulation:

  `logical` value indicating if `x` should be compiled following the
  compressed formulation? If `compressed_formulation = NA`, then the
  compressed is used unless one of the constraints in `x` requires the
  expanded formulation. Defaults to `NA`.

## Value

An
[`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
object.

## Details

This function might be useful for those interested in understanding how
their conservation planning
[`problem()`](https://prioritizr.net/reference/problem.md) is expressed
as a mathematical optimization problem. However, if `x` just needs to be
solved, then the [`solve()`](https://prioritizr.net/reference/solve.md)
function should be used directly.

**Please note that in nearly all cases, the default value for
`compressed_formulation` should be used**. This is because manually
setting the `compressed_formulation` will, at best, have no effect on
the problem. At worst, it may result in an error, a mis-specified
problem, or unnecessarily long solve times.

## Examples

``` r
# \dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# build minimal conservation problem
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1)

# compile the conservation problem into an optimization problem
o <- compile(p)

# print the optimization problem
print(o)
#> An optimization problem (<OptimizationProblem>)
#> • model sense: min
#> • dimensions:  5, 90, 450 (rows, columns, cells)
#> • variables:   90 (B)
# }
```
