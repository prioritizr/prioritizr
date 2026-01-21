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

  `logical` should the conservation problem compiled into a compressed
  version of a planning problem? If `TRUE` then the problem is expressed
  using the compressed formulation. If `FALSE` then the problem is
  expressed using the expanded formulation. If `NA`, then the compressed
  is used unless one of the constraints requires the expanded
  formulation. This argument defaults to `NA`.

## Value

A
[`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
object.

## Details

This function might be useful for those interested in understanding how
their conservation planning
[`problem()`](https://prioritizr.net/reference/problem.md) is expressed
as a mathematical problem. However, if the problem just needs to be
solved, then the [`solve()`](https://prioritizr.net/reference/solve.md)
function should just be used.

**Please note that in nearly all cases, the default argument to
`compressed_formulation` should be used**. The only situation where
manually setting the argument to `formulation` is desirable is during
testing. Manually setting the argument to `formulation` will at best
have no effect on the problem. At worst, it may result in an error, a
misspecified problem, or unnecessarily long solve times.

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
