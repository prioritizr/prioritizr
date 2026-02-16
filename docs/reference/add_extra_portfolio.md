# Add an extra portfolio

Generate a portfolio of solutions for a conservation planning problem by
storing feasible solutions discovered during the optimization process.
This method is useful for quickly obtaining multiple solutions, but does
not provide any guarantees on the number of solutions, or the quality of
solutions.

## Usage

``` r
add_extra_portfolio(x)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the portfolio added to it.

## Details

This strategy for generating a portfolio requires problems to be solved
using the *Gurobi* software suite (i.e., using
[`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md).
Specifically, version 8.0.0 (or greater) of the gurobi package must be
installed.

## See also

See [portfolios](https://prioritizr.net/reference/portfolios.md) for an
overview of all functions for adding a portfolio.

Other functions for adding portfolios:
[`add_cuts_portfolio()`](https://prioritizr.net/reference/add_cuts_portfolio.md),
[`add_default_portfolio()`](https://prioritizr.net/reference/add_default_portfolio.md),
[`add_gap_portfolio()`](https://prioritizr.net/reference/add_gap_portfolio.md),
[`add_shuffle_portfolio()`](https://prioritizr.net/reference/add_shuffle_portfolio.md),
[`add_top_portfolio()`](https://prioritizr.net/reference/add_top_portfolio.md)

## Examples

``` r
# \dontrun{
# set seed for reproducibility
set.seed(600)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_features <- get_sim_zones_features()

# create minimal problem with a portfolio for extra solutions
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.05) %>%
  add_extra_portfolio() %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve problem and generate portfolio
s1 <- solve(p1)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0.001 0.014

# convert portfolio into a multi-layer raster object
s1 <- terra::rast(s1)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'rast': object 's1' not found

# print number of solutions found
print(terra::nlyr(s1))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'nlyr': object 's1' not found

# plot solutions
plot(s1, axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's1' not found

# create multi-zone problem with a portfolio for extra solutions
p2 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
  add_extra_portfolio() %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve problem and generate portfolio
s2 <- solve(p2)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0 0.012

# convert each solution in the portfolio into a single category layer
s2 <- terra::rast(lapply(s2, category_layer))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'rast': object 's2' not found

# print number of solutions found
print(terra::nlyr(s2))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'nlyr': object 's2' not found

# plot solutions in portfolio
plot(s2, axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's2' not found
# }
```
