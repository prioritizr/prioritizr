# Add a shuffle portfolio

Generate a portfolio of solutions for a conservation planning problem by
randomly reordering the data prior to solving the problem. Although this
function can be useful for generating multiple different solutions for a
given problem, it is recommended to use
[add_pool_portfolio](https://prioritizr.net/reference/prioritizr-deprecated.md)
if the *Gurobi* software is available.

## Usage

``` r
add_shuffle_portfolio(x, number_solutions = 10, threads = 1)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- number_solutions:

  `integer` value denoting the number of required solutions. Defaults to
  10.

- threads:

  `integer` value denoting the number of threads to use for generating
  the solution portfolio. Defaults to 1.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the portfolio added to it.

## Details

This strategy for generating a portfolio of solutions often results in
different solutions, depending on optimality gap, but may return
duplicate solutions. In general, this strategy is most effective when
problems are quick to solve and multiple threads are available for
solving each problem separately.

## Notes

In previous versions (\< 9.0.0.0), this function had a
`remove_duplicates` parameter. To streamline and provide this
functionality for other functions, duplicate solutions can now be
removed by using the the `remove_duplicates` parameter of
[`solve()`](https://prioritizr.net/reference/solve.md).

## See also

Other functions for adding portfolios:
[`add_cuts_portfolio()`](https://prioritizr.net/reference/add_cuts_portfolio.md),
[`add_default_portfolio()`](https://prioritizr.net/reference/add_default_portfolio.md),
[`add_extra_portfolio()`](https://prioritizr.net/reference/add_extra_portfolio.md),
[`add_gap_portfolio()`](https://prioritizr.net/reference/add_gap_portfolio.md),
[`add_single_portfolio()`](https://prioritizr.net/reference/add_single_portfolio.md),
[`add_top_portfolio()`](https://prioritizr.net/reference/add_top_portfolio.md)

## Examples

``` r
# set seed for reproducibility
set.seed(500)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_features <- get_sim_zones_features()

# create minimal problem with shuffle portfolio
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_shuffle_portfolio(10) %>%
  add_default_solver(gap = 0.2, verbose = FALSE)

# solve problem and generate 10 solutions within 20% of optimality
s1 <- solve(p1)

# convert portfolio into a multi-layer raster
s1 <- terra::rast(s1)

# print number of solutions found
print(terra::nlyr(s1))
#> [1] 10

# plot solutions in portfolio
plot(s1, axes = FALSE)


# build multi-zone conservation problem with shuffle portfolio
p2 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
  add_binary_decisions() %>%
  add_shuffle_portfolio(10) %>%
  add_default_solver(gap = 0.2, verbose = FALSE)

# solve the problem
s2 <- solve(p2)

# convert each solution in the portfolio into a single category layer
s2 <- terra::rast(lapply(s2, category_layer))

# print number of solutions found
print(terra::nlyr(s2))
#> [1] 10

# plot solutions in portfolio
plot(s2, axes = FALSE)
```
