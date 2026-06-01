# Add a default portfolio

Generate a portfolio based on defaults.

## Usage

``` r
add_default_portfolio(x)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the portfolio added to it.

## Details

By default, this is portfolio is added to
[`problem()`](https://prioritizr.net/reference/problem.md) objects if no
other portfolios is manually specified. In particular, this function
adds the
[`add_single_portfolio()`](https://prioritizr.net/reference/add_single_portfolio.md)
function to `x` so that only a single solution is generated.

## See also

Other functions for adding portfolios:
[`add_cuts_portfolio()`](https://prioritizr.net/reference/add_cuts_portfolio.md),
[`add_extra_portfolio()`](https://prioritizr.net/reference/add_extra_portfolio.md),
[`add_gap_portfolio()`](https://prioritizr.net/reference/add_gap_portfolio.md),
[`add_shuffle_portfolio()`](https://prioritizr.net/reference/add_shuffle_portfolio.md),
[`add_single_portfolio()`](https://prioritizr.net/reference/add_single_portfolio.md),
[`add_top_portfolio()`](https://prioritizr.net/reference/add_top_portfolio.md)

## Examples

``` r
# set seed for reproducibility
set.seed(600)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create minimal problem with default portfolio
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.05) %>%
  add_default_portfolio() %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve problem
s <- solve(p)

# plot solution
plot(s)
```
