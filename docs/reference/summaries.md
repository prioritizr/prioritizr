# Evaluate solutions using summary statistics

After generating a solution to a conservation planning problem, it can
be useful to evaluate how well it performs. These functions can be used
to evaluate a solution according to various different summary
statistics.

## Details

The following functions can be used to summarize the performance of a
solution to a conservation planning
[`problem()`](https://prioritizr.net/reference/problem.md).

- [`eval_n_summary()`](https://prioritizr.net/reference/eval_n_summary.md):

  Calculate the number of planning units selected within a solution.

- [`eval_cost_summary()`](https://prioritizr.net/reference/eval_cost_summary.md):

  Calculate the total cost of a solution.

- [`eval_feature_representation_summary()`](https://prioritizr.net/reference/eval_feature_representation_summary.md):

  Calculate how well features are represented by a solution. This
  function can be used for all problems.

- [`eval_target_coverage_summary()`](https://prioritizr.net/reference/eval_target_coverage_summary.md):

  Calculate how well feature representation
  [targets](https://prioritizr.net/reference/targets.md) are met by a
  solution. This function can only be used with problems that contain
  [targets](https://prioritizr.net/reference/targets.md).

- [`eval_boundary_summary()`](https://prioritizr.net/reference/eval_boundary_summary.md):

  Calculate the exposed boundary length (perimeter) associated with a
  solution.

- [`eval_connectivity_summary()`](https://prioritizr.net/reference/eval_connectivity_summary.md):

  Calculate the connectivity held within a solution using symmetric
  data.

- [`eval_asym_connectivity_summary()`](https://prioritizr.net/reference/eval_asym_connectivity_summary.md):

  Calculate the connectivity held within a solution using asymmetric
  data.

## See also

Other overviews:
[`constraints`](https://prioritizr.net/reference/constraints.md),
[`decisions`](https://prioritizr.net/reference/decisions.md),
[`importance`](https://prioritizr.net/reference/importance.md),
[`objectives`](https://prioritizr.net/reference/objectives.md),
[`penalties`](https://prioritizr.net/reference/penalties.md),
[`portfolios`](https://prioritizr.net/reference/portfolios.md),
[`solvers`](https://prioritizr.net/reference/solvers.md),
[`targets`](https://prioritizr.net/reference/targets.md)

## Examples

``` r
# \dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create a minimal problem
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s <- solve(p)

# evaluate number of selected planning units in solution
eval_n_summary(p, s)
#> # A tibble: 1 × 2
#>   summary     n
#>   <chr>   <dbl>
#> 1 overall    10

# evaluate solution cost
eval_cost_summary(p, s)
#> # A tibble: 1 × 2
#>   summary  cost
#>   <chr>   <dbl>
#> 1 overall 1987.

# evaluate feature representation by solution
eval_feature_representation_summary(p, s)
#> # A tibble: 5 × 5
#>   summary feature   total_amount absolute_held relative_held
#>   <chr>   <chr>            <dbl>         <dbl>         <dbl>
#> 1 overall feature_1         83.3          8.91         0.107
#> 2 overall feature_2         31.2          3.13         0.100
#> 3 overall feature_3         72.0          7.34         0.102
#> 4 overall feature_4         42.7          4.35         0.102
#> 5 overall feature_5         56.7          6.01         0.106

# evaluate target coverage by solution
eval_target_coverage_summary(p, s)
#> # A tibble: 5 × 9
#>   feature   met   total_amount absolute_target absolute_held absolute_shortfall
#>   <chr>     <lgl>        <dbl>           <dbl>         <dbl>              <dbl>
#> 1 feature_1 TRUE          83.3            8.33          8.91                  0
#> 2 feature_2 TRUE          31.2            3.12          3.13                  0
#> 3 feature_3 TRUE          72.0            7.20          7.34                  0
#> 4 feature_4 TRUE          42.7            4.27          4.35                  0
#> 5 feature_5 TRUE          56.7            5.67          6.01                  0
#> # ℹ 3 more variables: relative_target <dbl>, relative_held <dbl>,
#> #   relative_shortfall <dbl>

# evaluate exposed boundary (perimeter) length by solution
eval_boundary_summary(p, s)
#> # A tibble: 1 × 2
#>   summary boundary
#>   <chr>      <dbl>
#> 1 overall     2.25

# create a symmetric connectivity matrix to describe pair-wise connectivity
# values between combinations of planning units,
# see ?connectivity_matrix for more information

# for brevity, we will do this using the cost data
# cost valuers have high connectivity between them
cm <- connectivity_matrix(sim_pu_raster, sim_pu_raster)

# evaluate connectivity of solution using symmetric data
eval_connectivity_summary(p, s, data = cm)
#> # A tibble: 1 × 2
#>   summary connectivity
#>   <chr>          <dbl>
#> 1 overall         198.

# create an asymmetric connectivity matrix to describe pair-wise
# connectivity values between combinations of planning units

# for brevity, we will just generate a matrix with random values
acm <- matrix(
  runif(ncell(sim_pu_raster) ^ 2),
  ncol = terra::ncell(sim_pu_raster)
)

# evaluate connectivity of solution using asymmetric data
eval_asym_connectivity_summary(p, s, data = acm)
#> # A tibble: 1 × 2
#>   summary asym_connectivity
#>   <chr>               <dbl>
#> 1 overall              50.5

# }
```
