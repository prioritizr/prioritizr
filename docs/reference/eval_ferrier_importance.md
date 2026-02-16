# Evaluate solution importance using Ferrier scores

Calculate importance scores for planning units selected in a solution
following Ferrier *et al.* (2000).

## Usage

``` r
eval_ferrier_importance(x, solution)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- solution:

  `numeric`, `matrix`, `data.frame`,
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
  or [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  object. The argument should be in the same format as the planning unit
  cost data in the argument to `x`. See the Solution format section for
  more information.

## Value

A `matrix`,
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html),
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
or [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
object containing the scores for each planning unit selected in the
solution. Specifically, the returned object is in the same format
(except if the planning units are a `numeric` vector) as the planning
unit data in the argument to `x`.

## Details

Importance scores are reported separately for each feature within each
planning unit. Additionally, a total importance score is also calculated
as the sum of the scores for each feature. Note that this function only
works for problems that use targets and a single zone. It will throw an
error for problems that do not meet these criteria.

## Notes

In previous versions, the documentation for this function had a warning
indicating that the mathematical formulation for this function required
verification. The mathematical formulation for this function has since
been corrected and verified, so now this function is recommended for
general use.

## Solution format

Broadly speaking, the argument to `solution` must be in the same format
as the planning unit data in the argument to `x`. Further details on the
correct format are listed separately for each of the different planning
unit data formats:

- `x` has `numeric` planning units:

  The argument to `solution` must be a `numeric` vector with each
  element corresponding to a different planning unit. It should have the
  same number of planning units as those in the argument to `x`.
  Additionally, any planning units missing cost (`NA`) values should
  also have missing (`NA`) values in the argument to `solution`.

- `x` has `matrix` planning units:

  The argument to `solution` must be a `matrix` vector with each row
  corresponding to a different planning unit, and each column correspond
  to a different management zone. It should have the same number of
  planning units and zones as those in the argument to `x`.
  Additionally, any planning units missing cost (`NA`) values for a
  particular zone should also have a missing (`NA`) values in the
  argument to `solution`.

- `x` has
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  planning units:

  The argument to `solution` be a
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object where different cells correspond to different planning units
  and layers correspond to a different management zones. It should have
  the same dimensionality (rows, columns, layers), resolution, extent,
  and coordinate reference system as the planning units in the argument
  to `x`. Additionally, any planning units missing cost (`NA`) values
  for a particular zone should also have missing (`NA`) values in the
  argument to `solution`.

- `x` has `data.frame` planning units:

  The argument to `solution` must be a `data.frame` with each column
  corresponding to a different zone, each row corresponding to a
  different planning unit, and cell values corresponding to the solution
  value. This means that if a `data.frame` object containing the
  solution also contains additional columns, then these columns will
  need to be subsetted prior to using this function (see below for
  example with
  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) data).
  Additionally, any planning units missing cost (`NA`) values for a
  particular zone should also have missing (`NA`) values in the argument
  to `solution`.

- `x` has [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  planning units:

  The argument to `solution` must be a
  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
  with each column corresponding to a different zone, each row
  corresponding to a different planning unit, and cell values
  corresponding to the solution value. This means that if the
  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
  containing the solution also contains additional columns, then these
  columns will need to be subsetted prior to using this function (see
  below for example). Additionally, the argument to `solution` must also
  have the same coordinate reference system as the planning unit data.
  Furthermore, any planning units missing cost (`NA`) values for a
  particular zone should also have missing (`NA`) values in the argument
  to `solution`.

## References

Ferrier S, Pressey RL, and Barrett TW (2000) A new predictor of the
irreplaceability of areas for achieving a conservation goal, its
application to real-world planning, and a research agenda for further
refinement. *Biological Conservation*, 93: 303–325.

## See also

See [importance](https://prioritizr.net/reference/importance.md) for an
overview of all functions for evaluating the importance of planning
units selected in a solution.

Other functions for evaluating solution importance:
[`eval_rank_importance()`](https://prioritizr.net/reference/eval_rank_importance.md),
[`eval_rare_richness_importance()`](https://prioritizr.net/reference/eval_rare_richness_importance.md),
[`eval_replacement_importance()`](https://prioritizr.net/reference/eval_replacement_importance.md)

## Examples

``` r
# \dontrun{
# seed seed for reproducibility
set.seed(600)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_pu_polygons <- get_sim_pu_polygons()
sim_features <- get_sim_features()

# create minimal problem with binary decisions
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve problem
s1 <- solve(p1)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013

# print solution
print(s1)
#> Error: object 's1' not found

# plot solution
plot(s1, main = "solution", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's1' not found

# calculate importance scores using Ferrier et al. 2000 method
fs1 <- eval_ferrier_importance(p1, s1)
#> Error in eval_ferrier_importance(p1, s1): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's1' not found

# print importance scores,
# each planning unit has an importance score for each feature
# (as indicated by the column names) and each planning unit also
# has an overall total importance score (in the "total" column)
print(fs1)
#> Error: object 'fs1' not found

# plot total importance scores
plot(fs1, main = names(fs1), axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 'fs1' not found

# create minimal problem with polygon planning units
p2 <-
  problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.05) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve problem
s2 <- solve(p2)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0 0.013

# print solution
print(s2)
#> Error: object 's2' not found

# plot solution
plot(s2[, "solution_1"], main = "solution")
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's2' not found

# calculate importance scores
fs2 <- eval_ferrier_importance(p2, s2[, "solution_1"])
#> Error in eval_ferrier_importance(p2, s2[, "solution_1"]): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's2' not found

# plot importance scores
plot(fs2)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 'fs2' not found

# }
```
