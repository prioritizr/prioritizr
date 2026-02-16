# Evaluate target coverage by solution

Calculate how well feature representation
[targets](https://prioritizr.net/reference/targets.md) are met by a
solution to a conservation planning problem. It is useful for
understanding if features are adequately represented by a solution. Note
that this function can only be used with problems that contain
[targets](https://prioritizr.net/reference/targets.md).

## Usage

``` r
eval_target_coverage_summary(
  x,
  solution,
  include_zone = number_of_zones(x) > 1,
  include_sense = number_of_zones(x) > 1
)
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

- include_zone:

  `logical` include the `zone` column in the output? Defaults to `TRUE`
  for problems that contain multiple zones.

- include_sense:

  `logical` include the `sense` column in the output? Defaults to `TRUE`
  for problems that contain multiple zones.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object. Here, each row describes information for a different target. It
contains the following columns:

- feature:

  `character` name of the feature associated with each target.

- zone:

  `list` of `character` zone names associated with each target. This
  column is in a list-column format because a single target can
  correspond to multiple zones (see
  [`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)
  for details and examples). For an example of converting the
  list-column format to a standard `character` column format, please see
  the Examples section. This column is only included if the argument to
  `include_zones` is `TRUE`.

- sense:

  `character` sense associated with each target. Sense values specify
  the nature of the target. Typically (e.g., when using the
  [`add_absolute_targets()`](https://prioritizr.net/reference/add_absolute_targets.md)
  or
  [`add_relative_targets()`](https://prioritizr.net/reference/add_relative_targets.md)
  functions), targets are specified using sense values indicating that
  the total amount of a feature held within a solution (ideally) be
  greater than or equal to a threshold amount (i.e., a sense value of
  `">="`). Additionally, targets (i.e., using the
  [`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)
  function) can also be specified using sense values indicating that the
  total amount of a feature held within a solution must be equal to a
  threshold amount (i.e., a sense value of `"="`) or smaller than or
  equal to a threshold amount (i.e., a sense value of `"<="`). This
  column is only included if the argument to `include_sense` is `TRUE`.

- total_amount:

  `numeric` total amount of the feature available across the entire
  conservation planning problem for meeting each target (not just
  planning units selected within the solution). For problems involving a
  single zone, this column is calculated as the sum of all of the values
  for a given feature (similar to values in the `total_amount` column
  produced by the
  [`eval_feature_representation_summary()`](https://prioritizr.net/reference/eval_feature_representation_summary.md)
  function). For problems involving multiple zones, this column is
  calculated as the sum of the values for the feature associated with
  target (per the `"feature"` column), across the zones associated with
  the target (per the `"zone"` column).

- absolute_target:

  `numeric` total threshold amount associated with each target.

- absolute_held:

  `numeric` total amount held within the solution for the feature and
  (if relevant) zones associated with each target (per the `"feature"`
  and `"zone"` columns, respectively). This column is calculated as the
  sum of the feature data, supplied when creating a
  [`problem()`](https://prioritizr.net/reference/problem.md) object
  (e.g., presence/absence values), weighted by the status of each
  planning unit in the solution (e.g., selected or not for
  prioritization).

- absolute_shortfall:

  `numeric` total amount by which the solution fails to meet each
  target. This column is calculated as the difference between the total
  amount held within the solution for the feature and (if relevant)
  zones associated with the target (i.e., `"absolute_held"` column) and
  the target total threshold amount (i.e., `"absolute_target"` column),
  with values set to zero depending on the sense specified for the
  target (e.g., if the target sense is `>=` then the difference is set
  to zero if the value in the `"absolute_held"` is smaller than that in
  the `"absolute_target"` column).

- relative_target:

  `numeric` proportion threshold amount associated with each target.
  This column is calculated by dividing the total threshold amount
  associated with each target (i.e., `"absolute_target"` column) by the
  total amount associated with each target (i.e., `"total_amount"`
  column).

- relative_held:

  `numeric` proportion held within the solution for the feature and (if
  relevant) zones associated with each target (per the `"feature"` and
  `"zone"` columns, respectively). This column is calculated by dividing
  the total amount held for each target (i.e., `"absolute_held"` column)
  by the total amount for with each target (i.e., `"total_amount"`
  column).

- relative_shortfall:

  `numeric` proportion by which the solution fails to meet each target.
  This column is calculated by dividing the total shortfall for each
  target (i.e., `"absolute_shortfall"` column) by the total threshold
  amount associated with each target (i.e., `"absolute_target"` column).

- met:

  `logical` indicating if each target is met by the solution. This
  column is calculated by checking if the total shortfall associated
  with each target (i.e., `"absolute_shortfall`" column) is equal to
  zero.

## Notes

In prior versions (\< 8.0.6.7), this function calculated the relative
shortfall for a target by dividing the total shortfall for the target
(i.e., `"absolute_shortfall"` column) by the total amount associated
with each target (i.e., `"total_amount"` column). This was subsequently
changed to ensure consistency with the minimum shortfall objective
([`add_min_shortfall_objective()`](https://prioritizr.net/reference/add_min_shortfall_objective.md)).

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

## See also

See [summaries](https://prioritizr.net/reference/summaries.md) for an
overview of all functions for summarizing solutions.

Other functions for summarizing solutions:
[`eval_asym_connectivity_summary()`](https://prioritizr.net/reference/eval_asym_connectivity_summary.md),
[`eval_boundary_summary()`](https://prioritizr.net/reference/eval_boundary_summary.md),
[`eval_connectivity_summary()`](https://prioritizr.net/reference/eval_connectivity_summary.md),
[`eval_cost_summary()`](https://prioritizr.net/reference/eval_cost_summary.md),
[`eval_feature_representation_summary()`](https://prioritizr.net/reference/eval_feature_representation_summary.md),
[`eval_n_summary()`](https://prioritizr.net/reference/eval_n_summary.md)

## Examples

``` r
# \dontrun{
# set seed for reproducibility
set.seed(500)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_pu_polygons <- get_sim_pu_polygons()
sim_features <- get_sim_features()
sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
sim_zones_features <- get_sim_zones_features()

# build minimal conservation problem with raster data
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve the problem
s1 <- solve(p1)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.004 0 0.014

# print solution
print(s1)
#> Error: object 's1' not found

# plot solution
plot(s1, main = "solution", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's1' not found

# calculate target coverage by the solution
r1 <- eval_target_coverage_summary(p1, s1)
#> Error in eval_target_coverage_summary(p1, s1): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's1' not found
print(r1, width = Inf) # note: `width = Inf` tells R to print all columns
#> Error: object 'r1' not found

# build minimal conservation problem with polygon data
p2 <-
  problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve the problem
s2 <- solve(p2)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0 0.013

# print first six rows of the attribute table
print(head(s2))
#> Error: object 's2' not found

# plot solution
plot(s2[, "solution_1"])
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's2' not found

# calculate target coverage by the solution
r2 <- eval_target_coverage_summary(p2, s2[, "solution_1"])
#> Error in eval_target_coverage_summary(p2, s2[, "solution_1"]): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's2' not found
print(r2, width = Inf)
#> Error: object 'r2' not found

# build multi-zone conservation problem with polygon data
p3 <-
  problem(
    sim_zones_pu_polygons, sim_zones_features,
    cost_column = c("cost_1", "cost_2", "cost_3")
  ) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve the problem
s3 <- solve(p3)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013

# print solution
print(s3)
#> Error: object 's3' not found

# create new column representing the zone id that each planning unit
# was allocated to in the solution
s3$solution <- category_vector(
  s3[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
)
#> Error in category_vector(s3[, c("solution_1_zone_1", "solution_1_zone_2",     "solution_1_zone_3")]): ℹ In argument to `x`.
#> Caused by error:
#> ! object 's3' not found
s3$solution <- factor(s3$solution)
#> Error: object 's3' not found

# plot solution
plot(s3[, "solution"])
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's3' not found

# calculate target coverage by the solution
r3 <- eval_target_coverage_summary(
  p3, s3[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
)
#> Error in eval_target_coverage_summary(p3, s3[, c("solution_1_zone_1",     "solution_1_zone_2", "solution_1_zone_3")]): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's3' not found
print(r3, width = Inf)
#> Error: object 'r3' not found

# create a new column with character values containing the zone names,
# by extracting these data out of the zone column
# (which is in list-column format)
r3$zone2 <- vapply(r3$zone, FUN.VALUE = character(1), paste, sep = " & ")
#> Error: object 'r3' not found

# print r3 again to show the new column
print(r3, width = Inf)
#> Error: object 'r3' not found
# }
```
