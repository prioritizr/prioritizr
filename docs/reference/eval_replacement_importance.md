# Evaluate solution importance using replacement cost scores

Calculate importance scores for planning units selected in a solution
based on the replacement cost method (Cabeza and Moilanen 2006).

## Usage

``` r
eval_replacement_importance(
  x,
  solution,
  rescale = TRUE,
  run_checks = TRUE,
  force = FALSE,
  threads = 1L
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

- rescale:

  `logical` flag indicating if replacement cost values – excepting
  infinite (`Inf`) and zero values – should be rescaled to range between
  0.01 and 1. Defaults to `TRUE`.

- run_checks:

  `logical` flag indicating whether presolve checks should be run prior
  solving the problem. These checks are performed using the
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  function. Defaults to `TRUE`. Skipping these checks may reduce run
  time for large problems.

- force:

  `logical` flag indicating if an attempt should be made to solve the
  problem even if potential issues were detected during the presolve
  checks. Defaults to `FALSE`.

- threads:

  `integer` number of threads to use for the optimization algorithm.
  Defaults to 1 such that only a single thread is used.

## Value

A `numeric`, `matrix`, `data.frame`,
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
or [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
containing the importance scores for each planning unit in the solution.
Specifically, the returned object is in the same format as the planning
unit data in the argument to `x`.

## Details

This function implements a modified version of the replacement cost
method (Cabeza and Moilanen 2006). Specifically, the score for each
planning unit is calculated as the difference in the objective value of
a solution when each planning unit is locked out and the optimization
processes rerun with all other selected planning units locked in. In
other words, the replacement cost metric corresponds to change in
solution quality incurred if a given planning unit cannot be acquired
when implementing the solution and the next best planning unit (or set
of planning units) will need to be considered instead. Thus planning
units with a higher score are more important (and irreplaceable). For
example, when using the minimum set objective function
([`add_min_set_objective()`](https://prioritizr.net/reference/add_min_set_objective.md)),
the replacement cost scores correspond to the additional costs needed to
meet targets when each planning unit is locked out. When using the
maximum utility objective function
([`add_max_utility_objective()`](https://prioritizr.net/reference/add_max_utility_objective.md),
the replacement cost scores correspond to the reduction in the utility
when each planning unit is locked out. Infinite values mean that no
feasible solution exists when planning units are locked out—they are
absolutely essential for obtaining a solution (e.g., they contain rare
species that are not found in any other planning units or were locked
in). Zeros values mean that planning units can be swapped with other
planning units and this will have no effect on the performance of the
solution at all (e.g., because they were only selected due to spatial
fragmentation penalties).

These calculations can take a long time to complete for large or complex
conservation planning problems. As such, we recommend using this method
for small or moderate-sized conservation planning problems (e.g., \<
30,000 planning units). To reduce run time, we recommend calculating
these scores without additional penalties (e.g.,
[`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md))
or spatial constraints (e.g.,
[`add_contiguity_constraints()`](https://prioritizr.net/reference/add_contiguity_constraints.md)).
To further reduce run time, we recommend using proportion-type decisions
when calculating the scores (see below for an example).

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

Cabeza M and Moilanen A (2006) Replacement cost: A practical measure of
site value for cost-effective reserve planning. *Biological
Conservation*, 132: 336–342.

## See also

See [importance](https://prioritizr.net/reference/importance.md) for an
overview of all functions for evaluating the importance of planning
units selected in a solution.

Other functions for evaluating solution importance:
[`eval_ferrier_importance()`](https://prioritizr.net/reference/eval_ferrier_importance.md),
[`eval_rank_importance()`](https://prioritizr.net/reference/eval_rank_importance.md),
[`eval_rare_richness_importance()`](https://prioritizr.net/reference/eval_rare_richness_importance.md)

## Examples

``` r
# \dontrun{
# seed seed for reproducibility
set.seed(600)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_pu_polygons <- get_sim_pu_polygons()
sim_features <- get_sim_features()
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_features <- get_sim_zones_features()

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

# calculate importance scores
rc1 <- eval_replacement_importance(p1, s1)
#> Error in eval_replacement_importance(p1, s1): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's1' not found

# print importance scores
print(rc1)
#> Error: object 'rc1' not found

# plot importance scores
plot(rc1, main = "replacement cost", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 'rc1' not found

# since replacement cost scores can take a long time to calculate with
# binary decisions, we can calculate them using proportion-type
# decision variables. Note we are still calculating the scores for our
# previous solution (s1), we are just using a different optimization
# problem when calculating the scores.
p2 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_proportion_decisions() %>%
  add_default_solver(gap = 0, verbose = FALSE)

# calculate importance scores using proportion type decisions
rc2 <- eval_replacement_importance(p2, s1)
#> Error in eval_replacement_importance(p2, s1): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's1' not found

# print importance scores based on proportion type decisions
print(rc2)
#> Error: object 'rc2' not found

# plot importance scores based on proportion type decisions
# we can see that the importance values in rc1 and rc2 are similar,
# and this confirms that the proportion type decisions are a good
# approximation
plot(rc2, main = "replacement cost", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 'rc2' not found

# create minimal problem with polygon planning units
p3 <-
  problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.05) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve problem
s3 <- solve(p3)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.014

# print solution
print(s3)
#> Error: object 's3' not found

# plot solution
plot(s3[, "solution_1"], main = "solution")
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's3' not found

# calculate importance scores
rc3 <- eval_rare_richness_importance(p3, s3[, "solution_1"])
#> Error in eval_rare_richness_importance(p3, s3[, "solution_1"]): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's3' not found

# plot importance scores
plot(rc3, main = "replacement cost")
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 'rc3' not found

# build multi-zone conservation problem with raster data
p4 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve the problem
s4 <- solve(p4)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.001 0 0.013
names(s4) <- paste0("zone ", seq_len(terra::nlyr(s4)))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'nlyr': object 's4' not found

# print solution
print(s4)
#> Error: object 's4' not found

# plot solution
# each panel corresponds to a different zone, and data show the
# status of each planning unit in a given zone
plot(s4, axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's4' not found

# calculate importance scores
rc4 <- eval_replacement_importance(p4, s4)
#> Error in eval_replacement_importance(p4, s4): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's4' not found
names(rc4) <- paste0("zone ", seq_len(terra::nlyr(s4)))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'nlyr': object 's4' not found

# plot importance
# each panel corresponds to a different zone, and data show the
# importance of each planning unit in a given zone
plot(rc4, axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 'rc4' not found
# }
```
