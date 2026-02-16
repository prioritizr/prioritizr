# Add mandatory allocation constraints

Add constraints to a conservation planning problem to ensure that every
planning unit is allocated to a management zone in the solution. Note
that this function can only be used with problems that contain multiple
zones.

## Usage

``` r
add_mandatory_allocation_constraints(x)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the constraints added to it.

## Details

For a conservation planning
[`problem()`](https://prioritizr.net/reference/problem.md) with multiple
management zones, it may sometimes be desirable to obtain a solution
that assigns each and every planning unit to a zone. For example, when
developing land-use plans, some decision makers may require that every
parcel of land is allocated a specific land-use type. In other words are
no "left over" areas. Although it might seem tempting to simply solve
the problem and manually assign "left over" planning units to a default
zone afterwards (e.g., an "other", "urban", or "grazing" land-use), this
could result in highly sub-optimal solutions if there are penalties for
siting the default land-use adjacent to other zones. Instead, this
function can be used to specify that all planning units in a problem
with multiple zones must be allocated to a management zone (i.e., zone
allocation is mandatory).

## See also

See [constraints](https://prioritizr.net/reference/constraints.md) for
an overview of all functions for adding constraints.

Other functions for adding constraints:
[`add_contiguity_constraints()`](https://prioritizr.net/reference/add_contiguity_constraints.md),
[`add_feature_contiguity_constraints()`](https://prioritizr.net/reference/add_feature_contiguity_constraints.md),
[`add_linear_constraints()`](https://prioritizr.net/reference/add_linear_constraints.md),
[`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md),
[`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md),
[`add_manual_bounded_constraints()`](https://prioritizr.net/reference/add_manual_bounded_constraints.md),
[`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md),
[`add_neighbor_constraints()`](https://prioritizr.net/reference/add_neighbor_constraints.md)

## Examples

``` r
# \dontrun{
# set seed for reproducibility
set.seed(500)

# load data
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_features <- get_sim_zones_features()

# create multi-zone problem with minimum set objective
targets_matrix <- matrix(rpois(15, 1), nrow = 5, ncol = 3)

# create minimal problem with minimum set objective
p1 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_absolute_targets(targets_matrix) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create another problem that is the same as p1, but has constraints
# to mandate that every planning unit in the solution is assigned to
# zone
p2 <- p1 %>% add_mandatory_allocation_constraints()

# solve problems
s1 <- solve(p1)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013
s2 <- solve(p2)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013

# convert solutions into category layers, where each cell is assigned
 # value indicating which zone it was assigned to in the zone
c1 <- category_layer(s1)
#> Error in category_layer(s1): ℹ In argument to `x`.
#> Caused by error:
#> ! object 's1' not found
c2 <- category_layer(s2)
#> Error in category_layer(s2): ℹ In argument to `x`.
#> Caused by error:
#> ! object 's2' not found

# plot solution category layers
plot(c(c1, c2), main = c("default", "mandatory allocation"), axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 'c1' not found
# }
```
