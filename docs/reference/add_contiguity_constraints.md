# Add contiguity constraints

Add constraints to a conservation planning problem to ensure that all
selected planning units are spatially connected with each other and form
a single contiguous unit.

## Usage

``` r
# S4 method for class 'ConservationProblem,ANY,ANY'
add_contiguity_constraints(x, zones, data)

# S4 method for class 'ConservationProblem,ANY,data.frame'
add_contiguity_constraints(x, zones, data)

# S4 method for class 'ConservationProblem,ANY,matrix'
add_contiguity_constraints(x, zones, data)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- zones:

  `matrix` or `Matrix` object describing the connection scheme for
  different zones. Each row and column corresponds to a different zone
  in the argument to `x`, and cell values must contain binary `numeric`
  values (i.e., one or zero) that indicate if connected planning units
  (as specified in the argument to `data`) should be still considered
  connected if they are allocated to different zones. The cell values
  along the diagonal of the matrix indicate if planning units should be
  subject to contiguity constraints when they are allocated to a given
  zone. Note arguments to `zones` must be symmetric, and that a row or
  column has a value of one then the diagonal element for that row or
  column must also have a value of one. The default argument to `zones`
  is an identity matrix (i.e., a matrix with ones along the matrix
  diagonal and zeros elsewhere), so that planning units are only
  considered connected if they are both allocated to the same zone.

- data:

  `NULL`, `matrix`, `Matrix`, `data.frame` object showing which planning
  units are connected with each other. The argument defaults to `NULL`
  which means that the connection data is calculated automatically using
  the
  [`adjacency_matrix()`](https://prioritizr.net/reference/adjacency_matrix.md)
  function. See the Data format section for more information.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the constraints added to it.

## Details

This function uses connection data to identify solutions that form a
single contiguous unit. It was inspired by the mathematical formulations
detailed in Önal and Briers (2006).

## Data format

The argument to `data` can be specified using the following formats.

- `data` as a `NULL` value:

  indicating that connection data should be calculated automatically
  using the
  [`adjacency_matrix()`](https://prioritizr.net/reference/adjacency_matrix.md)
  function. This is the default argument. Note that the connection data
  must be manually defined using one of the other formats below when the
  planning unit data in the argument to `x` is not spatially referenced
  (e.g., in `data.frame` or `numeric` format).

- `data` as a `matrix`/`Matrix` object:

  where rows and columns represent different planning units and the
  value of each cell indicates if the two planning units are connected
  or not. Cell values should be binary `numeric` values (i.e., one or
  zero). Cells that occur along the matrix diagonal have no effect on
  the solution at all because each planning unit cannot be a connected
  with itself.

- `data` as a `data.frame` object:

  containing columns that are named `"id1"`, `"id2"`, and `"boundary"`.
  Here, each row denotes the connectivity between two planning units
  following the *Marxan* format. The `"boundary"` column should contain
  binary `numeric` values that indicate if the two planning units
  specified in the `"id1"` and `"id2"` columns are connected or not.
  This data can be used to describe symmetric or asymmetric
  relationships between planning units. By default, input data is
  assumed to be symmetric unless asymmetric data is also included (e.g.,
  if data is present for planning units 2 and 3, then the same amount of
  connectivity is expected for planning units 3 and 2, unless
  connectivity data is also provided for planning units 3 and 2).

## Notes

In early versions, this function was named as the
[`add_connected_constraints()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
function.

## References

Önal H and Briers RA (2006) Optimal selection of a connected reserve
network. *Operations Research*, 54: 379–388.

## See also

See [constraints](https://prioritizr.net/reference/constraints.md) for
an overview of all functions for adding constraints.

Other functions for adding constraints:
[`add_feature_contiguity_constraints()`](https://prioritizr.net/reference/add_feature_contiguity_constraints.md),
[`add_linear_constraints()`](https://prioritizr.net/reference/add_linear_constraints.md),
[`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md),
[`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md),
[`add_mandatory_allocation_constraints()`](https://prioritizr.net/reference/add_mandatory_allocation_constraints.md),
[`add_manual_bounded_constraints()`](https://prioritizr.net/reference/add_manual_bounded_constraints.md),
[`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md),
[`add_neighbor_constraints()`](https://prioritizr.net/reference/add_neighbor_constraints.md)

## Examples

``` r
# \dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_features <- get_sim_zones_features()

# create minimal problem
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create problem with added connected constraints
p2 <- p1 %>% add_contiguity_constraints()

# solve problems
s1 <- c(solve(p1), solve(p2))
names(s1) <- c("basic solution", "connected solution")

# plot solutions
plot(s1, axes = FALSE)


# create minimal problem with multiple zones, and limit the solver to
# 30 seconds to obtain solutions in a feasible period of time
p3 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
  add_binary_decisions() %>%
  add_default_solver(time_limit = 30, verbose = FALSE)

# create problem with added constraints to ensure that the planning units
# allocated to each zone form a separate contiguous unit
z4 <- diag(3)
print(z4)
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1
p4 <- p3 %>% add_contiguity_constraints(z4)

# create problem with added constraints to ensure that the planning
# units allocated to each zone form a separate contiguous unit,
# except for planning units allocated to zone 3 which do not need
# form a single contiguous unit
z5 <- diag(3)
z5[3, 3] <- 0
print(z5)
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    0
p5 <- p3 %>% add_contiguity_constraints(z5)

# create problem with added constraints that ensure that the planning
# units allocated to zones 1 and 2 form a contiguous unit
z6 <- diag(3)
z6[1, 2] <- 1
z6[2, 1] <- 1
print(z6)
#>      [,1] [,2] [,3]
#> [1,]    1    1    0
#> [2,]    1    1    0
#> [3,]    0    0    1
p6 <- p3 %>% add_contiguity_constraints(z6)

# solve problems
s2 <- lapply(list(p3, p4, p5, p6), solve)
s2 <- lapply(s2, category_layer)
s2 <- terra::rast(s2)
names(s2) <- c("basic solution", "p4", "p5", "p6")

# plot solutions
plot(s2, axes = FALSE)


# create a problem that has a main "reserve zone" and a secondary
# "corridor zone" to connect up import areas. Here, each feature has a
# target of 50% of its distribution. If a planning unit is allocated to the
# "reserve zone", then the prioritization accrues 100% of the amount of
# each feature in the planning unit. If a planning unit is allocated to the
# "corridor zone" then the prioritization accrues 40% of the amount of each
# feature in the planning unit. Also, the cost of managing a planning unit
# in the "corridor zone" is 30% of that when it is managed as the
# "reserve zone". Finally, the problem has constraints which
# ensure that all of the selected planning units form a single contiguous
# unit, so that the planning units allocated to the "corridor zone" can
# link up the planning units allocated to the "reserve zone"

# create planning unit data
pus <- sim_zones_pu_raster[[c(1, 1)]]
pus[[2]] <- pus[[2]] * 0.3
print(pus)
#> class       : SpatRaster 
#> size        : 10, 10, 2  (nrow, ncol, nlyr)
#> resolution  : 0.1, 0.1  (x, y)
#> extent      : 0, 1, 0, 1  (xmin, xmax, ymin, ymax)
#> coord. ref. : Undefined Cartesian SRS 
#> sources     : sim_zones_pu_raster.tif  
#>               memory  
#> varnames    : sim_zones_pu_raster 
#>               sim_zones_pu_raster 
#> names       :   zone_1,   zone_1 
#> min values  : 190.1328, 57.03983 
#> max values  : 215.8638, 64.75915 

# create biodiversity data
fts <- zones(
  sim_features, sim_features * 0.4,
  feature_names = names(sim_features),
  zone_names = c("reserve zone", "corridor zone")
)
print(fts)
#> A zones object <ZonesSpatRaster/Zones>
#> • zones:    "reserve zone" and "corridor zone" (2 total)
#> • features: "feature_1", "feature_2", "feature_3", "feature_4", and "feature_5" (5 total)

# create targets
targets <- tibble::tibble(
  feature = names(sim_features),
  zone = list(zone_names(fts))[rep(1, 5)],
  target = terra::global(sim_features, "sum", na.rm = TRUE)[[1]] * 0.5,
  type = rep("absolute", 5)
)
print(targets)
#> # A tibble: 5 × 4
#>   feature   zone      target type    
#>   <chr>     <list>     <dbl> <chr>   
#> 1 feature_1 <chr [2]>   41.6 absolute
#> 2 feature_2 <chr [2]>   15.6 absolute
#> 3 feature_3 <chr [2]>   36.0 absolute
#> 4 feature_4 <chr [2]>   21.3 absolute
#> 5 feature_5 <chr [2]>   28.4 absolute

# create zones matrix
z7 <- matrix(1, ncol = 2, nrow = 2)
print(z7)
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1

# create problem
p7 <-
  problem(pus, fts) %>%
  add_min_set_objective() %>%
  add_manual_targets(targets) %>%
  add_contiguity_constraints(z7) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problems
s7 <- category_layer(solve(p7))

# plot solutions
plot(s7, main = "solution", axes = FALSE)

# }
```
