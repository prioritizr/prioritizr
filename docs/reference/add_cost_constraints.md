# Add cost constraints

Add constraints to a conservation planning problem to ensure that the
cost of selected planning units meets certain criteria.

## Usage

``` r
add_cost_constraints(x, budget, sense)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- budget:

  `numeric` value specifying the maximum expenditure permitted for the
  solution. If `x` has multiple zones, then `budget` can be (i) a single
  `numeric` value to specify an overall budget for the entire solution
  or (ii) a `numeric` vector to specify a budget for each zone
  (separately) in the solution.

- sense:

  `character` value specifying the constraint sense. Acceptable values
  are: `">="`, `"<="`, or `"="`. If `x` has multiple zones, then `sense`
  can be (i) a single `character` value to specify a constraint sense
  for the entire solution or (ii) a `character` vector to specify a
  different constraint sense for each zone (separately) in the solution.
  Note that `sense` and `budget` must have the same number of values.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the constraints added to it.

## Details

This function adds constraints constraints that can be used to ensure
that the cost of solutions meet certain criteria (see Examples section
below for details). For example, these constraints can be used to
specify both minimum and maximum budget thresholds. Note that this
function provided as a convenient alternative for adding linear
constraints (per
[`add_linear_constraints()`](https://prioritizr.net/reference/add_linear_constraints.md))
to a [`problem()`](https://prioritizr.net/reference/problem.md).

## Mathematical formulation

The cost constraints are implemented using the following equation. Let
\\I\\ denote the set of planning units (indexed by \\i\\), \\Z\\ the set
of management zones (indexed by \\z\\), and \\X\_{iz}\\ the decision
variable for allocating planning unit \\i\\ to zone \\z\\ (e.g., with
binary values indicating if each planning unit is allocated or not).
Also, let \\D\_{iz}\\ denote the costs associated with planning units
\\i \in I\\ for zones \\z \in Z\\ (per `data`, if supplied as a `matrix`
object), \\\theta\\ denote the constraint sense (per `sense`), and \\B\\
denote the budget threshold (per `budget`).

\$\$ \sum\_{i}^{I} \sum\_{z}^{Z} (D\_{iz} \times X\_{iz}) \space \theta
\space t \$\$

## See also

Other functions for adding constraints:
[`add_contiguity_constraints()`](https://prioritizr.net/reference/add_contiguity_constraints.md),
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
# set seed for reproducibility
set.seed(600)

# load data
sim_complex_pu_raster <- get_sim_complex_pu_raster()
sim_complex_features <- get_sim_complex_features()

# create layer with 1s for all planning units
sim_ones_complex_raster <- (sim_complex_pu_raster * 0) + 1

# here we will formulate a multi-objective optimization problem
# that (i) minimizes the largest target shortfall for feature representation,
# (ii) minimizes the overall target shortfalls for feature representation,
# and (iii) minimizes the cost of the solution. since the
# first objective is to minimize the largest shortfall and the second
# objective is to minimize overall target shortfalls,
# this helps balance shortfalls among all features and better
# achieve complementarity. additionally, we will specify that
# (approximately) 30% of the study area should be selected (i.e., by
# specifying a budget for the upper threshold and a linear constraint for
# the lower threshold on the number of selected planning units).

# calculate budget based on 30% of the number of planning units
budget <-
  0.3 * terra::global(sim_ones_complex_raster, "sum", na.rm = TRUE)[[1]]

# build multi-objective conservation planning problem
mp <-
  multi_problem(
    obj1 =
      problem(sim_ones_complex_raster, sim_complex_features) %>%
      add_min_largest_shortfall_objective(budget = budget) %>%
      add_auto_targets("jung") %>%
      # note that this constraint only needs to be specified once
      add_cost_constraints(sense = ">=", budget = budget * 0.9) %>%
      add_binary_decisions(),
    obj2 =
      problem(sim_ones_complex_raster, sim_complex_features) %>%
      add_min_shortfall_objective(budget = budget) %>%
      # note that we use the same targets for both obj1 and obj2
      add_auto_targets("jung") %>%
      add_binary_decisions(),
    obj3 =
      problem(sim_complex_pu_raster, sim_complex_pu_raster) %>%
      add_min_penalties_objective() %>%
      # note a value of 1 is here because only the costs minimized
      add_cost_penalties(1) %>%
      add_binary_decisions()
  ) %>%
  add_default_solver(gap = 0.01, verbose = FALSE)

# to explore trade-offs between how well the feature targets
# are met and cost, we will generate a matrix of relative tolerance values
# for the hierarchical approach. note that the first column of this
# matrix will have only zeros to help promote balanced
# shortfalls across different features, and the second column
# will have non-zeros because we are interested in trade-offs between
# overall feature shortfalls and cost
rel_tol_matrix <- matrix(0, ncol = 2, nrow = 10)
rel_tol_matrix[, 2] <- seq(0, 0.5, length.out = nrow(rel_tol_matrix))

# display matrix
print(rel_tol_matrix)
#>       [,1]       [,2]
#>  [1,]    0 0.00000000
#>  [2,]    0 0.05555556
#>  [3,]    0 0.11111111
#>  [4,]    0 0.16666667
#>  [5,]    0 0.22222222
#>  [6,]    0 0.27777778
#>  [7,]    0 0.33333333
#>  [8,]    0 0.38888889
#>  [9,]    0 0.44444444
#> [10,]    0 0.50000000

# add hierarchical approach to multi-objective problem
mp <-
  mp %>%
  add_hier_approach(rel_tol = rel_tol_matrix)

# generate solutions
ms <- solve(mp)
#> ■■■■                              10% | ETA: 31s
#> ■■■■■■■                           20% | ETA: 29s
#> ■■■■■■■■■■                        30% | ETA: 25s
#> ■■■■■■■■■■■■■                     40% | ETA: 21s
#> ■■■■■■■■■■■■■■■■                  50% | ETA: 18s
#> ■■■■■■■■■■■■■■■■■■■               60% | ETA: 14s
#> ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA: 11s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         80% | ETA:  7s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      90% | ETA:  3s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s

# plot the solutions
plot(terra::rast(ms), axes = FALSE)


# extract objective values for the solutions
obj_matrix <- attributes(ms)$objective

# preview the objective values
head(obj_matrix)
#>                 obj1     obj2      obj3
#> solution_1 0.5254115 17.44765 1027535.5
#> solution_2 0.5254618 18.41463  855765.4
#> solution_3 0.5254624 19.38561  771956.7
#> solution_4 0.5254624 20.35338  718990.5
#> solution_5 0.5254624 21.32408  680327.2
#> solution_6 0.5254624 22.29214  648289.1

# plot the objectives values to visualize trade-offs
# (note that smaller values are better for both objectives)
plot(
  obj_matrix[, 2:3],
  main = "Trade-offs between objectives",
  xlab = "Species representation (overall shortfall)",
  ylab = "Solution cost"
)

# }
```
