# Add minimum penalties objective

Set the objective of a conservation planning problem to minimize the
penalties added to the problem. Targets can optionally be specified to
ensure that the solution must meet all the
[targets](https://prioritizr.net/reference/targets.md). Budgets can also
optionally be specified to ensure that the solution does not exceed a
budgetary threshold. This objective is designed to be used with
multi-objective optimization.

## Usage

``` r
add_min_penalties_objective(x, budget = NULL)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- budget:

  `numeric` value specifying the maximum expenditure permitted for the
  solution. If `x` has multiple zones, then `budget` can be (i) a single
  `numeric` value to specify an overall budget for the entire solution
  or (ii) a `numeric` vector to specify a budget for each zone
  (separately) in the solution. Defaults to `NULL` such expenditure is
  not limited.

## Details

The minimum penalty objective is designed to be used with problems that
have penalties (see
[penalties](https://prioritizr.net/reference/penalties.md) for details).
It can be used to generate solutions that focus entirely on minimizing
the penalties, whilst (optionally) ensuring that certain constraints are
met. This is is useful when performing multi-objective optimization (see
examples below).

## Mathematical formulation

This objective can be expressed mathematically for a set of planning
units (\\I\\ indexed by \\i\\) and a set of features (\\J\\ indexed by
\\j\\) as:

\$\$\mathit{Minimize} \space 0 \\ \mathit{subject \space to} \\ \sum\_{i
= 1}^{I} x_i r\_{ij} \geq T_j \space \forall \space j \in J \\ \sum\_{i
= 1}^{I} x_i c_i \leq B\$\$

Here, \\x_i\\ is the
[decisions](https://prioritizr.net/reference/decisions.md) variable
(e.g., specifying whether planning unit \\i\\ has been selected (1) or
not (0)), \\c_i\\ is the cost of planning unit \\i\\, \\r\_{ij}\\ is the
amount of feature \\j\\ in planning unit \\i\\, and \\T_j\\ is the
target for feature \\j\\. Since the objective is to minimize zero, this
function does not actually provide any criteria to compare competing
solutions. As such, when used in conjunction with a penalty function
(see [penalties](https://prioritizr.net/reference/penalties.md)), only
the penalty (e.g.,
[`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md))
will be used to compare competing solutions during optimization.

## See also

See [objectives](https://prioritizr.net/reference/objectives.md) for an
overview of all functions for adding objectives. Also see
[targets](https://prioritizr.net/reference/targets.md) for an overview
of all functions for adding targets. Additionally, see
[penalties](https://prioritizr.net/reference/penalties.md) for an
overview of all functions for adding penalties.

Other functions for adding objectives:
[`add_max_cover_objective()`](https://prioritizr.net/reference/add_max_cover_objective.md),
[`add_max_n_targets_met_objective()`](https://prioritizr.net/reference/add_max_n_targets_met_objective.md),
[`add_max_phylo_div_objective()`](https://prioritizr.net/reference/add_max_phylo_div_objective.md),
[`add_max_phylo_end_objective()`](https://prioritizr.net/reference/add_max_phylo_end_objective.md),
[`add_max_wtd_sum_objective()`](https://prioritizr.net/reference/add_max_wtd_sum_objective.md),
[`add_min_largest_shortfall_objective()`](https://prioritizr.net/reference/add_min_largest_shortfall_objective.md),
[`add_min_set_objective()`](https://prioritizr.net/reference/add_min_set_objective.md),
[`add_min_shortfall_objective()`](https://prioritizr.net/reference/add_min_shortfall_objective.md)

## Examples

``` r
# \dontrun{
# set seed for reproducibility
set.seed(500)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_features <- get_sim_zones_features()

# here we will show how the min penalties objective can be used
# to generate a solution that accounts for spatial fragmentation
# (via boundary penalties) using multi-objective optimization techniques

# create initial problem
# note that this does not consider boundary penalties
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s1 <- solve(p1)

# plot solution
plot(s1, main = "initial solution", axes = FALSE)


# create a multi-objective problem that contains the
# initial problem as well as an additional problem that is
# focused entirely on minimizing spatial fragmentation.
# additionally, this multi-objective problem will use the
# hierarchical approach for optimization and we will
# consider three rel_tol values to generate multiple solutions
# that represent different levels of trade-off between total cost
# and spatial fragmentation. note that we use a small penalty value
# in add_boundary_penalties() to avoid scaling issues and this
# has no influence on the trade-offs between cost and spatial fragmentation.
rel_tol <- c(0, 0.05, 0.1, 0.2)
mp <-
  multi_problem(
    obj1 = p1,
    obj2 =
      problem(sim_pu_raster, sim_features) %>%
     add_min_penalties_objective() %>%
     add_boundary_penalties(penalty = 0.1) %>%
     add_binary_decisions()
  ) %>%
  add_hier_approach(rel_tol = matrix(rel_tol, ncol = 1)) %>%
  add_default_solver(verbose = FALSE)
#> Warning: → `...` contains a `problem()` object that has a `?solvers` added to it.
#> ℹ To specify solver settings for multi-objective optimization, the solver
#>   should be added to the `multi_problem()` object.

# generate multi-objective solutions
s2 <- solve(mp)
#> ■■■■■■■■■                         25% | ETA:  3s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s

# plot multi-objective solutions
plot(terra::rast(s2), main = paste("rel_tol =", rel_tol), axes = FALSE)

# }
```
