# Add minimum set objective

Set the objective of a conservation planning problem to minimize the
cost of the solution whilst ensuring that all
[targets](https://prioritizr.net/reference/targets.md) are met. This
objective is similar to that used in *Marxan* and is detailed in
Rodrigues *et al.* (2000).

## Usage

``` r
add_min_set_objective(x)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the objective added to it.

## Details

The minimum set objective – in the the context of systematic reserve
design – seeks to find the set of planning units that minimizes the
overall cost of a reserve network, while meeting a set of representation
targets for the conservation features. This objective is equivalent to a
simplified *Marxan* reserve design problem with the Boundary Length
Modifier (BLM) set to zero. The difference between this objective and
the *Marxan* software is that the targets for the features will always
be met (and as such it does not use Species Penalty Factors).

## Mathematical formulation

This objective can be expressed mathematically for a set of planning
units (\\I\\ indexed by \\i\\) and a set of features (\\J\\ indexed by
\\j\\) as:

\$\$\mathit{Minimize} \space \sum\_{i = 1}^{I} x_i c_i \\
\mathit{subject \space to} \\ \sum\_{i = 1}^{I} x_i r\_{ij} \geq T_j
\space \forall \space j \in J\$\$

Here, \\x_i\\ is the
[decisions](https://prioritizr.net/reference/decisions.md) variable
(e.g., specifying whether planning unit \\i\\ has been selected (1) or
not (0)), \\c_i\\ is the cost of planning unit \\i\\, \\r\_{ij}\\ is the
amount of feature \\j\\ in planning unit \\i\\, and \\T_j\\ is the
target for feature \\j\\. The first term is the objective function and
the second is the set of constraints. In words this says find the set of
planning units that meets all the representation targets while
minimizing the overall cost.

## References

Rodrigues AS, Cerdeira OJ, and Gaston KJ (2000) Flexibility, efficiency,
and accountability: adapting reserve selection algorithms to more
complex conservation problems. *Ecography*, 23: 565–574.

## See also

See [objectives](https://prioritizr.net/reference/objectives.md) for an
overview of all functions for adding objectives. Also see
[targets](https://prioritizr.net/reference/targets.md) for an overview
of all functions for adding targets.

Other functions for adding objectives:
[`add_max_cover_objective()`](https://prioritizr.net/reference/add_max_cover_objective.md),
[`add_max_n_targets_met_objective()`](https://prioritizr.net/reference/add_max_n_targets_met_objective.md),
[`add_max_phylo_div_objective()`](https://prioritizr.net/reference/add_max_phylo_div_objective.md),
[`add_max_phylo_end_objective()`](https://prioritizr.net/reference/add_max_phylo_end_objective.md),
[`add_max_wtd_sum_objective()`](https://prioritizr.net/reference/add_max_wtd_sum_objective.md),
[`add_min_largest_shortfall_objective()`](https://prioritizr.net/reference/add_min_largest_shortfall_objective.md),
[`add_min_penalties_objective()`](https://prioritizr.net/reference/add_min_penalties_objective.md),
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

# create minimal problem with minimum set objective
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s1 <- solve(p1)

# plot solution
plot(s1, main = "solution", axes = FALSE)


# create multi-zone problem with minimum set objective
targets_matrix <- matrix(rpois(15, 1), nrow = 5, ncol = 3)

p2 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_absolute_targets(targets_matrix) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s2 <- solve(p2)

# plot solution
plot(category_layer(s2), main = "solution", axes = FALSE)

# }
```
