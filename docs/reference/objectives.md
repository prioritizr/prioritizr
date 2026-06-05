# Add an objective

An objective is used to specify the overall goal of a conservation
planning problem. All conservation planning problems involve minimizing
or maximizing some kind of objective. For instance, the planner may
require a solution that conserves enough habitat for each species while
minimizing the overall cost of the reserve network. Alternatively, the
planner may require a solution that maximizes the number of conserved
species while ensuring that the cost of the reserve network does not
exceed the budget. Please note that all conservation planning problems
require an objective function, and attempting to solve a problem without
an objective will result in an error.

## Details

The following functions can be used to add an objective to a
conservation planning
[`problem()`](https://prioritizr.net/reference/problem.md). Note that if
multiple of these functions are added to a
[`problem()`](https://prioritizr.net/reference/problem.md), then only
the last function added will be used.

- [`add_min_set_objective()`](https://prioritizr.net/reference/add_min_set_objective.md):

  Minimize the cost of the solution whilst ensuring that all targets are
  met. This objective is similar to that used in *Marxan*.

- [`add_min_shortfall_objective()`](https://prioritizr.net/reference/add_min_shortfall_objective.md):

  Minimize the overall (weighted sum) shortfall for as many targets as
  possible while ensuring that the cost of the solution does not exceed
  a budget.

- [`add_min_penalties_objective()`](https://prioritizr.net/reference/add_min_penalties_objective.md):

  Minimize the penalties associated with a problem as much as possible
  subject to a budget. This is mainly used when performing hierarchical
  multi-objective optimization.

- [`add_min_largest_shortfall_objective()`](https://prioritizr.net/reference/add_min_largest_shortfall_objective.md):

  Minimize the largest (maximum) shortfall among all targets while
  ensuring that the cost of the solution does not exceed a budget.

- [`add_max_phylo_div_objective()`](https://prioritizr.net/reference/add_max_phylo_div_objective.md):

  Maximize the phylogenetic diversity of the features represented in the
  solution subject to a budget.

- [`add_max_phylo_end_objective()`](https://prioritizr.net/reference/add_max_phylo_end_objective.md):

  Maximize the phylogenetic endemism of the features represented in the
  solution subject to a budget.

- [`add_max_n_targets_met_objective()`](https://prioritizr.net/reference/add_max_n_targets_met_objective.md):

  Maximize the number of feature targets that are fully met, while
  ensuring that the cost of the solution does not exceed a budget. Note
  that this objective does not value the partial fulfillment of targets.

- [`add_max_cover_objective()`](https://prioritizr.net/reference/add_max_cover_objective.md):

  Represent at least one instance of as many features as possible within
  a given budget. Note that this objective is not compatible with
  targets.

- [`add_max_wtd_sum_objective()`](https://prioritizr.net/reference/add_max_wtd_sum_objective.md):

  Maximize the weighted sum of the features represented by the solution
  subject to a budget. Note that this objective is not compatible with
  targets. Although there are a few cases where this objective may be
  suitable, we strongly advise against using this objective in general.

## Recommended practices

In general, we recommend using either the minimum set
[`add_min_set_objective()`](https://prioritizr.net/reference/add_min_set_objective.md)
or the minimum shortfall
[`add_min_shortfall_objective()`](https://prioritizr.net/reference/add_min_shortfall_objective.md)
objectives for conservation planning This is because both of these
objectives account for complementarity—a foundational concept in
systematic conservation planning (Kirkpatrick 1983). If solutions need
to conform to a type of budget (e.g., maximum expenditure, or maximum
amount of land that can be protected), then the minimum shortfall
objective is typically most appropriate. Otherwise, if solutions do not
need to conform to a particular budget, then the minimum set objective
is typically most appropriate. We strongly caution against using the
maximum weighted sum objective
([`add_max_wtd_sum_objective()`](https://prioritizr.net/reference/add_max_wtd_sum_objective.md)
because – except under very specific conditions – it has "repeatedly
been shown to identify priorities that are biologically ineffective and
economically inefficient" (Brown *et al.* 2015).

## References

Brown CJ, Bode M, Venter O, Barnes MD, McGowan J, Runge CA, Watson JEM,
and Possingham HP (2015) Effective conservation requires clear
objectives and prioritizing actions, not places or species. *Proceedings
of the National Academy of Sciences* 112: E4342.

Kirkpatrick JB (1983) An iterative method for establishing priorities
for the selection of nature reserves: An example from Tasmania.
*Biological Conservation*, 25: 127–134.

## See also

Other overviews:
[`approaches`](https://prioritizr.net/reference/approaches.md),
[`constraints`](https://prioritizr.net/reference/constraints.md),
[`decisions`](https://prioritizr.net/reference/decisions.md),
[`importance`](https://prioritizr.net/reference/importance.md),
[`penalties`](https://prioritizr.net/reference/penalties.md),
[`portfolios`](https://prioritizr.net/reference/portfolios.md),
[`solvers`](https://prioritizr.net/reference/solvers.md),
[`summaries`](https://prioritizr.net/reference/summaries.md),
[`targets`](https://prioritizr.net/reference/targets.md)

## Examples

``` r
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()
sim_phylogeny <- get_sim_phylogeny()

# create base problem
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

 # create problem with added minimum set objective
p1 <- p %>% add_min_set_objective()

# create problem with added maximum coverage objective
# note that this objective does not use targets
p2 <- p %>% add_max_cover_objective(500)

# create problem with added maximum number of targets met objective
p3 <- p %>% add_max_n_targets_met_objective(1900)

# create problem with added minimum shortfall objective
p4 <- p %>% add_min_shortfall_objective(1900)

# create problem with added minimum largest shortfall objective
p5 <- p %>% add_min_largest_shortfall_objective(1900)

# create problem with added maximum phylogenetic diversity objective
p6 <- p %>% add_max_phylo_div_objective(1900, sim_phylogeny)

# create problem with added maximum phylogenetic diversity objective
p7 <- p %>% add_max_phylo_end_objective(1900, sim_phylogeny)

# create problem with added maximum weighted sum objective
# note that this objective does not use targets
p8 <- p %>% add_max_wtd_sum_objective(1900)
#> ℹ `add_max_wtd_sum_objective()` has severe limitations - use with caution.

# solve problems
s <- c(
  solve(p1), solve(p2), solve(p3), solve(p4), solve(p5), solve(p6),
  solve(p7), solve(p8)
)
#> Warning: `problem()` has an objective that does not support targets.
#> ℹ The specified targets will be ignored during optimization.
#> ℹ If the targets are important, then use a different objective.
#> Warning: `problem()` has an objective that does not support targets.
#> ℹ The specified targets will be ignored during optimization.
#> ℹ If the targets are important, then use a different objective.
names(s) <- c(
  "min set", "max coverage", "max n targets met", "min shortfall",
  "min largest shortfall", "max phylogenetic diversity",
  "max phylogenetic endemism", "max wtd sum"
)

# plot solutions
plot(s, axes = FALSE)
```
