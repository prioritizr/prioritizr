# Add feature weights

Add features weights to a conservation planning problem. Specifically,
some objective functions aim to maximize (or minimize) a metric that
measures how well a set of features are represented by a solution (e.g.,
maximize the number of features that are adequately represented,
[`add_max_features_objective()`](https://prioritizr.net/reference/add_max_features_objective.md)).
In such cases, it may be desirable to prefer the representation of some
features over other features (e.g., features that have higher extinction
risk might be considered more important than those with lower extinction
risk). To achieve this, weights can be used to specify how much more
important it is for a solution to represent particular features compared
with other features.

## Usage

``` r
# S4 method for class 'ConservationProblem,numeric'
add_feature_weights(x, weights)

# S4 method for class 'ConservationProblem,matrix'
add_feature_weights(x, weights)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- weights:

  `numeric` or `matrix` of weights. See the Weights format section for
  more information.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
with the weights added to it.

## Details

Weights can only be applied to problems that have an objective that is
budget limited (e.g.,
[`add_max_cover_objective()`](https://prioritizr.net/reference/add_max_cover_objective.md),
[`add_min_shortfall_objective()`](https://prioritizr.net/reference/add_min_shortfall_objective.md)).
They can also be applied to problems that aim to maximize phylogenetic
representation
([`add_max_phylo_div_objective()`](https://prioritizr.net/reference/add_max_phylo_div_objective.md))
to favor the representation of specific features over the representation
of some phylogenetic branches. Weights cannot be negative values and
must have values that are equal to or larger than zero. **Note that
planning unit costs are scaled to 0.01 to identify the cheapest solution
among multiple optimal solutions. This means that the optimization
process will favor cheaper solutions over solutions that meet feature
targets (or occurrences) when feature weights are lower than 0.01.**

## Weights format

The argument to `weights` can be specified using the following formats.

- `weights` as a `numeric` vector:

  containing weights for each feature. Note that this format cannot be
  used to specify weights for problems with multiple zones.

- `weights` as a `matrix` object:

  containing weights for each feature in each zone. Here, each row
  corresponds to a different feature in argument to `x`, each column
  corresponds to a different zone in argument to `x`, and each cell
  contains the weight value for a given feature that the solution can to
  secure in a given zone. Note that if the problem contains targets
  created using
  [`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)
  then a `matrix` should be supplied containing a single column that
  indicates that weight for fulfilling each target.

## See also

See [penalties](https://prioritizr.net/reference/penalties.md) for an
overview of all functions for adding penalties.

Other functions for adding penalties:
[`add_asym_connectivity_penalties()`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md),
[`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md),
[`add_connectivity_penalties()`](https://prioritizr.net/reference/add_connectivity_penalties.md),
[`add_linear_penalties()`](https://prioritizr.net/reference/add_linear_penalties.md),
[`add_neighbor_penalties()`](https://prioritizr.net/reference/add_neighbor_penalties.md)

## Examples

``` r
# \dontrun{
# load package
require(ape)
#> Loading required package: ape

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()
sim_phylogeny <- get_sim_phylogeny()
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_features <- get_sim_zones_features()

# create minimal problem that aims to maximize the number of features
# adequately conserved given a total budget of 3800. Here, each feature
# needs 20% of its habitat for it to be considered adequately conserved
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_max_features_objective(budget = 3800) %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create weights that assign higher importance to features with less
# suitable habitat in the study area
w2 <- exp((1 / terra::global(sim_features, "sum", na.rm = TRUE)[[1]]) * 200)

# create problem using rarity weights
p2 <- p1 %>% add_feature_weights(w2)

# create manually specified weights that assign higher importance to
# certain features. These weights could be based on a pre-calculated index
# (e.g., an index measuring extinction risk where higher values
# denote higher extinction risk)
w3 <- c(0, 0, 0, 100, 200)
p3 <- p1 %>% add_feature_weights(w3)

# solve problems
s1 <- c(solve(p1), solve(p2), solve(p3))
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0.001 0.014
names(s1) <- c("equal weights", "rarity weights", "manual weights")
#> Error: object 's1' not found

# plot solutions
plot(s1, axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's1' not found

# plot the example phylogeny
par(mfrow = c(1, 1))
plot(sim_phylogeny, main = "simulated phylogeny")


# create problem with a maximum phylogenetic diversity objective,
# where each feature needs 10% of its distribution to be secured for
# it to be adequately conserved and a total budget of 1900
p4 <-
  problem(sim_pu_raster, sim_features) %>%
  add_max_phylo_div_objective(1900, sim_phylogeny) %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s4 <- solve(p4)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0 0.012

# plot solution
plot(s4, main = "solution", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's4' not found

# find out which features have their targets met
r4 <- eval_target_coverage_summary(p4, s4)
#> Error in eval_target_coverage_summary(p4, s4): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's4' not found
print(r4, width = Inf)
#> Error: object 'r4' not found

# plot the example phylogeny and color the represented features in red
plot(
  sim_phylogeny, main = "represented features",
  tip.color = replace(
    rep("black", terra::nlyr(sim_features)), which(r4$met), "red"
  )
)

#> Error: object 'r4' not found

# we can see here that the third feature ("layer.3", i.e.,
# sim_features[[3]]) is not represented in the solution. Let us pretend
# that it is absolutely critical this feature is adequately conserved
# in the solution. For example, this feature could represent a species
# that plays important role in the ecosystem, or a species that is
# important commercial activities (e.g., eco-tourism). So, to generate
# a solution that conserves the third feature whilst also aiming to
# maximize phylogenetic diversity, we will create a set of weights that
# assign a particularly high weighting to the third feature
w5 <- c(0, 0, 10000, 0, 0)

# we can see that this weighting (i.e., w5[3]) has a much higher value than
# the branch lengths in the phylogeny so solutions that represent this
# feature be much closer to optimality
print(sim_phylogeny$edge.length)
#> [1] 1.42105185 0.34712544 0.04100098 0.30612447 0.30612447 0.59604770 1.17212960
#> [8] 1.17212960

# create problem with high weighting for the third feature and solve it
s5 <- p4 %>% add_feature_weights(w5) %>% solve()
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013

# plot solution
plot(s5, main = "solution", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's5' not found

# find which features have their targets met
r5 <- eval_target_coverage_summary(p4, s5)
#> Error in eval_target_coverage_summary(p4, s5): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's5' not found
print(r5, width = Inf)
#> Error: object 'r5' not found

# plot the example phylogeny and color the represented features in red
# here we can see that this solution only adequately conserves the
# third feature. This means that, given the budget, we are faced with the
# trade-off of conserving either the third feature, or a phylogenetically
# diverse set of three different features.
plot(
  sim_phylogeny, main = "represented features",
  tip.color = replace(
    rep("black", terra::nlyr(sim_features)), which(r5$met), "red"
  )
)
#> Error: object 'r5' not found

# create multi-zone problem with maximum features objective,
# with 10% representation targets for each feature, and set
# a budget such that the total maximum expenditure in all zones
# cannot exceed 3000
p6 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_max_features_objective(3000) %>%
  add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create weights that assign equal weighting for the representation
# of each feature in each zone except that it does not matter if
# feature 1 is represented in zone 1 and it really important
# that feature 3 is really in zone 1
w7 <- matrix(1, ncol = 3, nrow = 5)
w7[1, 1] <- 0
w7[3, 1] <- 100

# create problem with weights
p7 <- p6 %>% add_feature_weights(w7)

# solve problems
s6 <- solve(p6)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0 0.013
s7 <- solve(p7)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013

# convert solutions to category layers
c6 <- category_layer(s6)
#> Error in category_layer(s6): ℹ In argument to `x`.
#> Caused by error:
#> ! object 's6' not found
c7 <- category_layer(s7)
#> Error in category_layer(s7): ℹ In argument to `x`.
#> Caused by error:
#> ! object 's7' not found

# plot solutions
plot(c(c6, c7), main = c("equal weights", "manual weights"), axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 'c6' not found

# create minimal problem to show the correct method for setting
# weights for problems with manual targets
p8 <-
  problem(sim_pu_raster, sim_features) %>%
  add_max_features_objective(budget = 3000) %>%
  add_manual_targets(
    data.frame(
    feature = c("feature_1", "feature_4"),
    type = "relative",
    target = 0.1)
  ) %>%
  add_feature_weights(matrix(c(1, 200), ncol = 1)) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s8 <- solve(p8)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013

# plot solution
plot(s8, main = "solution", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's8' not found
# }
```
