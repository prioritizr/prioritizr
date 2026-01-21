# Add relative targets

Add targets to a conservation planning problem expressed as a proportion
(between 0 and 1) of the maximum level of representation of each feature
in the study area. Please note that proportions are scaled according to
the features' total abundances in the study area (including any locked
out planning units, or planning units with `NA` cost values) using the
[`feature_abundances()`](https://prioritizr.net/reference/feature_abundances.md)
function.

## Usage

``` r
add_relative_targets(x, targets)

# S4 method for class 'ConservationProblem,numeric'
add_relative_targets(x, targets)

# S4 method for class 'ConservationProblem,matrix'
add_relative_targets(x, targets)

# S4 method for class 'ConservationProblem,character'
add_relative_targets(x, targets)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- targets:

  Object that specifies the targets for each feature. See the Targets
  format section for more information.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the targets added to it.

## Details

This function is used to set targets for each feature (separately). For
problems associated with a single management zone, this function may be
useful to specify individual targets for each feature. For problems
associated with multiple management zones, this function can also be
used to specify a target for each feature within each zone (separately).
For example, this may be useful in planning exercises where it is
important to ensure that some of the features are adequately represented
by multiple zones. For example, in a marine spatial planning exercise,
it may be important for some features (e.g., commercial important fish
species) to be adequately represented by a conservation zone for
ensuring their long-term persistence, and also by a fishing zone to for
ensure food security. For greater flexibility in target setting (such as
setting targets that can be met through the allocation of multiple
zones), see the
[`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)
function.

## Target setting

Many conservation planning problems require targets. Targets are used to
specify the minimum amount, or proportion, of a feature's spatial
distribution that should ideally be protected. This is important so that
the optimization process can weigh the merits and trade-offs between
improving the representation of one feature over another feature.
Although it can be challenging to set meaningful targets, this is a
critical step for ensuring that prioritizations meet the stakeholder
objectives that underpin a prioritization exercise (Carwardine *et al.*
2009). In other words, targets play an important role in ensuring that a
priority setting process is properly tuned according to stakeholder
requirements. For example, targets provide a mechanism for ensuring that
a prioritization secures enough habitat to promote the long-term
persistence of each threatened species, culturally important species, or
economically important ecosystem services under consideration. Since
there is often uncertainty regarding stakeholder objectives (e.g., how
much habitat should be protected for a given species) or the influence
of particular target on a prioritization (e.g., how would setting a 90%
or 100% for a threatened species alter priorities), it is often useful
to generate and compare a suite of prioritizations based on different
target scenarios.

## Targets format

The `targets` for a problem can be specified using the following
formats.

- `targets` as a `numeric` vector:

  containing target values for each feature. Additionally, for
  convenience, this format can be a single value to assign the same
  target to each feature. Note that this format cannot be used to
  specify targets for problems with multiple zones.

- `targets` as a `matrix` object:

  containing a target for each feature in each zone. Here, each row
  corresponds to a different feature in argument to `x`, each column
  corresponds to a different zone in argument to `x`, and each cell
  contains the target value for a given feature that the solution needs
  to secure in a given zone.

- `targets` as a `character` vector:

  containing the column name(s) in the feature data associated with the
  argument to `x` that contain targets. This format can only be used
  when the feature data associated with `x` is a
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) or
  `data.frame`. For problems that contain a single zone, the argument to
  `targets` must contain a single column name. Otherwise, for problems
  that contain multiple zones, the argument to `targets` must contain a
  column name for each zone.

## References

Carwardine J, Klein CJ, Wilson KA, Pressey RL, Possingham HP (2009)
Hitting the target and missing the point: target‐based conservation
planning in context. *Conservation Letters*, 2: 4–11.

## See also

Other functions for adding targets:
[`add_absolute_targets()`](https://prioritizr.net/reference/add_absolute_targets.md),
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md),
[`add_group_targets()`](https://prioritizr.net/reference/add_group_targets.md),
[`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)

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

# create base problem
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create problem with 10% targets
p1 <- p %>% add_relative_targets(0.1)

# create problem with varying targets for each feature
targets <- c(0.1, 0.2, 0.3, 0.4, 0.5)
p2 <- p %>% add_relative_targets(targets)

# solve problem
s3 <- c(solve(p1), solve(p2))
names(s3) <- c("10% targets", "varying targets")

# plot solution
plot(s3, axes = FALSE)


# create a problem with multiple management zones
p4 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create a problem with targets that specify an equal amount of each feature
# to be represented in each zone
p4_targets <- matrix(
  0.1, nrow = 5, ncol = 3, dimnames = list(
    feature_names(sim_zones_features), zone_names(sim_zones_features)
  )
)
print(p4_targets)
#>           zone_1 zone_2 zone_3
#> feature_1    0.1    0.1    0.1
#> feature_2    0.1    0.1    0.1
#> feature_3    0.1    0.1    0.1
#> feature_4    0.1    0.1    0.1
#> feature_5    0.1    0.1    0.1

p5 <- p4 %>% add_relative_targets(p4_targets)

# solve problem
s5 <- solve(p5)

# plot solution (cell values correspond to zone identifiers)
plot(category_layer(s5), main = "equal targets")


# create a problem with targets that require a varying amount of each
# feature to be represented in each zone
p6_targets <- matrix(
  runif(15, 0.01, 0.2), nrow = 5, ncol = 3, dimnames = list(
    feature_names(sim_zones_features), zone_names(sim_zones_features)
  )
)
print(p6_targets)
#>               zone_1     zone_2     zone_3
#> feature_1 0.16838399 0.04908221 0.06359158
#> feature_2 0.14775224 0.10731456 0.17964011
#> feature_3 0.19530969 0.18583855 0.15529418
#> feature_4 0.09884473 0.16747797 0.04122594
#> feature_5 0.16433284 0.14519964 0.14909414

p6 <- p4 %>% add_relative_targets(p6_targets)

# solve problem
s6 <- solve(p6)

# plot solution (cell values correspond to zone identifiers)
plot(category_layer(s6), main = "varying targets")

# }
```
