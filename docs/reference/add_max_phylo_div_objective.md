# Add maximum phylogenetic diversity objective

Set the objective of a conservation planning problem to maximize the
phylogenetic diversity of the features represented in the solution
subject to a budget. This objective is similar to
[`add_max_features_objective()`](https://prioritizr.net/reference/add_max_features_objective.md)
except that emphasis is placed on representing a phylogenetically
diverse set of species, rather than as many features as possible
(subject to weights). This function was inspired by Faith (1992) and
Rodrigues *et al.* (2002).

## Usage

``` r
add_max_phylo_div_objective(x, budget, tree)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- budget:

  `numeric` value specifying the maximum expenditure of the
  prioritization. For problems with multiple zones, the argument to
  `budget` can be (i) a single `numeric` value to specify a single
  budget for the entire solution or (ii) a `numeric` vector to specify a
  separate budget for each management zone.

- tree:

  [`ape::phylo()`](https://rdrr.io/pkg/ape/man/read.tree.html) object
  specifying a phylogenetic tree for the conservation features.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the objective added to it.

## Details

The maximum phylogenetic diversity objective finds the set of planning
units that meets representation targets for a phylogenetic tree while
staying within a fixed budget. If multiple solutions can meet all
targets while staying within budget, the cheapest solution is chosen.
Note that this objective is similar to the maximum features objective
([`add_max_features_objective()`](https://prioritizr.net/reference/add_max_features_objective.md))
in that it allows for both a budget and targets to be set for each
feature. However, unlike the maximum feature objective, the aim of this
objective is to maximize the total phylogenetic diversity of the targets
met in the solution, so if multiple targets are provided for a single
feature, the problem will only need to meet a single target for that
feature for the phylogenetic benefit for that feature to be counted when
calculating the phylogenetic diversity of the solution. In other words,
for multi-zone problems, this objective does not aim to maximize the
phylogenetic diversity in each zone, but rather this objective aims to
maximize the phylogenetic diversity of targets that can be met through
allocating planning units to any of the different zones in a problem.
This can be useful for problems where targets pertain to the total
amount held for each feature across multiple zones. For example, each
feature might have a non-zero amount of suitable habitat in each
planning unit when the planning units are assigned to a (i) not
restored, (ii) partially restored, or (iii) completely restored
management zone. Here each target corresponds to a single feature and
can be met through the total amount of habitat in planning units present
to the three zones.

## Mathematical formulation

This objective can be expressed mathematically for a set of planning
units (\\I\\ indexed by \\i\\) and a set of features (\\J\\ indexed by
\\j\\) as:

\$\$\mathit{Maximize} \space \sum\_{i = 1}^{I} -s \space c_i \space
x_i + \sum\_{j = 1}^{J} m_b l_b \\ \mathit{subject \space to} \\
\sum\_{i = 1}^{I} x_i r\_{ij} \geq y_j t_j \forall j \in J \\ m_b \leq
y_j \forall j \in T(b) \\ \sum\_{i = 1}^{I} x_i c_i \leq B\$\$

Here, \\x_i\\ is the
[decisions](https://prioritizr.net/reference/decisions.md) variable
(e.g., specifying whether planning unit \\i\\ has been selected (1) or
not (0)), \\r\_{ij}\\ is the amount of feature \\j\\ in planning unit
\\i\\, \\t_j\\ is the representation target for feature \\j\\, \\y_j\\
indicates if the solution has meet the target \\t_j\\ for feature \\j\\.
Additionally, \\T\\ represents a phylogenetic tree containing features
\\j\\ and has the branches \\b\\ associated within lengths \\l_b\\. The
binary variable \\m_b\\ denotes if at least one feature associated with
the branch \\b\\ has met its representation as indicated by \\y_j\\. For
brevity, we denote the features \\j\\ associated with branch \\b\\ using
\\T(b)\\. Finally, \\B\\ is the budget allocated for the solution,
\\c_i\\ is the cost of planning unit \\i\\, and \\s\\ is a scaling
factor used to shrink the costs so that the problem will return a
cheapest solution when there are multiple solutions that represent the
same amount of all features within the budget.

## Notes

In early versions, this function was named as the
`add_max_phylo_div_objective` function.

## References

Faith DP (1992) Conservation evaluation and phylogenetic diversity.
*Biological Conservation*, 61: 1–10.

Rodrigues ASL and Gaston KJ (2002) Maximising phylogenetic diversity in
the selection of networks of conservation areas. *Biological
Conservation*, 105: 103–111.

## See also

See [objectives](https://prioritizr.net/reference/objectives.md) for an
overview of all functions for adding objectives. Also, see
[targets](https://prioritizr.net/reference/targets.md) for an overview
of all functions for adding targets, and
[`add_feature_weights()`](https://prioritizr.net/reference/add_feature_weights.md)
to specify weights for different features.

Other functions for adding objectives:
[`add_max_cover_objective()`](https://prioritizr.net/reference/add_max_cover_objective.md),
[`add_max_features_objective()`](https://prioritizr.net/reference/add_max_features_objective.md),
[`add_max_phylo_end_objective()`](https://prioritizr.net/reference/add_max_phylo_end_objective.md),
[`add_max_utility_objective()`](https://prioritizr.net/reference/add_max_utility_objective.md),
[`add_min_largest_shortfall_objective()`](https://prioritizr.net/reference/add_min_largest_shortfall_objective.md),
[`add_min_penalties_objective()`](https://prioritizr.net/reference/add_min_penalties_objective.md),
[`add_min_set_objective()`](https://prioritizr.net/reference/add_min_set_objective.md),
[`add_min_shortfall_objective()`](https://prioritizr.net/reference/add_min_shortfall_objective.md)

## Examples

``` r
# \dontrun{
# load ape package
require(ape)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()
sim_phylogeny <- get_sim_phylogeny()
sim_zones_pu_raster  <- get_sim_zones_pu_raster()
sim_zones_features  <- get_sim_zones_features()

# plot the simulated phylogeny
par(mfrow = c(1, 1))
plot(sim_phylogeny, main = "phylogeny")


# create problem with a maximum phylogenetic diversity objective,
# where each feature needs 10% of its distribution to be secured for
# it to be adequately conserved and a total budget of 1900
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_max_phylo_div_objective(1900, sim_phylogeny) %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s1 <- solve(p1)

# plot solution
plot(s1, main = "solution", axes = FALSE)


# find out which features have their targets met
r1 <- eval_target_coverage_summary(p1, s1)
print(r1, width = Inf)
#> # A tibble: 5 × 9
#>   feature   met   total_amount absolute_target absolute_held absolute_shortfall
#>   <chr>     <lgl>        <dbl>           <dbl>         <dbl>              <dbl>
#> 1 feature_1 FALSE         83.3            8.33          5.87               2.46
#> 2 feature_2 TRUE          31.2            3.12          3.18               0   
#> 3 feature_3 FALSE         72.0            7.20          4.39               2.80
#> 4 feature_4 TRUE          42.7            4.27          4.37               0   
#> 5 feature_5 FALSE         56.7            5.67          4.43               1.24
#>   relative_target relative_held relative_shortfall
#>             <dbl>         <dbl>              <dbl>
#> 1             0.1        0.0705              0.295
#> 2             0.1        0.102               0    
#> 3             0.1        0.0611              0.389
#> 4             0.1        0.102               0    
#> 5             0.1        0.0781              0.219

# plot the phylogeny and color the adequately represented features in red
plot(
  sim_phylogeny, main = "adequately represented features",
  tip.color = replace(
    rep("black", terra::nlyr(sim_features)),
    sim_phylogeny$tip.label %in% r1$feature[r1$met], "red"
  )
)


# rename the features in the example phylogeny for use with the
# multi-zone data
sim_phylogeny$tip.label <- feature_names(sim_zones_features)

# create targets for a multi-zone problem. Here, each feature needs a total
# of 10 units of habitat to be conserved among the three zones to be
# considered adequately conserved
targets <- tibble::tibble(
  feature = feature_names(sim_zones_features),
  zone = list(zone_names(sim_zones_features))[
    rep(1, number_of_features(sim_zones_features))],
  type = rep("absolute", number_of_features(sim_zones_features)),
  target = rep(10, number_of_features(sim_zones_features))
)

# create a multi-zone problem with a maximum phylogenetic diversity
# objective, where the total expenditure in all zones is 5000.
p2 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_max_phylo_div_objective(5000, sim_phylogeny) %>%
  add_manual_targets(targets) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s2 <- solve(p2)

# plot solution
plot(category_layer(s2), main = "solution", axes = FALSE)


# find out which features have their targets met
r2 <- eval_target_coverage_summary(p2, s2)
print(r2, width = Inf)
#> # A tibble: 5 × 11
#>   feature   zone      sense met   total_amount absolute_target absolute_held
#>   <chr>     <list>    <chr> <lgl>        <dbl>           <dbl>         <dbl>
#> 1 feature_1 <chr [3]> >=    TRUE         250.               10         17.6 
#> 2 feature_2 <chr [3]> >=    FALSE         93.6              10          6.66
#> 3 feature_3 <chr [3]> >=    TRUE         216.               10         14.0 
#> 4 feature_4 <chr [3]> >=    TRUE         128.               10         10.0 
#> 5 feature_5 <chr [3]> >=    TRUE         170.               10         12.4 
#>   absolute_shortfall relative_target relative_held relative_shortfall
#>                <dbl>           <dbl>         <dbl>              <dbl>
#> 1               0             0.0400        0.0706              0    
#> 2               3.34          0.107         0.0712              0.334
#> 3               0             0.0463        0.0646              0    
#> 4               0             0.0781        0.0784              0    
#> 5               0             0.0588        0.0731              0    

# plot the phylogeny and color the adequately represented features in red
plot(
  sim_phylogeny, main = "adequately represented features",
  tip.color = replace(
    rep("black", terra::nlyr(sim_features)), which(r2$met), "red"
  )
)


# create a multi-zone problem with a maximum phylogenetic diversity
# objective, where each zone has a separate budget.
p3 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_max_phylo_div_objective(c(2500, 500, 2000), sim_phylogeny) %>%
  add_manual_targets(targets) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s3 <- solve(p3)

# plot solution
plot(category_layer(s3), main = "solution", axes = FALSE)


# find out which features have their targets met
r3 <- eval_target_coverage_summary(p3, s3)
print(r3, width = Inf)
#> # A tibble: 5 × 11
#>   feature   zone      sense met   total_amount absolute_target absolute_held
#>   <chr>     <list>    <chr> <lgl>        <dbl>           <dbl>         <dbl>
#> 1 feature_1 <chr [3]> >=    TRUE         250.               10         16.2 
#> 2 feature_2 <chr [3]> >=    FALSE         93.6              10          7.64
#> 3 feature_3 <chr [3]> >=    TRUE         216.               10         13.6 
#> 4 feature_4 <chr [3]> >=    TRUE         128.               10         10.7 
#> 5 feature_5 <chr [3]> >=    TRUE         170.               10         11.2 
#>   absolute_shortfall relative_target relative_held relative_shortfall
#>                <dbl>           <dbl>         <dbl>              <dbl>
#> 1               0             0.0400        0.0649              0    
#> 2               2.36          0.107         0.0816              0.236
#> 3               0             0.0463        0.0631              0    
#> 4               0             0.0781        0.0832              0    
#> 5               0             0.0588        0.0658              0    

# plot the phylogeny and color the adequately represented features in red
plot(
  sim_phylogeny, main = "adequately represented features",
  tip.color = replace(
    rep("black", terra::nlyr(sim_features)), which(r3$met), "red"
  )
)

# }
```
