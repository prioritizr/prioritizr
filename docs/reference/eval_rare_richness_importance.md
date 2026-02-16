# Evaluate solution importance using rarity weighted richness scores

Calculate importance scores for planning units selected in a solution
using rarity weighted richness scores (based on Williams *et al.* 1996).

## Usage

``` r
eval_rare_richness_importance(x, solution, rescale = TRUE)
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

## Value

A `numeric`, `matrix`, `data.frame`,
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
or [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
containing the importance scores for each planning unit in the solution.
Specifically, the returned object is in the same format as the planning
unit data in the argument to `x`.

## Details

Rarity weighted richness scores are calculated using the following
terms. Let \\I\\ denote the set of planning units (indexed by \\i\\),
let \\J\\ denote the set of conservation features (indexed by \\j\\),
let \\r\_{ij}\\ denote the amount of feature \\j\\ associated with
planning unit \\i\\, and let \\m_j\\ denote the maximum value of feature
\\j\\ in \\r\_{ij}\\ in all planning units \\i \in I\\. To calculate the
rarity weighted richness (*RWR*) for planning unit \\k\\:

\$\$ \mathit{RWR}\_{k} = \sum\_{j}^{J} \frac{ \frac{r\_{ik}}{m_j} }{
\sum\_{i}^{I}r\_{ij}} \$\$

This method is only recommended for large-scaled conservation planning
exercises (i.e., more than 100,000 planning units) where importance
scores cannot be calculated using other methods in a feasible period of
time. This is because rarity weighted richness scores cannot (i) account
for the cost of different planning units, (ii) account for multiple
management zones, and (iii) identify truly irreplaceable planning units
— unlike the replacement cost metric which does not suffer any of these
limitations.

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

Williams P, Gibbons D, Margules C, Rebelo A, Humphries C, and Pressey RL
(1996) A comparison of richness hotspots, rarity hotspots and
complementary areas for conserving diversity using British birds.
*Conservation Biology*, 10: 155–174.

## See also

See [importance](https://prioritizr.net/reference/importance.md) for an
overview of all functions for evaluating the importance of planning
units selected in a solution.

Other functions for evaluating solution importance:
[`eval_ferrier_importance()`](https://prioritizr.net/reference/eval_ferrier_importance.md),
[`eval_rank_importance()`](https://prioritizr.net/reference/eval_rank_importance.md),
[`eval_replacement_importance()`](https://prioritizr.net/reference/eval_replacement_importance.md)

## Examples

``` r
# \dontrun{
# seed seed for reproducibility
set.seed(600)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_pu_polygons <- get_sim_pu_polygons()
sim_features <- get_sim_features()

# create minimal problem with raster planning units
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
rwr1 <- eval_rare_richness_importance(p1, s1)
#> Error in eval_rare_richness_importance(p1, s1): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's1' not found

# print importance scores
print(rwr1)
#> Error: object 'rwr1' not found

# plot importance scores
plot(rwr1, main = "rarity weighted richness", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 'rwr1' not found

# create minimal problem with polygon planning units
p2 <-
  problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.05) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve problem
s2 <- solve(p2)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.014

# print solution
print(s2)
#> Error: object 's2' not found

# plot solution
plot(s2[, "solution_1"], main = "solution")
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's2' not found

# calculate importance scores
rwr2 <- eval_rare_richness_importance(p2, s2[, "solution_1"])
#> Error in eval_rare_richness_importance(p2, s2[, "solution_1"]): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's2' not found

# plot importance scores
plot(rwr2, main = "rarity weighted richness")
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 'rwr2' not found
# }
```
