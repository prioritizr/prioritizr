# Create objective weights

Create multiple sets of weight values to generate multiple solutions
with the weighted sum approach for multi-objective optimization (i.e.,
the `weights` parameter of
[`add_wtd_sum_approach()`](https://prioritizr.net/reference/add_wtd_sum_approach.md)).

## Usage

``` r
objective_weights_matrix(
  n_objectives,
  n_per_objective,
  include_zeros = TRUE,
  include_extremes = TRUE
)
```

## Arguments

- n_objectives:

  `integer` number of objectives.

- n_per_objective:

  `integer` number of weight values to to generate for each objective.

- include_zeros:

  `logical` value indicating if the weight values should include zeros?
  If `include_zeros = TRUE`, then some of the sets will assign a weight
  of zero to some of the objectives, and so solutions based on these
  sets will be influenced by only some of the objectives (i.e., those
  with non-zero weight values). Defaults to `TRUE`.

- include_extremes:

  `logical` value indicating if the sets of weight values should
  combinations of weight values that consider only a single objective?
  If `include_extremes = TRUE`, then some of the sets will contain zeros
  for all objectives except a single objective. Defaults to `TRUE`.

## Value

A `numeric` matrix with weight values. Here, rows correspond to
different sets of each weight values and columns correspond to different
objectives.

## Examples

``` r
# \dontrun{
# in this example, we aim to identify a set of planning units that will
# not exceed a particular budget and meet objectives for
# (i) representing species that are important for ecosystem
# functioning (hereafter, keystone species) and (ii) representing species
# that have high social or cultural value (hereafter, iconic species)

# import data
con_cost <- get_sim_pu_raster()
keystone_spp <- get_sim_features()[[1:3]]
iconic_spp <- get_sim_features()[[4:5]]

# define a total conservation budget (30% of total cost)
budget <- terra::global(con_cost, "sum", na.rm = TRUE)[[1]] * 0.3

# define a single-objective problem for the keystone species objective
p1 <-
  problem(con_cost, keystone_spp) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.4) %>%
  add_binary_decisions()

# define a single-objective problem for the iconic species objective
p2 <-
  problem(con_cost, iconic_spp) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.45) %>%
  add_binary_decisions()

# solve the single-objective problems
s1 <-
  p1 %>%
  add_default_solver(verbose = FALSE) %>%
  solve()
s2 <-
  p2 %>%
  add_default_solver(verbose = FALSE) %>%
  solve()

# plot the solutions to the single-objective problems
plot(s1, main = "Keystone species", axes = FALSE)

plot(s2, main = "Iconic species", axes = FALSE)


# now create multi-objective problem with equal weights for the objectives
mp1 <-
  multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
  add_wtd_sum_approach(c(0.5, 0.5), verbose = TRUE) %>%
  add_default_solver(verbose = FALSE)

# solve problem
ms1 <- solve(mp1)

# plot solution to multi-objective problem
plot(ms1, main = "Equal weights", axes = FALSE)


# we will now generate multiple solutions based on a matrix
# that contains different combinations of weight values

# create a matrix with weight values for objectives
obj_weights_matrix <- objective_weights_matrix(
 n_objectives = 2,
 n_per_objective = 5,
 include_zero = TRUE
)

# preview weight matrix
head(obj_weights_matrix)
#>      Var1 Var2
#> [1,]  1.0  1.0
#> [2,]  1.0  0.0
#> [3,]  0.0  1.0
#> [4,]  0.4  0.2
#> [5,]  0.6  0.2
#> [6,]  0.8  0.2

# create multi-objective problem using weight matrix
mp2 <-
  multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
  add_wtd_sum_approach(obj_weights_matrix, verbose = FALSE) %>%
  add_default_solver(verbose = FALSE)

# solve multi-objective problem and generate multiple solutions
ms2 <- solve(mp2)

# extract objective values for the solutions
obj_matrix <- attributes(ms2)$objective

# preview the objective values
head(obj_matrix)
#>            keystone_obj iconic_obj
#> solution_1    0.9421096  0.7490599
#> solution_2    0.8656717  2.0000000
#> solution_3    3.0000000  0.6072101
#> solution_4    0.9421096  0.7490599
#> solution_5    0.9421096  0.7490599
#> solution_6    0.9421096  0.7490599

# plot the objectives values to visualize the approximated Pareto frontier
# (note that smaller values are better because these objectives seek to
# minimize representation shortfalls)
plot(
  obj_matrix,
  main = "Approximated Pareto frontier",
  xlab = "Keystone objective (shortfall)",
  ylab = "Iconic objective (shortfall)"
)


# we can see that there are multiple solutions (points) that have
# exactly the same performance for the two objectives (these appear
# as points with slightly thicker borders), and this is a key limitation
# of the weighted sum approach
# }
```
