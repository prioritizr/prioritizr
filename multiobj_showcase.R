###### Hierarchical Approach ######

# import data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

##### Minimum set with minimum shortfall #####

# create two example problems
p1 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions()

p2 <- problem(sim_pu_raster, sim_features) %>%
  add_min_shortfall_objective(
    budget = 0.2 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
  ) %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions()

#### Using an input vector

# define relative tolerance (high degradation)
rel_tol <- 0.9

# create multi-objective problem
multi_p1 <- multi_problem(p1, p2) %>% # under the hood takes these two problems and merges them strategically together
  add_hierarchical_approach(rel_tol = rel_tol, verbose = FALSE) %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) # uses special internal gurobi functionality

# solve problem
multi_s1 <- solve(multi_p1)

# plot solutions for problems associated with spatial data
plot(multi_s1, main = "High degradation", axes = FALSE)

# define relative tolerance (low degradation)
rel_tol <- 0.1

# create multi-objective problem
multi_p2 <- multi_problem(p1, p2) %>%
  add_hierarchical_approach(rel_tol = rel_tol, verbose = FALSE) %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# solve problem
multi_s2 <- solve(multi_p2)

# plot solutions for problems associated with spatial data
plot(multi_s2, main = "Low degradation", axes = FALSE)

#### Using an input matrix
rel_tol_mat <- matrix(c(0.9, 0.1), nrow = 2, ncol = 1)

# create multi-objective problem
multi_p3 <- multi_problem(
  obj1 = problem(sim_pu_raster, sim_features) %>% # can also name objectives
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions(),
  obj2 = problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(
      budget = 0.2 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
    ) %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions()
) %>%
  add_hierarchical_approach(rel_tol = rel_tol_mat, verbose = FALSE) %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# solve problem
multi_s3 <- solve(multi_p3)

# plot solutions for problems associated with spatial data
par(mfrow = c(2, 1))
plot(c(multi_s3[[1]], multi_s3[[2]]), main = c("High degradation", "Low degradation"), axes = FALSE)

#### What does high and low degradation mean?
#### Compare to individual solutions

# solve individual problems
s1 <- p1 %>% solve()
s2 <- p2 %>% solve()

# Our order is p1 -> p2
# So low degradation in our hierarchical should be more similar to the first problem
plot(c(multi_s3[[2]], s1), main = c("Low degradation", "Solution 1"), axes = FALSE)

# And high degradation should be more similar to the second problem
plot(c(multi_s3[[1]], s2), main = c("High degradation", "Solution 2"), axes = FALSE)

#### Also works for other solvers (e.g., HiGHs)

# create multi-objective problem
multi_p4 <- multi_problem(
  obj1 = problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions(),
  obj2 = problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(
      budget = 0.2 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
    ) %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions()
) %>%
  add_hierarchical_approach(rel_tol = rel_tol_mat, verbose = FALSE) %>%
  add_highs_solver(gap = 0, verbose = FALSE) # uses default hierarchical approach

# solve problem
multi_s4 <- solve(multi_p4)

# plot solutions for problems associated with spatial data
plot(c(multi_s4[[2]], s1), main = c("Low degradation", "Solution 1"), axes = FALSE)
plot(c(multi_s4[[1]], s2), main = c("High degradation", "Solution 2"), axes = FALSE)

##### Exploring trade-offs
# Visualising an approximated pareto front with five solutions
#### Using an input matrix
rel_tol_mat <- matrix(c(0.9, 0.7, 0.5, 0.3, 0.1), nrow = 5, ncol = 1)

# create multi-objective problem
multi_p5 <- multi_problem(
  obj1 = problem(sim_pu_raster, sim_features) %>% # can also name objectives
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions(),
  obj2 = problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(
      budget = 0.2 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
    ) %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions()
) %>%
  add_hierarchical_approach(rel_tol = rel_tol_mat, verbose = FALSE) %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# solve problem
multi_s5 <- solve(multi_p5)

# for all solutions: calculate the two objective values (here: cost and shortfall)
# in gurobi: these are returned automatically (e.g. attributes(multi_s1)$objective returns two values)
# and maybe TODO: for other solvers/default approach also return all vals for all objectives in the final solution?

obj_mat <- attributes(multi_s5)$objective

# plot cost (x) vs shortfall (y)
plot(
  obj_mat[, 1], obj_mat[, 2],
  xlab = "Cost",
  ylab = "Shortfall",
  pch = 19,
  col = "blue",
  main = "Pareto front (approximated)"
)

##### Minimum set with minimum shortfall and another minimum shortfall ####
# create multi-objective problem
multi_p6 <- multi_problem(
  obj1 = problem(sim_pu_raster, sim_features) %>% # can also name objectives
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions(),
  obj2 = problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(
      budget = 0.2 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
    ) %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions(),
  obj3 = problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(
      budget = 0.5 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
    ) %>%
    add_relative_targets(0.3) %>%
    add_binary_decisions()
) %>%
  add_hierarchical_approach(rel_tol = rel_tol_mat, verbose = FALSE) %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# solve problem
multi_s6 <- solve(multi_p6) # show error message

# re-define rel_tol_mat
rel_tol_mat <- matrix(c(0.9, 0.1, 0.1, 0.9), nrow = 2, ncol = 2)

# create multi-objective problem
multi_p6 <- multi_problem(
  obj1 = problem(sim_pu_raster, sim_features) %>% # can also name objectives
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions(),
  obj2 = problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(
      budget = 0.2 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
    ) %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions(),
  obj3 = problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(
      budget = 0.5 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
    ) %>%
    add_relative_targets(0.3) %>%
    add_binary_decisions()
) %>%
  add_hierarchical_approach(rel_tol = rel_tol_mat, verbose = FALSE) %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# solve problem
multi_s6 <- solve(multi_p6)

##### Multi-zone problem ####
#TODO

###### Weighted-sum Approach ######
rm(list = ls()) # just to keep this in a single script for now

# import data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

###### Minimum set with minimum shortfall #####

#### Using an input vector

# define weights
weights <- runif(2)

# create multi-objective problem
multi_p1 <-
  multi_problem(
    obj1 = problem(sim_pu_raster, sim_features) %>% # can also name objectives
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions(),
    obj2 = problem(sim_pu_raster, sim_features) %>%
      add_min_shortfall_objective(
        budget = 0.2 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
      ) %>%
      add_relative_targets(0.2) %>%
      add_binary_decisions()
  ) %>%
  add_weighted_sum_approach(weights = weights, verbose = FALSE) %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve problem
multi_s1 <- solve(multi_p1)

plot(multi_s1, main = "Random weights", axes = FALSE)

#### What does changing the weights mean?
#### Compare to individual solutions
 #TODO

#### Using an input matrix
weights <- matrix(c(0.001, 0.002, 0.003,
                    0.999, 0.998, 0.997), ncol = 2)

# create multi-objective problem
multi_p2 <-
  multi_problem(
    obj1 = problem(sim_pu_raster, sim_features) %>% # can also name objectives
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions(),
    obj2 = problem(sim_pu_raster, sim_features) %>%
      add_min_shortfall_objective(
        budget = 0.2 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
      ) %>%
      add_relative_targets(0.2) %>%
      add_binary_decisions()
  ) %>%
  add_weighted_sum_approach(weights = weights, verbose = FALSE) %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve problem
multi_s2 <- solve(multi_p2) #TODO: some print statement what we are currently solving

obj_mat <- attributes(multi_s2)$objective

# plot cost (x) vs shortfall (y)
plot(
  obj_mat[, 1], obj_mat[, 2],
  xlab = "Cost",
  ylab = "Shortfall",
  pch = 19,
  col = "blue",
  main = "Pareto front (approximated)"
)


##### Minimum set with minimum shortfall and another minimum shortfall ####


##### Multi-zone problem #####
