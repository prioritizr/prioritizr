#### part 1: single-objective ####

# import data
con_cost <- get_sim_pu_raster()
keystone_spp <- get_sim_features()[[1:3]]
iconic_spp <- get_sim_features()[[4:5]]

# set budget
con_budget <- terra::global(con_cost, "sum", na.rm = TRUE)[[1]] * 0.3

# define individual problems
p1 <-
  problem(con_cost, keystone_spp) %>%
  add_min_shortfall_objective(con_budget) %>%
  add_relative_targets(0.4) %>%
  add_binary_decisions() %>%
  add_default_solver()
p2 <-
  problem(con_cost, iconic_spp) %>%
  add_min_shortfall_objective(con_budget) %>%
  add_relative_targets(0.4) %>%
  add_binary_decisions() %>%
  add_default_solver()

# solve problems
s1 <- solve(p1)
s2 <- solve(p2)

# plot
plot(s1)
plot(s2)

# feature rep
eval_target_coverage_summary(p1, s1)$relative_held
eval_target_coverage_summary(p2, s2)$relative_held

#### part 2: multi-objective - weighted sum ####

# create multi-objective problem
mp1 <-
  multi_problem(keystone_obj = p1, iconic_obj = p2) %>% # under the hood takes these two problems and merges them strategically together
  add_weighted_sum_approach(c(0.5, 0.5), verbose = FALSE) %>% # use input vector with equal weights
  add_default_solver()

# solve problem
ms1 <- solve(mp1)

# create multi-objective problem
mp2 <-
  multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
  add_weighted_sum_approach(
    matrix(c( # use input matrix
      0.5, 0.5, # balanced
      1.0, 0.0, # all in on keystone
      0.0, 1.0 # all in on iconic
    ), ncol = 2)
  ) %>%
  add_default_solver()

# solve problem
ms2 <- solve(mp2)

# extract objective values and plot approximated pareto front (very few weight values)
obj_mat <- attributes(ms2)$objective
plot(obj_mat)

# create multi-objective problem
mp3 <-
  multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
  add_weighted_sum_approach(
    matrix(runif(300), ncol = 2),
    # expand.grid( # alternative
    #   v1 = seq(0.01, 1, length.out = 20),
    #   v2 = seq(0.01, 1, length.out = 20)
    # ) %>%
    #   dplyr::filter(v1 != v2) %>%
    #   as.matrix(),
    verbose = FALSE
  ) %>%
  add_default_solver()

# solve problem
ms3 <- solve(mp3)

# extract objective values and plot approximated pareto front (more weight values)
obj_mat <- attributes(ms3)$objective
plot(obj_mat)

#### part 3: multi-objective - hierarchical ####

mp4 <- multi_problem(
  obj1 = problem(con_cost, keystone_spp) %>%
    add_min_shortfall_objective(con_budget) %>%
    add_relative_targets(0.4) %>%
    add_binary_decisions() %>%
    add_default_solver(),
  obj2 = problem(con_cost, iconic_spp) %>%
    add_min_shortfall_objective(con_budget) %>%
    add_relative_targets(0.4) %>%
    add_binary_decisions() %>%
    add_default_solver()
) %>%
  add_hierarchical_approach(rel_tol = matrix(c(0.9, 0.1), nrow = 2, ncol = 1), verbose = FALSE) %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) # uses gurobi internal functionality, but we have also added functionality that works with all other solvers

# solve problem
ms4 <- solve(mp4)

# plot solutions for problems associated with spatial data
plot(c(ms4[[1]], ms4[[2]]), main = c("High degradation", "Low degradation"), axes = FALSE)

#### What does high and low degradation mean?
#### Compare to individual solutions

# Our order is p1 -> p2
# so high degradation should be more similar to the second problem
plot(c(ms4[[1]], s2), main = c("High degradation", "Solution 2"), axes = FALSE)

# and low degradation in our hierarchical should be more similar to the first problem
plot(c(ms4[[2]], s1), main = c("Low degradation", "Solution 1"), axes = FALSE) # previous constraints locked in when solving for second ojective, so still a bit different comapred to indiviudal solution

#### Explore trade-offs

rel_tol <- matrix(seq(0, 1, length.out = 40), ncol = 1)
mp5 <- multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
  add_hierarchical_approach(rel_tol) %>%
  add_default_solver()
ms5 <- solve(mp5)

# extract objective values and plot approximated pareto front (very few weight values)
obj_mat <- attributes(ms5)$objective
plot(obj_mat)

#### 3 objective functions
rel_tol_mat <- matrix(c(0.9, 0.1, 0.1, 0.9), nrow = 2, ncol = 2)

# create multi-objective problem
mp6 <- multi_problem(
  obj1 = problem(con_cost, keystone_spp) %>% 
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions(),
  obj2 = problem(con_cost, keystone_spp) %>%
    add_min_shortfall_objective(con_budget) %>%
    add_relative_targets(0.4) %>%
    add_binary_decisions() %>%
    add_default_solver(),
  obj3 = problem(con_cost, iconic_spp) %>%
    add_min_shortfall_objective(con_budget) %>%
    add_relative_targets(0.4) %>%
    add_binary_decisions() %>%
    add_default_solver()
) %>%
  add_hierarchical_approach(rel_tol = rel_tol_mat, verbose = FALSE) %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# solve problem
ms6 <- solve(mp6)

# plot
plot(c(ms6[[1]], ms6[[2]]), main = c("High/low degradation", "Low/high degradation"), axes = FALSE)

#### part 4: zones ####
con_zone_cost <- get_sim_pu_raster() # layer
agr_zone_cost <- get_sim_pu_raster() * 20 # layer 
con_zone_con_ft <- get_sim_features()[[1:3]] # stack with 3 layers
con_zone_agr_ft <- get_sim_features()[[4:5]] * 0 # stack with 2 layers, vals 0 
agr_zone_con_ft <- get_sim_features()[[1:3]] * 0 # stack with 3 layers, vals 0
agr_zone_agr_ft <- get_sim_features()[[4:5]] * 100 # stack with 2 layer

con_budget <- terra::global(con_zone_cost, "sum", na.rm = TRUE)[[1]] * 0.2
agr_budget <- terra::global(agr_zone_cost, "sum", na.rm = TRUE)[[1]] * 0.2

p <- multi_problem(
  con_obj = problem(
    c(con_zone_cost, agr_zone_cost),
    zones(
      con_zone = con_zone_con_ft,
      agr_zone = agr_zone_con_ft
    )
  ) %>%
    add_min_shortfall_objective(c(con_budget, agr_budget)) %>%
    add_relative_targets(matrix(0.4, nrow = 5, ncol = 2)) %>%
    add_binary_decisions(),
  
  agr_obj = problem(
    c(con_zone_cost, agr_zone_cost),
    zones(
      con_zone = con_zone_agr_ft,
      agr_zone = agr_zone_agr_ft
    )
  ) %>%
    add_min_shortfall_objective(c(con_budget, agr_budget)) %>%
    add_relative_targets(matrix(0.9, nrow = 5, ncol = 2)) %>%
    add_binary_decisions()
) %>%
  add_hierarchical_approach(rel_tol) %>%
  add_gurobi_solver()
