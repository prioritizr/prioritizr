devtools::load_all()
library(Matrix)
# import data
sim_pu <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create two small example problems
p1 <- problem(sim_pu, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions()

p2 <- problem(sim_pu, sim_features) %>%
  add_min_shortfall_objective(
    budget = 0.2 * terra::global(sim_pu, sum, na.rm = TRUE)[[1]]
  ) %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions()

weights <- c(0.3, 0.7)

# compile problem
x <- compile_ws_approach(list(p1, p2), weights)
# calculations for tests
# TODO
# tests
expect_equal(x$rhs(), c(targ1, targ2))
expect_equal(x$sense(), rep(">=", 5))
expect_equal(
  x$obj(),
  c(sim_pu_raster[[1]][!is.na(sim_pu_raster)]* weights[1]) +
    c(sim_pu_raster[[1]][!is.na(sim_pu_raster)] * weights[2])
)

model_p <- list(
  A = x$A(),
  obj = x$obj(),
  modelsense = x$modelsense(),
  rhs = x$rhs(),
  sense = x$sense(),
  vtype = x$vtype(),
  lb = x$lb(),
  ub = x$ub()
)

library(gurobi)
library(terra)

result_1 <- gurobi(model_p, params = list(OutputFlag = 1))


### hierarchical
degradation_vec <- 0.1 #when increasing this, shortfalls are decreasing and cost is increasing which is what we want

h_p1 <- add_hierarchical_approach(list(p1, p2), degradation_vec)

n_pu <- p1$number_of_planning_units()
s1 <- h_p1[[2]]$solution[1:n_pu] #final solution
sf1 <- h_p1[[2]]$solution[-(1:n_pu)]

solution_rast <- sim_pu

# Get indices of non-NA cells (these were the PUs used in the model)
valid_idx <- which(!is.na(values(sim_pu)))

# Fill in solution values
values(solution_rast)[valid_idx] <- s1

terra::plot(solution_rast)
print(sf1) #shortfalls
print(sum(s1)) #area
print(sum(h_p1[[1]]$model$obj*s1)) # cost
