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

result_1 <- gurobi(model_p, params = list(OutputFlag = 1))
