# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# define rel_tol
rel_tol_vector <- c(0.1, 0.9)

# create two small example problems
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

multi_test <- multi_problem(p1, p2) %>%
  add_default_solver(gap = 0, verbose = FALSE)

test1 <- add_hierarchical_approach(multi_test, 0.9) %>%
  solve()
test2 <- add_hierarchical_approach(multi_test, 0.1) %>%
  solve()

test3 <- p1 %>% solve()
test4 <- p2 %>% solve()

terra::plot(c(test1, test4))
terra::plot(c(test2, test3))
#I'd expect these to be similar: 
# test1 has a high degradation, so should be more similar to our second problem (so test4)
# test2 has low degradation, so should be similar to first problem (so test3)

# alternative
rel_tol_mat <- matrix(c(0.9, 0.1), nrow = 2, ncol = 1)
test5 <- add_hierarchical_approach(multi_test, rel_tol_mat) %>%
  solve()

terra::plot(c(test5[[1]], test4))
terra::plot(c(test5[[2]], test3))

#########

library(highs)

p1 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_highs_solver(gap = 0, verbose = FALSE)

p2 <- problem(sim_pu_raster, sim_features) %>%
  add_min_shortfall_objective(
    budget = 0.2 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
  ) %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_highs_solver(gap = 0, verbose = FALSE)

multi_highs <- multi_problem(p1, p2) %>%
  add_highs_solver(gap = 0, verbose = FALSE)

test1 <- add_hierarchical_approach(multi_highs, 0.9) %>%
  solve()
test2 <- add_hierarchical_approach(multi_highs, 0.1) %>%
  solve()

test3 <- p1 %>% solve()
test4 <- p2 %>% solve()

#these have the ones I expect to be similar
terra::plot(c(test1, test4))
terra::plot(c(test2, test3))

# alternative
rel_tol_mat <- matrix(c(0.9, 0.1), nrow = 2, ncol = 1)
test5 <- add_hierarchical_approach(multi_highs, rel_tol_mat) %>%
  solve() 

#### this works now (rel_tol smaller <0.1): 
testx <- add_hierarchical_approach(multi_highs, 0.001) %>%
  solve()

terra::plot(c(testx, test2))

terra::plot(c(testx, test3))
terra::plot(c(testx, test4))


#####

library(rcbc)

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

multi_highs <- multi_problem(p1, p2) %>%
  add_cbc_solver(gap = 0, verbose = FALSE)

test1 <- add_hierarchical_approach(multi_highs, 0.9) %>%
  solve()
test2 <- add_hierarchical_approach(multi_highs, 0.1) %>%
  solve()

test3 <- p1 %>% solve()
test4 <- p2 %>% solve()

#these have the ones I expect to be similar
terra::plot(c(test1, test4))
terra::plot(c(test2, test3))

# alternative
rel_tol_mat <- matrix(c(0.9, 0.1), nrow = 2, ncol = 1)
test5 <- add_hierarchical_approach(multi_highs, rel_tol_mat) %>%
  solve() 

#### this works now (rel_tol smaller <0.1): 
testx <- add_hierarchical_approach(multi_highs, 0.001) %>%
  solve()

terra::plot(c(testx, test2))

terra::plot(c(testx, test3))
terra::plot(c(testx, test4))

##### takes forever

library(Rsymphony)

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

multi_highs <- multi_problem(p1, p2) %>%
  add_rsymphony_solver(gap = 0, verbose = FALSE)

test1 <- add_hierarchical_approach(multi_highs, 0.9) %>%
  solve()
test2 <- add_hierarchical_approach(multi_highs, 0.1) %>%
  solve()

test3 <- p1 %>% solve()
test4 <- p2 %>% solve()

#these have the ones I expect to be similar
terra::plot(c(test1, test4))
terra::plot(c(test2, test3))

# alternative
rel_tol_mat <- matrix(c(0.9, 0.1), nrow = 2, ncol = 1)
test5 <- add_hierarchical_approach(multi_highs, rel_tol_mat) %>%
  solve() 

#### this works now (rel_tol smaller <0.1): 
testx <- add_hierarchical_approach(multi_highs, 0.001) %>%
  solve()

terra::plot(c(testx, test2))

terra::plot(c(testx, test3))
terra::plot(c(testx, test4))



