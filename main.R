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
test2 <- add_hierarchical_approach(multi_test, 0.001) %>%
  solve()

terra::plot(c(test1, test2))

test3 <- p1 %>% solve()
test4 <- p2 %>% solve()
terra::plot(c(test3, test4))

#I'd expect these to be similar: 
terra::plot(c(test4, test2))
terra::plot(c(test3, test1)) # not as similar, why not -> because still have hard constraints for my p1 that have to be met

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

terra::plot(c(test1, test2))

test3 <- p1 %>% solve()
test4 <- p2 %>% solve()
terra::plot(c(test3, test4))

#this is weird, the wrong ones are similar!! Go over code
terra::plot(c(test1, test4))
terra::plot(c(test2, test3))

#### this doesn't work (rel_tol smaller <0.1): 
test2 <- add_hierarchical_approach(multi_highs, 0.01) %>%
  solve()
