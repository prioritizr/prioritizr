test_that("compile dual min set problems", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define rel_tol
  rel_tol <- c(0.1)
  # calculate targets data
  targ1 <- floor(
    terra::global(sim_features[[1:2]], "sum", na.rm = TRUE)[[1]] * 0.25
  )
  targ2 <- floor(
    terra::global(sim_features[[3:5]], "sum", na.rm = TRUE)[[1]] * 0.1
  )
  # create problems
  p1 <-
    problem(sim_pu_raster * 1, sim_features[[1:2]]) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ1) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_pu_raster * 3, sim_features[[3:5]]) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ2) %>%
    add_binary_decisions()
  # compile problem
  x <- compile_hierarchical_approach(list(p1, p2))
  # tests
  extras <-  length(compile(p1)$obj())-compile(p1)$number_of_planning_units() + 
    length(compile(p2)$obj())-compile(p2)$number_of_planning_units()
  
  expect_type(x, "list")
  expect_named(x, c("mopt", "obj", "modelsense"))
  expect_equal(nrow(x$obj), 2) 
  expect_type(x$modelsense, "character")
  expect_length(x$modelsense, 2)
  expect_true(all(x$modelsense %in% c("min", "max")))
  expect_equal(length(x$mopt$lb()), compile(p1)$number_of_planning_units()+
                 extras) 
  expect_equal(length(x$mopt$ub()), compile(p1)$number_of_planning_units()+
                 extras) 
  expect_equal(length(x$mopt$vtype()), compile(p1)$number_of_planning_units()+
                 extras) 
  expect_equal(nrow(x$mopt$A()), nrow(compile(p1)$A()) + nrow(compile(p2)$A())) 
  expect_equal(ncol(x$mopt$A()), compile(p1)$number_of_planning_units()+
                 extras)
  expect_equal(length(x$mopt$rhs()), length(compile(p1)$rhs()) + length(compile(p2)$rhs())) 
})

test_that("solve dual min set problems", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define rel_tol
  rel_tol <- c(0.1)
  # calculate targets data
  targ1 <- floor(
    terra::global(sim_features[[1:2]], "sum", na.rm = TRUE)[[1]] * 0.25
  )
  targ2 <- floor(
    terra::global(sim_features[[3:5]], "sum", na.rm = TRUE)[[1]] * 0.1
  )
  # create problems
  p1 <-
    problem(sim_pu_raster * 1, sim_features[[1:2]]) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ1) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_pu_raster * 3, sim_features[[3:5]]) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ2) %>%
    add_binary_decisions()
  # solve problem
  s <- solve_hierarchical_approach(list(p1, p2), rel_tol, method = "manual")
  # run tests
  ## TODO
  expect_true(all(terra::values(s1) %in% c(0, 1, NA, NaN)))
  
  # also need some tests changing degradation parameter and then checking changes

})


test_that("compile dual min shortfalls problems", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define rel_tol
  rel_tol <- c(0.1)
  # calculate targets data
  targ1 <- floor(
    terra::global(sim_features[[1:2]], "sum", na.rm = TRUE)[[1]] * 0.1
  )
  targ2 <- floor(
    terra::global(sim_features[[3:5]], "sum", na.rm = TRUE)[[1]] * 0.25
  )
  # create problems
  p1 <-
    problem(sim_pu_raster * 1, sim_features[[1:2]]) %>%
    add_min_shortfall_objective(
      budget = 0.2 * terra::global(sim_pu_raster/sim_pu_raster, sum, na.rm = TRUE)[[1]]
    ) %>%
    add_absolute_targets(targ1) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_pu_raster * 3, sim_features[[3:5]]) %>%
    add_min_shortfall_objective(
      budget = 0.4 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]
    ) %>%
    add_absolute_targets(targ2) %>%
    add_binary_decisions()
  # compile problem
  x <- compile_hierarchical_approach(list(p1, p2))
  # tests
  extras <-  length(compile(p1)$obj())-compile(p1)$number_of_planning_units() + 
    length(compile(p2)$obj())-compile(p2)$number_of_planning_units()
  
  expect_type(x, "list")
  expect_named(x, c("mopt", "obj", "modelsense"))
  expect_equal(nrow(x$obj), 2) 
  expect_type(x$modelsense, "character")
  expect_length(x$modelsense, 2)
  expect_true(all(x$modelsense %in% c("min", "max")))
  expect_equal(length(x$mopt$lb()), compile(p1)$number_of_planning_units()+
                 extras) 
  expect_equal(length(x$mopt$ub()), compile(p1)$number_of_planning_units()+
                 extras) 
  expect_equal(length(x$mopt$vtype()), compile(p1)$number_of_planning_units()+
                 extras) 
  expect_equal(nrow(x$mopt$A()), nrow(compile(p1)$A()) + nrow(compile(p2)$A())) 
  expect_equal(ncol(x$mopt$A()), compile(p1)$number_of_planning_units()+
                 extras)
  expect_equal(length(x$mopt$rhs()), length(compile(p1)$rhs()) + length(compile(p2)$rhs())) 

})

test_that("solve dual min shortfalls problems", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define rel_tol
  rel_tol_vector <- c(0.2, 0.4)
  # calculate targets data
  targ1 <- floor(
    terra::global(sim_features[[c(1,3)]], "sum", na.rm = TRUE)[[1]] * 0.1
  )
  targ2 <- floor(
    terra::global(sim_features[[c(2,4,5)]], "sum", na.rm = TRUE)[[1]] * 0.6
  )
  # create problems
  p1 <-
    problem(sim_pu_raster * 5, sim_features[[c(1,3)]]) %>%
    add_min_shortfall_objective(
      budget = 0.1 * terra::global(sim_pu_raster*5, sum, na.rm = TRUE)[[1]]
    ) %>%
    add_absolute_targets(targ1) %>%
    add_binary_decisions()
  p2 <-
    problem(area_test, sim_features[[c(2,4,5)]]) %>%
    add_min_shortfall_objective(
      budget = 0.7 * terra::global(area_test, sum, na.rm = TRUE)[[1]]
    ) %>%
    add_absolute_targets(targ2) %>%
    add_binary_decisions()
  
  # solve problem
  s1 <- solve_hierarchical_approach(list(p1, p2), rel_tol_vector[1], method = "manual")
  s2 <- solve_hierarchical_approach(list(p1, p2), rel_tol_vector[2], method = "manual")
  # run tests
  expect_true(all(terra::values(s1) %in% c(0, 1, NA, NaN)))
  expect_true(all(terra::values(s2) %in% c(0, 1, NA, NaN)))
  
  # check whether we have higher shortfall but lower cost in s1 compared to s2
  # expect_true(terra::global(s2*sim_pu_raster, sum, na.rm=TRUE) > 
  #               terra::global(s1*sim_pu_raster, sum, na.rm=TRUE))
  
  #hard to test without seeing shortfall

})

test_that("compile multiple mixed problems", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  
  # define rel_tol
  rel_tol <- c(0.1)
  
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
  
  # compile problem
  x <- compile_hierarchical_approach(list(p1, p2))
  # tests
  extras <-  length(compile(p1)$obj())-compile(p1)$number_of_planning_units() + 
    length(compile(p2)$obj())-compile(p2)$number_of_planning_units()
  
  expect_type(x, "list")
  expect_named(x, c("mopt", "obj", "modelsense"))
  expect_equal(nrow(x$obj), 2) 
  expect_type(x$modelsense, "character")
  expect_length(x$modelsense, 2)
  expect_true(all(x$modelsense %in% c("min", "max")))
  expect_equal(length(x$mopt$lb()), compile(p1)$number_of_planning_units()+
                 extras) 
  expect_equal(length(x$mopt$ub()), compile(p1)$number_of_planning_units()+
                 extras) 
  expect_equal(length(x$mopt$vtype()), compile(p1)$number_of_planning_units()+
                 extras) 
  expect_equal(nrow(x$mopt$A()), nrow(compile(p1)$A()) + nrow(compile(p2)$A())) 
  expect_equal(ncol(x$mopt$A()), compile(p1)$number_of_planning_units()+
                 extras)
  expect_equal(length(x$mopt$rhs()), length(compile(p1)$rhs()) + length(compile(p2)$rhs())) 

})

test_that("solve multiple mixed problems", {
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
  
  # solve problem
  s1 <- solve_hierarchical_approach(list(p1, p2), rel_tol_vector[1], method = "manual")
  s2 <- solve_hierarchical_approach(list(p1, p2), rel_tol_vector[2], method = "manual")
  
  s1 <- solve_hierarchical_approach(list(p1, p2), rel_tol_vector[1], method = "gurobi")
  s2 <- solve_hierarchical_approach(list(p1, p2), rel_tol_vector[2], method = "gurobi")
  # run tests
  expect_true(all(terra::values(s1) %in% c(0, 1, NA, NaN)))
  expect_true(all(terra::values(s2) %in% c(0, 1, NA, NaN)))
  
  # check whether we have higher shortfall but lower cost in s1 compared to s2
  expect_true(terra::global(s2*sim_pu_raster, sum, na.rm=TRUE) > 
                terra::global(s1*sim_pu_raster, sum, na.rm=TRUE))
  
  # the way we are saving this currently, I don't think I can extract the shortfalls?
  

})
