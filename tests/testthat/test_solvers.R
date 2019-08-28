context("solvers")

test_that("add_default_solver (raster cost data)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_default_solver(time_limit = 5)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "Raster"))
  expect_equal(raster::nlayers(s), 1)
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
              tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("add_default_solver (spatial cost data)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # make data
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_default_solver(time_limit = 5)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "SpatialPolygonsDataFrame"))
  expect_equal(length(s), length(sim_pu_polygons))
  expect_equal(s@polygons, sim_pu_polygons@polygons)
  expect_true(raster::compareCRS(s, sim_pu_polygons))
})

test_that("add_rsymphony_solver (binary decisions)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  skip_if_not_installed("Rsymphony")
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_rsymphony_solver(first_feasible = TRUE, verbose = TRUE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "Raster"))
  expect_equal(raster::nlayers(s), 1)
  expect_equal(sort(unique(raster::getValues(s))), c(0, 1))
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("add_rsymphony_solver (proportion decisions)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  skip_if_not_installed("Rsymphony")
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions() %>%
       add_rsymphony_solver(gap = 0, verbose = TRUE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "Raster"))
  expect_equal(raster::nlayers(s), 1)
  expect_gte(min(raster::getValues(s), na.rm = TRUE), 0)
  expect_lte(max(raster::getValues(s), na.rm = TRUE), 1)
  expect_gt(max(raster::getValues(s) - round(raster::getValues(s)),
              na.rm = TRUE), 0.01)
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("add_rsymphony_solver (proportion decisions, floating point)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  skip_if_not_installed("Rsymphony")
  skip_if_not_installed("prioritizrdata")
  # make data
  data(tas_pu, tas_features, package = "prioritizrdata")
  p <- problem(tas_pu, tas_features, cost = "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(1) %>%
       add_proportion_decisions() %>%
       add_rsymphony_solver(gap = 0, verbose = TRUE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "SpatialPolygonsDataFrame"))
  expect_true("solution_1" %in% names(s@data))
  expect_equal(nrow(s), nrow(tas_pu))
  expect_is(s$solution_1, "numeric")
  expect_gte(min(s$solution_1), 0)
  expect_lte(max(s$solution_1), 1)
})

test_that("add_rsymphony_solver (variable bounds methods)", {
  skip_if_not_installed("Rsymphony")
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_rsymphony_solver(first_feasible = TRUE, verbose = TRUE)
  p$solver$calculate(compile.ConservationProblem(p))
  p$solver$set_variable_ub(1, 0)
  p$solver$set_variable_lb(2, 1)
  # check that  solution has correct properties
  expect_equal(p$solver$data$model$bounds$upper$val,
               replace(rep(1, p$number_of_planning_units()), 1, 0))
  expect_equal(p$solver$data$model$bounds$lower$val,
               replace(rep(0, p$number_of_planning_units()), 2, 1))
})

test_that("add_lpsymphony_solver (binary decisions)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  skip_if_not_installed("lpsymphony")
  skip_on_os("linux") # lpsymphony package crashes unpredictably on Ubuntu 16.04
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_lpsymphony_solver(first_feasible = TRUE, verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "Raster"))
  expect_equal(raster::nlayers(s), 1)
  expect_equal(sort(unique(raster::getValues(s))), c(0, 1))
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("add_lpsymphony_solver (proportion decisions)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  skip_if_not_installed("lpsymphony")
  skip_on_os("linux") # lpsymphony package crashes unpredictably on Ubuntu 16.04
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions() %>%
       add_lpsymphony_solver(gap = 0, verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "Raster"))
  expect_equal(raster::nlayers(s), 1)
  expect_gte(min(raster::getValues(s), na.rm = TRUE), 0)
  expect_lte(max(raster::getValues(s), na.rm = TRUE), 1)
  expect_gt(max(raster::getValues(s) - round(raster::getValues(s)),
              na.rm = TRUE), 0.01)
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("add_lpsymphony_solver (proportion decisions, floating point)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  skip_if_not_installed("lpsymphony")
  skip_if_not_installed("prioritizrdata")
  skip_on_os("linux") # lpsymphony package crashes unpredictably on Ubuntu 16.04
  # make data
  data(tas_pu, tas_features, package = "prioritizrdata")
  p <- problem(tas_pu, tas_features, cost = "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(1) %>%
       add_proportion_decisions() %>%
       add_lpsymphony_solver(gap = 0, verbose = TRUE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "SpatialPolygonsDataFrame"))
  expect_true("solution_1" %in% names(s@data))
  expect_equal(nrow(s), nrow(tas_pu))
  expect_is(s$solution_1, "numeric")
  expect_gte(min(s$solution_1), 0)
  expect_lte(max(s$solution_1), 1)
})

test_that("add_lpsymphony_solver (variable bounds methods)", {
  skip_if_not_installed("lpsymphony")
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_lpsymphony_solver(first_feasible = TRUE, verbose = TRUE)
  p$solver$calculate(compile.ConservationProblem(p))
  p$solver$set_variable_ub(1, 0)
  p$solver$set_variable_lb(2, 1)
  # check that  solution has correct properties
  expect_equal(p$solver$data$model$bounds$upper$val,
               replace(rep(1, p$number_of_planning_units()), 1, 0))
  expect_equal(p$solver$data$model$bounds$lower$val,
               replace(rep(0, p$number_of_planning_units()), 2, 1))
})

test_that("add_gurobi_solver (binary decisions)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  skip_if_not_installed("gurobi")
  # make data
  data(sim_pu_raster, sim_features)
  p1 <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions() %>%
        add_gurobi_solver(time_limit = 5, verbose = TRUE)
  p2 <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions() %>%
        add_gurobi_solver(time_limit = 5, numeric_focus = TRUE, verbose = TRUE)
  s1 <- solve(p1)
  s2 <- solve(p2)
  # check that solution has correct properties
  expect_is(s1, "Raster")
  expect_equal(raster::nlayers(s1), 1)
  expect_equal(sort(unique(raster::getValues(s1))), c(0, 1))
  expect_true(raster::compareRaster(sim_pu_raster, s1, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
  expect_equal(raster::getValues(s1), raster::getValues(s2))
})

test_that("add_gurobi_solver (proportion decisions)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  skip_if_not_installed("gurobi")
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions() %>%
       add_gurobi_solver(gap = 0, verbose = TRUE)
  s <- solve(p)
  # check that solution has correct properties
  expect_is(s, "Raster")
  expect_equal(raster::nlayers(s), 1)
  expect_gte(min(raster::getValues(s), na.rm = TRUE), 0)
  expect_lte(max(raster::getValues(s), na.rm = TRUE), 1)
  expect_gt(max(raster::getValues(s) - round(raster::getValues(s)),
              na.rm = TRUE), 0.01)
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("add_gurobi_solver (proportion decisions, floating point)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  skip_if_not_installed("gurobi")
  skip_if_not_installed("prioritizrdata")
  # make data
  data(tas_pu, tas_features, package = "prioritizrdata")
  p <- problem(tas_pu, tas_features, cost = "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(1) %>%
       add_proportion_decisions() %>%
       add_gurobi_solver(gap = 0, verbose = TRUE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "SpatialPolygonsDataFrame"))
  expect_true("solution_1" %in% names(s@data))
  expect_equal(nrow(s), nrow(tas_pu))
  expect_is(s$solution_1, "numeric")
  expect_gte(min(s$solution_1), 0)
  expect_lte(max(s$solution_1), 1)
})

test_that("add_gurobi_solver (variable bounds methods)", {
  skip_if_not_installed("gurobi")
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_gurobi_solver(first_feasible = TRUE, verbose = TRUE)
  p$solver$calculate(compile.ConservationProblem(p))
  p$solver$set_variable_ub(1, 0)
  p$solver$set_variable_lb(2, 1)
  # check that  solution has correct properties
  expect_equal(p$solver$data$model$ub,
               replace(rep(1, p$number_of_planning_units()), 1, 0))
  expect_equal(p$solver$data$model$lb,
               replace(rep(0, p$number_of_planning_units()), 2, 1))
})
