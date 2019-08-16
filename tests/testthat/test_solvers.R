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

test_that("add_rsymphony_solver", {
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
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
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

test_that("add_lpsymphony_solver", {
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
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
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

test_that("add_gurobi_solver", {
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
  expect_true(raster::compareRaster(sim_pu_raster, s1, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
  expect_equal(raster::getValues(s1), raster::getValues(s2))
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
