context("add_lpsymphony_solver")

test_that("binary decisions", {
  skip_on_cran()
  skip_if_not_installed("lpsymphony")
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_lpsymphony_solver(first_feasible = TRUE, verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "Raster"))
  expect_equal(terra::nlyr(s), 1)
  expect_equal(sort(unique(terra::values(s))), c(0, 1))
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("proportion decisions", {
  skip_on_cran()
  skip_if_not_installed("lpsymphony")
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions() %>%
       add_lpsymphony_solver(gap = 0, verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "Raster"))
  expect_equal(terra::nlyr(s), 1)
  expect_gte(min(terra::values(s), na.rm = TRUE), 0)
  expect_lte(max(terra::values(s), na.rm = TRUE), 1)
  expect_gt(max(terra::values(s) - round(terra::values(s)),
              na.rm = TRUE), 0.01)
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("proportion decisions (floating point)", {
  skip_on_cran()
  skip_if_not_installed("lpsymphony")
  skip_if_not_installed("prioritizrdata")
  # skip_on_os("linux") # crashes unpredictably on Ubuntu 16.04
  # make data
  data(tas_pu, tas_features, package = "prioritizrdata")
  tas_pu <- tas_pu[seq_len(15), ]
  p <- problem(tas_pu, tas_features, cost = "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(1) %>%
       add_proportion_decisions() %>%
       add_lpsymphony_solver(gap = 0, verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "SpatialPolygonsDataFrame"))
  expect_true("solution_1" %in% names(s@data))
  expect_equal(nrow(s), nrow(tas_pu))
  expect_is(s$solution_1, "numeric")
  expect_gte(min(s$solution_1), 0)
  expect_lte(max(s$solution_1), 1)
})

test_that("variable bounds methods", {
  skip_if_not_installed("lpsymphony")
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
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

test_that("mix of binary and continuous variables", {
  skip_on_cran()
  skip_if_not_installed("lpsymphony")
  # make data
  b <- raster::cellStats(sim_pu_raster, "sum") * 0.2
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_max_utility_objective(b) %>%
       add_binary_decisions() %>%
       add_lpsymphony_solver(verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "Raster"))
  expect_equal(terra::nlyr(s), 1)
  expect_equal(sort(unique(terra::values(s))), c(0, 1))
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("first_feasible", {
  skip_on_cran()
  skip_if_not_installed("lpsymphony")
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p1 <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions() %>%
        add_lpsymphony_solver(first_feasible = TRUE, verbose = FALSE)
  s1 <- solve(p1)
  # check that solution has correct properties
  expect_is(s1, "Raster")
  expect_equal(terra::nlyr(s1), 1)
  expect_equal(sort(unique(terra::values(s1))), c(0, 1))
  expect_true(raster::compareRaster(sim_pu_raster, s1, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("correct solution (simple)", {
  skip_on_cran()
  skip_if_not_installed("lpsymphony")
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- terra::rast(terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
                            terra::rast(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_locked_in_constraints(locked_in) %>%
       add_locked_out_constraints(locked_out) %>%
       add_lpsymphony_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # test for correct solution
  expect_equal(terra::values(s1), c(0, 1, 1, NA))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("correct solution (complex)", {
  skip_on_cran()
  skip_if_not_installed("lpsymphony")
  # create data
  cost <- terra::rast(matrix(c(1000, 100, 200, 300, NA), nrow = 1))
  features <- terra::rast(
    terra::rast(matrix(c(5,  5,   0,  0,  NA), nrow = 1)),
    terra::rast(matrix(c(2,  0,   8,  10, NA), nrow = 1)),
    terra::rast(matrix(c(10, 100, 10, 10, NA), nrow = 1)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_manual_targets(
         tibble::tibble(
           feature = names(features),
           type = "absolute",
           sense = c("=", ">=", "<="),
           target = c(5, 10, 20))) %>%
       add_lpsymphony_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # test for correct solution
  expect_equal(terra::values(s1), c(1, 0, 1, 0, NA))
  expect_equal(terra::values(s1), terra::values(s2))
})
