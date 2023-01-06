context("add_cbc_solver")

test_that("binary decisions", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p1 <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions() %>%
        add_cbc_solver(time_limit = 5, verbose = FALSE)
  p2 <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions() %>%
        add_cbc_solver(time_limit = 5, verbose = FALSE)
  s1 <- solve(p1)
  s2 <- solve(p2)
  # check that solution has correct properties
  expect_is(s1, "Raster")
  expect_equal(terra::nlyr(s1), 1)
  expect_equal(sort(unique(raster::getValues(s1))), c(0, 1))
  expect_true(raster::compareRaster(sim_pu_raster, s1, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
  expect_equal(raster::getValues(s1), raster::getValues(s2))
})

test_that("proportion decisions", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions() %>%
       add_cbc_solver(gap = 0, verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_is(s, "Raster")
  expect_equal(terra::nlyr(s), 1)
  expect_gte(min(raster::getValues(s), na.rm = TRUE), 0)
  expect_lte(max(raster::getValues(s), na.rm = TRUE), 1)
  expect_gt(max(raster::getValues(s) - round(raster::getValues(s)),
              na.rm = TRUE), 0.01)
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("proportion decisions (floating point)", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  skip_if_not_installed("prioritizrdata")
  # make data
  data(tas_pu, tas_features, package = "prioritizrdata")
  tas_pu <- tas_pu[seq_len(15), ]
  p <- problem(tas_pu, tas_features, cost = "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(1) %>%
       add_proportion_decisions() %>%
       add_cbc_solver(gap = 0, verbose = FALSE)
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
  skip_if_not_installed("rcbc")
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_cbc_solver(verbose = TRUE)
  p$solver$calculate(compile.ConservationProblem(p))
  p$solver$set_variable_ub(1, 0)
  p$solver$set_variable_lb(2, 1)
  # check that  solution has correct properties
  expect_equal(p$solver$data$model$col_ub,
               replace(rep(1, p$number_of_planning_units()), 1, 0))
  expect_equal(p$solver$data$model$col_lb,
               replace(rep(0, p$number_of_planning_units()), 2, 1))
})

test_that("mix of binary and continuous variables", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # make data
  b <- raster::cellStats(sim_pu_raster, "sum") * 0.2
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_max_utility_objective(b) %>%
       add_binary_decisions() %>%
       add_cbc_solver(verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "Raster"))
  expect_equal(terra::nlyr(s), 1)
  expect_equal(sort(unique(raster::getValues(s))), c(0, 1))
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("first_feasible", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p1 <- problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions() %>%
        add_cbc_solver(first_feasible = TRUE, verbose = FALSE)
  s1 <- solve(p1)
  # check that solution has correct properties
  expect_is(s1, "Raster")
  expect_equal(terra::nlyr(s1), 1)
  expect_equal(sort(unique(raster::getValues(s1))), c(0, 1))
  expect_true(raster::compareRaster(sim_pu_raster, s1, res = TRUE, crs = TRUE,
                                    tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("correct solution (simple)", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_locked_in_constraints(locked_in) %>%
       add_locked_out_constraints(locked_out) %>%
       add_cbc_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # test for correct solution
  expect_equal(raster::values(s1), c(0, 1, 1, NA))
  expect_equal(raster::values(s1), raster::values(s2))
})

test_that("correct solution (complex)", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # create data
  cost <- raster::raster(matrix(c(1000, 100, 200, 300, NA), nrow = 1))
  features <- raster::stack(
    raster::raster(matrix(c(5,  5,   0,  0,  NA), nrow = 1)),
    raster::raster(matrix(c(2,  0,   8,  10, NA), nrow = 1)),
    raster::raster(matrix(c(10, 100, 10, 10, NA), nrow = 1)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_manual_targets(
         tibble::tibble(
           feature = names(features),
           type = "absolute",
           sense = c("=", ">=", "<="),
           target = c(5, 10, 20))) %>%
       add_cbc_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # test for correct solution
  expect_equal(raster::values(s1), c(1, 0, 1, 0, NA))
  expect_equal(raster::values(s1), raster::values(s2))
})

test_that("start_solution", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  skip_if_not(
    any(grepl(
      "initial_solution", deparse1(args(rcbc::cbc_solve)), fixed = TRUE)),
    message = "newer version of rcbc R package required"
  )
  # create data
  cost <- raster::raster(matrix(c(1000, 100, 200, 300, NA), nrow = 1))
  features <- raster::stack(
    raster::raster(matrix(c(5,  5,   0,  0,  NA), nrow = 1)),
    raster::raster(matrix(c(2,  0,   8,  10, NA), nrow = 1)),
    raster::raster(matrix(c(10, 100, 10, 10, NA), nrow = 1)))
  start_valid <- raster::raster(matrix(c(1, 0, 1, 0, NA), nrow = 1))
  start_invalid <- raster::raster(matrix(c(0, 0, 0, 0, NA), nrow = 1))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_manual_targets(
         tibble::tibble(
           feature = names(features),
           type = "absolute",
           sense = c("=", ">=", "<="),
           target = c(5, 10, 20))) %>%
       add_binary_decisions()
  # create solution
  s1 <-
    p %>%
    add_cbc_solver(gap = 0, verbose = FALSE) %>%
    solve()
  s2 <-
    p %>%
    add_cbc_solver(
      gap = 0, verbose = FALSE, start_solution = start_valid) %>%
    solve()
  s3 <-
    p %>%
    add_cbc_solver(
      gap = 0, verbose = FALSE, start_solution = start_invalid) %>%
    solve()
  # test for correct solution
  expect_equal(raster::values(s1), c(1, 0, 1, 0, NA))
  expect_equal(raster::values(s1), raster::values(s2))
  expect_equal(raster::values(s1), raster::values(s3))
})

test_that("correct solution (last rij value = 0)", {
  skip_on_cran()
  skip_if_not_installed("rcbc")
  # create data
  cost <- raster::raster(matrix(c(1000, 100, 200, 300, 1), nrow = 1))
  features <- raster::stack(
    raster::raster(matrix(c(5,  5,   0,  0,  0), nrow = 1)),
    raster::raster(matrix(c(2,  0,   8,  10, 0), nrow = 1)),
    raster::raster(matrix(c(10, 100, 10, 10, 0), nrow = 1)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_manual_targets(
         tibble::tibble(
           feature = names(features),
           type = "absolute",
           sense = c("=", ">=", "<="),
           target = c(5, 10, 20))) %>%
       add_cbc_solver(gap = 0, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # test for correct solution
  expect_equal(raster::values(s1), c(1, 0, 1, 0, 0))
  expect_equal(raster::values(s1), raster::values(s2))})
