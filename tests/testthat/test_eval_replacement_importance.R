context("eval_replacement_importance")

test_that("numeric", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(id = seq_len(4),
                   cost = c(10, 2, NA, 3),
                   spp1 = c(0, 0, 0, 1),
                   spp2 = c(10, 5, 10, 6))
  # create problem
  p <-
    problem(pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
            as.matrix(t(pu[, 3:4]))) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- c(0, 1, NA, 1)
  # calculate replacement costs
  r <- eval_replacement_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- c(0, 8, NA, Inf)
  # run tests
  expect_equal(r, r2)
})

test_that("matrix (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(id = seq_len(4),
                   cost = c(10, 2, NA, 3),
                   spp1 = c(0, 0, 0, 1),
                   spp2 = c(10, 5, 10, 6))
  # create problem
  p <-
    problem(matrix(pu$cost, ncol = 1),
            data.frame(id = seq_len(2), name = c("spp1", "spp2")),
            as.matrix(t(pu[, 3:4]))) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- matrix(c(0, 1, NA, 1), ncol = 1)
  # calculate replacement costs
  r <- eval_replacement_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- matrix(c(0, 8, NA, Inf), ncol = 1)
  colnames(r2) <- "rc"
  # run tests
  expect_equal(r, r2)
})

test_that("matrix (single zone, rescale = TRUE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(id = seq_len(4),
                   cost = c(10, 2, NA, 3),
                   spp1 = c(0, 0, 0, 1),
                   spp2 = c(10, 5, 10, 6))
  # create problem
  p <-
    problem(matrix(pu$cost, ncol = 1),
            data.frame(id = seq_len(2), name = c("spp1", "spp2")),
            as.matrix(t(pu[, 3:4]))) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- matrix(c(0, 1, NA, 1), ncol = 1)
  # calculate replacement costs
  r <- eval_replacement_importance(p, s, rescale = TRUE)
  # create correct result
  r2 <- matrix(c(0, 0.505, NA, Inf), ncol = 1)
  colnames(r2) <- "rc"
  # run tests
  expect_equal(r, r2)
})

test_that("matrix (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(id = seq_len(8),
                   cost_1 = c(1,  2,  NA, 3, 100, 100, NA, 100),
                   cost_2 = c(10, 10, 10, 10,  4,   1, NA, 10),
                   spp1_1 = c(1,  2, 0, 0, 0, 0,  0, 0),
                   spp2_1 = c(NA, 0, 1, 1, 0, 0,  0, 0),
                   spp1_2 = c(1,  0, 0, 0, 1, 0,  0, 1),
                   spp2_2 = c(0,  0, 0, 0, 0, 10, 0, 0))
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  # create problem
  p <-
    problem(as.matrix(pu[, c(2, 3)]),
            data.frame(id = seq_len(2), name = c("spp1", "spp2")),
            list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- matrix(c(1, 0, NA, 1, 0, 0, NA, 0, 0, 0, 0, 0, 1, 0, NA, 0), ncol = 2)
  # calculate replacement costs
  r <- eval_replacement_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- matrix(c(1, 0, NA, Inf, 0, 0, NA, 0, 0, 0, 0, 0, 6, 0, NA, 0), ncol = 2)
  colnames(r2) <- c("rc_1", "rc_2")
  # run tests
  expect_equal(r, r2)
})

test_that("matrix (multiple zones, rescale = TRUE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(id = seq_len(8),
                   cost_1 = c(1,  2,  NA, 3, 100, 100, NA, 100),
                   cost_2 = c(10, 10, 10, 10,  4,   1, NA, 10),
                   spp1_1 = c(1,  2, 0, 0, 0, 0,  0, 0),
                   spp2_1 = c(NA, 0, 1, 1, 0, 0,  0, 0),
                   spp1_2 = c(1,  0, 0, 0, 1, 0,  0, 1),
                   spp2_2 = c(0,  0, 0, 0, 0, 10, 0, 0))
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  # create problem
  p <-
    problem(as.matrix(pu[, c(2, 3)]),
            data.frame(id = seq_len(2), name = c("spp1", "spp2")),
            list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- matrix(c(1, 0, NA, 1, 0, 0, NA, 0, 0, 0, 0, 0, 1, 0, NA, 0), ncol = 2)
  # calculate replacement costs
  r <- eval_replacement_importance(p, s, rescale = TRUE)
  # create correct result
  r2 <- matrix(c(0.01, 0, NA, Inf, 0, 0, NA, 0, 0, 0, 0, 0, 1, 0, NA, 0),
               ncol = 2)
  colnames(r2) <- c("rc_1", "rc_2")
  # run tests
  expect_equal(r, r2)
})

test_that("data.frame (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(id = seq_len(4),
                   cost = c(10, 2, NA, 3),
                   spp1 = c(0, 0, 0, 1),
                   spp2 = c(10, 5, 10, 6))
  # create problem
  p <-
    problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- tibble::tibble(solution = c(0, 1, NA, 1))
  # calculate replacement costs
  r <- eval_replacement_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- tibble::tibble(rc = c(0, 8, NA, Inf))
  # run tests
  expect_equal(r, r2)
})

test_that("data.frame (multiple zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(id = seq_len(8),
                   cost_1 = c(1,  2,  NA, 3, 100, 100, NA, 100),
                   cost_2 = c(10, 10, 10, 10,  4,   1, NA, 10),
                   spp1_1 = c(1,  2, 0, 0, 0, 0,  0, 0),
                   spp2_1 = c(NA, 0, 1, 1, 0, 0,  0, 0),
                   spp1_2 = c(1,  0, 0, 0, 1, 0,  0, 1),
                   spp2_2 = c(0,  0, 0, 0, 0, 10, 0, 0))
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  # create problem
  p <-
    problem(pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
            c("cost_1", "cost_2")) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- data.frame(cost_1 = c(1, 0, NA, 1, 0, 0, NA, 0),
                  cost_2 = c(0, 0, 0, 0, 1, 0, NA, 0))
  # calculate replacement costs
  r <- eval_replacement_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- tibble::tibble(rc_1 = c(1, 0, NA, Inf, 0, 0, NA, 0),
                       rc_2 = c(0, 0, 0, 0, 6, 0, NA, 0))
  # run tests
  expect_equal(r, r2)
})

test_that("Spatial (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  sim_pu_polygons <- get_sim_pu_polygons()
  pu <- sim_pu_polygons[1:4, ]
  pu@data <- data.frame(id = seq_len(4),
                        cost = c(10, 2, NA, 3),
                        spp1 = c(0, 0, 0, 1),
                        spp2 = c(10, 5, 10, 6))
  # create problem
  p <-
    problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  pu$solution <- c(0, 1, NA, 1)
  # calculate replacement costs
  r <- eval_replacement_importance(p, pu[, "solution"], rescale = FALSE)
  # create correct result
  pu$rc <- c(0, 8, NA, Inf)
  # run tests
  expect_equivalent(r, pu[, "rc"])
})

test_that("Spatial (multiple zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  sim_pu_polygons <- get_sim_pu_polygons()
  pu <- sim_pu_polygons[1:8, ]
  pu@data <- data.frame(id = seq_len(8),
                        cost_1 = c(1,  2,  NA, 3, 100, 100, NA, 100),
                        cost_2 = c(10, 10, 10, 10,  4,   1, NA, 10),
                        spp1_1 = c(1,  2, 0, 0, 0, 0,  0, 0),
                        spp2_1 = c(NA, 0, 1, 1, 0, 0,  0, 0),
                        spp1_2 = c(1,  0, 0, 0, 1, 0,  0, 1),
                        spp2_2 = c(0,  0, 0, 0, 0, 10, 0, 0))
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  # create problem
  p <-
    problem(pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
            c("cost_1", "cost_2")) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  pu$solution_1 <- c(1, 0, NA, 1, 0, 0, NA, 0)
  pu$solution_2 <- c(0, 0, 0, 0, 1, 0, NA, 0)
  # calculate replacement costs
  r <- eval_replacement_importance(p, pu[, c("solution_1", "solution_2")],
                        rescale = FALSE)
  # create correct result
  pu$rc_1 <- c(1, 0, NA, Inf, 0, 0, NA, 0)
  pu$rc_2 <- c(0, 0, 0, 0, 6, 0, NA, 0)
  # run tests
  expect_equivalent(r, pu[, c("rc_1", "rc_2")])
})

test_that("sf (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  sim_pu_polygons <- get_sim_pu_polygons()
  pu <- sim_pu_polygons[1:4, ]
  pu@data <- data.frame(id = seq_len(4),
                        cost = c(10, 2, NA, 3),
                        spp1 = c(0, 0, 0, 1),
                        spp2 = c(10, 5, 10, 6))
  pu <- sf::st_as_sf(pu)
  # create problem
  p <-
    problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  pu$solution <- c(0, 1, NA, 1)
  # calculate replacement costs
  r <- eval_replacement_importance(p, pu[, "solution"], rescale = FALSE)
  # create correct result
  pu$rc <- c(0, 8, NA, Inf)
  # run tests
  expect_equivalent(r, pu[, "rc"])
})

test_that("sf (multiple zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  sim_pu_polygons <- get_sim_pu_polygons()
  pu <- sim_pu_polygons[1:8, ]
  pu@data <- data.frame(id = seq_len(8),
                        cost_1 = c(1,  2,  NA, 3, 100, 100, NA, 100),
                        cost_2 = c(10, 10, 10, 10,  4,   1, NA, 10),
                        spp1_1 = c(1,  2, 0, 0, 0, 0,  0, 0),
                        spp2_1 = c(NA, 0, 1, 1, 0, 0,  0, 0),
                        spp1_2 = c(1,  0, 0, 0, 1, 0,  0, 1),
                        spp2_2 = c(0,  0, 0, 0, 0, 10, 0, 0))
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  pu <- sf::st_as_sf(pu)
  # create problem
  p <-
    problem(pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
            c("cost_1", "cost_2")) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  pu$solution_1 <- c(1, 0, NA, 1, 0, 0, NA, 0)
  pu$solution_2 <- c(0, 0, 0, 0, 1, 0, NA, 0)
  # calculate replacement costs
  r <- eval_replacement_importance(p, pu[, c("solution_1", "solution_2")],
                        rescale = FALSE)
  # create correct result
  pu$rc_1 <- c(1, 0, NA, Inf, 0, 0, NA, 0)
  pu$rc_2 <- c(0, 0, 0, 0, 6, 0, NA, 0)
  # run tests
  expect_equivalent(r, pu[, c("rc_1", "rc_2")])
})

test_that("Raster (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- raster::raster(matrix(c(10, 2, NA, 3), nrow = 1))
  features <- raster::stack(raster::raster(matrix(c(0, 0, 0, 1), nrow = 1)),
                            raster::raster(matrix(c(10, 5, 10, 6), nrow = 1)))
  # create problem
  p <-
    problem(pu, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- raster::raster(matrix(c(0, 1, NA, 1), nrow = 1))
  # calculate replacement costs
  r <- eval_replacement_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- raster::raster(matrix(c(0, 8, NA, Inf), nrow = 1))
  names(r2) <- "rc"
  # run tests
  expect_equal(r, r2)
})

test_that("Raster (multiple zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- raster::stack(
    raster::raster(matrix(c(1,  2,  NA, 3, 100, 100, NA, 100), nrow = 1)),
    raster::raster(matrix(c(10, 10, 10, 10,  4,   1, NA, 10), nrow = 1)))
  features <- raster::stack(
    raster::raster(matrix(c(1,  2, 0, 0, 0, 0,  0, 0), nrow = 1)),
    raster::raster(matrix(c(NA, 0, 1, 1, 0, 0,  0, 0), nrow = 1)),
    raster::raster(matrix(c(1,  0, 0, 0, 1, 0,  0, 1), nrow = 1)),
    raster::raster(matrix(c(0,  0, 0, 0, 0, 10, 0, 0), nrow = 1)))
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  # create problem
  p <-
    problem(pu, zones(features[[c(1, 2)]], features[[c(3, 4)]])) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- raster::stack(
    raster::raster(matrix(c(1, 0, NA, 1, 0, 0, NA, 0), nrow = 1)),
    raster::raster(matrix(c(0, 0, 0, 0, 1, 0, NA, 0), nrow = 1)))
  # calculate replacement costs
  r <- eval_replacement_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- raster::stack(
    raster::raster(matrix(c(1, 0, NA, Inf, 0, 0, NA, 0), nrow = 1)),
    raster::raster(matrix(c(0, 0, 0,  0,   6, 0, NA, 0), nrow = 1)))
  names(r2) <- c("rc_1", "rc_2")
  # run tests
  expect_equal(r, r2)
})

test_that("Raster (multiple threads)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # build problem
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.4) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0.1, verbose = FALSE)
  # find solution
  s <- solve(p)
  # calculate replacement costs without parallel processing
  r1 <- eval_replacement_importance(p, s)
  # calculate replacement costs with parallel processing
  suppressWarnings({r2 <- eval_replacement_importance(p, s, threads = 2)})
  # verify that parallel processing generates the same result
  expect_identical(as.matrix(r1), as.matrix(r2))
})

test_that("invalid inputs", {
  expect_error({
    # simulate data
    pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                     spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
    # create problem
    x <- problem(pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
                 as.matrix(t(pu[, 3:4])))
    # create a solution
    y <- rep(c(0, 1), 5)
    # calculate representation
    r <- eval_replacement_importance(x, y)
  })
  expect_error({
    # simulate data
    pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                     spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
    # create problem
    x <- problem(matrix(pu$cost, ncol = 1),
                 data.frame(id = seq_len(2), name = c("spp1", "spp2")),
                 as.matrix(t(pu[, 3:4])))
    # create a solution
    y <- matrix(rep(c(0, 1), 5), ncol = 1)
    # calculate representation
    r <- eval_replacement_importance(x, y)
  })
  expect_error({
    # simulate data
    pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                     spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
    # create problem
    x <- problem(pu, c("spp1", "spp2"), cost_column = "cost")
    # create a solution
    y <- data.frame(solution = rep(c(0, 1), 5))
    # calculate representation
    r <- eval_replacement_importance(x, y)
  })
  expect_error({
    # load data
    sim_pu_polygons <- get_sim_pu_polygons()
    pu <- sim_pu_polygons
    pu$cost[1:5] <- NA
    pu$solution <- rep(c(0, 1), 5)
    pu$spp1 <- runif(10)
    pu$spp2 <- c(rpois(9, 1), NA)
    # create problem
    x <- problem(pu, c("spp1", "spp2"), "cost")
    # create a solution
    y <- pu[, "solution"]
    # calculate representation
    r <- eval_replacement_importance(x, y)
  })
  expect_error({
    # load data
    sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
    # create problem
    x <- problem(sim_pu_raster, sim_features)
    # create a solution
    y <- raster::setValues(sim_pu_raster,
                           rep(c(0, 1), raster::ncell(sim_pu_raster) / 2))
    # calculate representation
    r <- eval_replacement_importance(x, y)
  })
})
