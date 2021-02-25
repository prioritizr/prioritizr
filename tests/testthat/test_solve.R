context("solve")

test_that("x=RasterLayer, y=RasterStack (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # simulate data
  costs <- raster::raster(matrix(c(1, 2, NA, 3), ncol = 4))
  spp <- raster::stack(raster::raster(matrix(c(1, 2, 0, 0), ncol = 4)),
                       raster::raster(matrix(c(NA, 0, 1, 1), ncol = 4)))
  # solve problem
  s <- problem(costs, spp) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1, 1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "RasterLayer")
  expect_equal(raster::values(s), c(1, 0, NA, 1))
  expect_true(raster::compareRaster(s, costs, stopiffalse = FALSE,
                                    tolerance = 1e-5))
})

test_that("x=RasterStack, y=ZonesRaster (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # simulate data
  costs <- raster::stack(
    raster::raster(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7)),
    raster::raster(matrix(c(10, 10, 10, 10,  4,   1, NA), ncol = 7)))
  spp <- raster::stack(
    raster::raster(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    raster::raster(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    raster::raster(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    raster::raster(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7)))
  # solve problem
  s <- problem(costs, zones(spp[[1:2]], spp[[3:4]])) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "RasterStack")
  expect_equal(raster::values(s[[1]]), c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(raster::values(s[[2]]), c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x=SpatialPolygonsDataFrame, y=RasterStack (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  costs <- raster::raster(matrix(1:4, byrow = TRUE, ncol = 2)) %>%
           as("SpatialPolygonsDataFrame")
  costs$cost <- c(1, 2, NA, 3)
  spp <- raster::stack(raster::raster(matrix(c(1, 2, 0, 0), byrow = TRUE,
                                             ncol = 2)),
                       raster::raster(matrix(c(NA, 0, 1, 1), byrow = TRUE,
                                             ncol = 2)))
  # solve problem
  s <- problem(costs, spp, cost_column = "cost") %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1, 1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "SpatialPolygonsDataFrame")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

test_that("x=SpatialPolygonsDataFrame, y=ZonesRaster (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  costs <- raster::raster(matrix(1:7, ncol = 7)) %>%
           as("SpatialPolygonsDataFrame")
  costs$cost_1 <- c(1,  2,  NA, 3, 100, 100, NA)
  costs$cost_2 <- c(10, 10, 10, 10,  4,   1, NA)
    spp <- raster::stack(
      raster::raster(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
      raster::raster(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
      raster::raster(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
      raster::raster(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7)))
  # solve problem
  s <- problem(costs, zones(spp[[1:2]], spp[[3:4]]),
               cost_column = c("cost_1", "cost_2")) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "SpatialPolygonsDataFrame")
  expect_equal(s$solution_1_1, c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s$solution_1_2, c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x=SpatialPolygonsDataFrame, y=character (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  costs <- raster::raster(matrix(1:4, byrow = 2, ncol = 2)) %>%
           as("SpatialPolygonsDataFrame")
  costs$cost <- c(1, 2, NA, 3)
  costs$spp1 <- c(1, 2, 0, 0)
  costs$spp2 <- c(NA, 0, 1, 1)
  # solve problem
  s <- problem(costs, c("spp1", "spp2"), cost_column = "cost") %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1, 1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "SpatialPolygonsDataFrame")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

test_that("x=SpatialPolygonsDataFrame, y=ZonesCharacter (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  costs <- raster::raster(matrix(1:7, ncol = 7)) %>%
           as("SpatialPolygonsDataFrame")
  costs$cost_1 <- c(1,  2,  NA, 3, 100, 100, NA)
  costs$cost_2 <- c(10, 10, 10, 10,  4,   1, NA)
  costs$spp1_z1 <- c(1,  2, 0, 0, 0, 0,  0)
  costs$spp2_z1 <- c(NA, 0, 1, 1, 0, 0,  0)
  costs$spp1_z2 <- c(1,  0, 0, 0, 1, 0,  0)
  costs$spp2_z2 <- c(0,  0, 0, 0, 0, 10, 0)
  # solve problem
  s <- problem(costs, zones(c("spp1_z1", "spp2_z1"), c("spp1_z2", "spp2_z2")),
               cost_column  = c("cost_1", "cost_2")) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "SpatialPolygonsDataFrame")
  expect_equal(s$solution_1_1, c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s$solution_1_2, c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x=sf, y=RasterStack (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  costs <- raster::raster(matrix(1:4, byrow = TRUE, ncol = 2)) %>%
           as("SpatialPolygonsDataFrame") %>%
           sf::st_as_sf()
  costs$cost <- c(1, 2, NA, 3)
  spp <- raster::stack(raster::raster(matrix(c(1, 2, 0, 0), byrow = TRUE,
                                             ncol = 2)),
                       raster::raster(matrix(c(NA, 0, 1, 1), byrow = TRUE,
                                             ncol = 2)))
  # solve problem
  s <- problem(costs, spp, cost_column = "cost") %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1, 1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "sf")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

test_that("x=sf, y=ZonesRaster (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  costs <- raster::raster(matrix(1:7, ncol = 7)) %>%
           as("SpatialPolygonsDataFrame") %>%
           sf::st_as_sf()
  costs$cost_1 <- c(1,  2,  NA, 3, 100, 100, NA)
  costs$cost_2 <- c(10, 10, 10, 10,  4,   1, NA)
    spp <- raster::stack(
      raster::raster(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
      raster::raster(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
      raster::raster(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
      raster::raster(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7)))
  # solve problem
  s <- problem(costs, zones(spp[[1:2]], spp[[3:4]]),
               cost_column = c("cost_1", "cost_2")) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "sf")
  expect_equal(s$solution_1_1, c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s$solution_1_2, c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x=sf, y=character (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  costs <- raster::raster(matrix(1:4, byrow = 2, ncol = 2)) %>%
           as("SpatialPolygonsDataFrame") %>%
           sf::st_as_sf()
  costs$cost <- c(1, 2, NA, 3)
  costs$spp1 <- c(1, 2, 0, 0)
  costs$spp2 <- c(NA, 0, 1, 1)
  # solve problem
  s <- problem(costs, c("spp1", "spp2"), cost_column = "cost") %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1, 1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "sf")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

test_that("x=sf, y=ZonesCharacter (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  costs <- raster::raster(matrix(1:7, ncol = 7)) %>%
           as("SpatialPolygonsDataFrame") %>%
           sf::st_as_sf()
  costs$cost_1 <- c(1,  2,  NA, 3, 100, 100, NA)
  costs$cost_2 <- c(10, 10, 10, 10,  4,   1, NA)
  costs$spp1_z1 <- c(1,  2, 0, 0, 0, 0,  0)
  costs$spp2_z1 <- c(NA, 0, 1, 1, 0, 0,  0)
  costs$spp1_z2 <- c(1,  0, 0, 0, 1, 0,  0)
  costs$spp2_z2 <- c(0,  0, 0, 0, 0, 10, 0)
  # solve problem
  s <- problem(costs, zones(c("spp1_z1", "spp2_z1"), c("spp1_z2", "spp2_z2")),
               cost_column  = c("cost_1", "cost_2")) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "sf")
  expect_equal(s$solution_1_1, c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s$solution_1_2, c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x=data.frame, y=data.frame (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # simulate data
  pu <- data.frame(id = seq_len(4), cost = c(1, 2, NA, 3))
  species <- data.frame(id = seq_len(2), name = letters[1:2])
  rij <- data.frame(pu = rep(1:4, 2), species = rep(1:2, each = 4),
                    amount = c(1, 2, 0, 0, 0, 0, 1, 1))
  # create problem
  s <- problem(pu, species, "cost", rij = rij) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1, 1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # run tests
  expect_is(s, "data.frame")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

test_that("x=data.frame, y=data.frame (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # simulate data
  costs <- data.frame(id = seq_len(7),
                      cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
                      cost_2 = c(10, 10, 10, 10,  4,   1, NA))
  spp <- data.frame(id = 1:2, name = c("spp1", "spp2"))
  zone <- data.frame(id = 1:2, name = c("z1", "z2"))
  rij <- data.frame(pu = rep(1:7, 4), species = rep(rep(1:2, each = 7), 2),
                    zone = rep(1:2, each = 14),
                    amount = c(1,  2, 0, 0, 0, 0,  0,
                               NA, 0, 1, 1, 0, 0,  0,
                               1,  0, 0, 0, 1, 0,  0,
                               0,  0, 0, 0, 0, 10, 0))
  rij <- rij[!is.na(rij$amount), ]
  # solve problem
  s <- problem(costs, spp, rij = rij, zone,
               cost_column = c("cost_1", "cost_2")) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "data.frame")
  expect_equal(s$solution_1_z1, c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s$solution_1_z2, c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x=numeric, y=data.frame (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # simulate data
  pu <- data.frame(id = seq_len(4), cost = c(1, NA, 1000, 3))
  species <- data.frame(id = seq_len(2), name = letters[1:2])
  rij <- matrix(c(1, 2, 0, 0, NA, 0, 1, 1), byrow = TRUE, nrow = 2)
  # create problem
  s <- problem(pu$cost, species, rij = rij) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1, 1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # run tests
  expect_is(s, "numeric")
  expect_equal(c(s), c(1, NA, 0, 1))
})

test_that("x=matrix, y=data.frame (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # simulate data
  costs <- data.frame(id = seq_len(7),
                      cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
                      cost_2 = c(10, 10, 10, 10,  4,   1, NA),
                      spp1_z1 = c(1,  2, 0, 0, 0, 0,  0),
                      spp2_z1 = c(NA, 0, 1, 1, 0, 0,  0),
                      spp1_z2 = c(1,  0, 0, 0, 1, 0,  0),
                      spp2_z2 = c(0,  0, 0, 0, 0, 10, 0))
  spp <- data.frame(id = 1:2, name = c("spp1", "spp2"))
  rij_matrix <- list(z1 = t(as.matrix(costs[, c("spp1_z1", "spp2_z1")])),
                     z2 = t(as.matrix(costs[, c("spp1_z2", "spp2_z2")])))
  # solve problem
  s <- problem(as.matrix(costs[, c("cost_1", "cost_2")]), spp,
               rij_matrix) %>%
       add_min_set_objective() %>%
       add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE) %>%
       solve()
  # tests
  expect_is(s, "matrix")
  expect_equal(s[, "z1"], c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s[, "z2"], c(0, 0, 0,  0, 1, 0, NA))
})

test_that("silent output when verbose=FALSE", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("Rsymphony")
  # simulate data
  costs <- raster::raster(matrix(c(1, 2, NA, 3), ncol = 4))
  spp <- raster::stack(raster::raster(matrix(c(1, 2, 0, 0), ncol = 4)),
                       raster::raster(matrix(c(NA, 0, 1, 1), ncol = 4)))
  # make problem
  p <- problem(costs, spp) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1, 1)) %>%
       add_binary_decisions() %>%
       add_rsymphony_solver(gap = 0, verbose = FALSE)
  # solve problem silently
  expect_silent(solve(p))
})

test_that("numerical instability (error when force = FALSE)", {
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons$cost[1] <- 1e+35
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_default_solver(time_limit = 5, verbose = FALSE)
  expect_warning(expect_error(solve(p)))
})

test_that("numerical instability (solution when force = TRUE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  data(sim_pu_polygons, sim_features)
  sim_pu_polygons$cost[1] <- 1e+35
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_default_solver(time_limit = 5, verbose = FALSE)
  expect_warning(expect_is(solve(p, force = TRUE), "SpatialPolygonsDataFrame"))
})
