context("solve")

test_that("x=Raster, y=RasterStack", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # simulate data
  costs <- raster::raster(matrix(c(1, 2, NA, 3), ncol = 4))
  spp <- raster::stack(raster::raster(matrix(c(1, 2, 0, 0), ncol = 4)),
                       raster::raster(matrix(c(NA, 0, 1, 1), ncol = 4)))
  # solve problem
  s <- problem(costs, spp) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1,1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0) %>%
       solve()
  # tests
  expect_is(s, "RasterLayer")
  expect_equal(raster::values(s), c(1, 0, NA, 1))
  expect_true(raster::compareRaster(s, costs, stopiffalse = FALSE,
                                    tolerance = 1e-5))
})

test_that("x=SpatialPolygonsDataFrame, y=RasterStack", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # make data
  costs <- raster::raster(matrix(1:4, byrow = TRUE, ncol = 2)) %>%
           as("SpatialPolygonsDataFrame")
  costs$cost <- c(1, 2, NA, 3)
  spp <- raster::stack(raster::raster(matrix(c(1, 2, 0, 0), byrow = TRUE,
                                             ncol = 2)),
                       raster::raster(matrix(c(NA,0,1,1), byrow = TRUE,
                                             ncol = 2)))
  # solve problem
  s <- problem(costs, spp, cost_column  = "cost") %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1,1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0) %>%
       solve()
  # tests
  expect_is(s, "SpatialPolygonsDataFrame")
  expect_equal(s$solution_1, c(1, 0, 1))
})

test_that("x=SpatialPolygonsDataFrame, y=character", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # make data
  costs <- raster::raster(matrix(1:4, byrow = TRUE, ncol = 2)) %>%
           as("SpatialPolygonsDataFrame")
  costs$cost <- c(1, 2, NA, 3)
  costs$spp1 <- c(1, 2, 0, 0)
  costs$spp2 <- c(NA, 0, 1, 1)
  # solve problem
  s <- problem(costs, c("spp1", "spp2"), cost_column  = "cost") %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1,1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0) %>%
       solve()
  # tests
  expect_is(s, "SpatialPolygonsDataFrame")
  expect_equal(s$solution_1, c(1, 0, 1))
})

test_that("x=data.frame, y=data.frame", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # simulate data
  pu <- data.frame(id = seq_len(4), cost = c(1, 2, 1000, 3))
  species <- data.frame(id = seq_len(2), name = letters[1:2])
  rij <- data.frame(pu = rep(1:4, 2), species = rep(1:2, each = 4),
                    amount = c(1, 2, 0, 0, 0, 0, 1, 1))
  # create problem
  s <- problem(pu, species, rij = rij) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1,1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0) %>%
       solve()
  # run tests
  expect_is(s, "data.frame")
  expect_equal(s$solution_1, c(1, 0, 0, 1))
})

test_that("x=numeric, y=data.frame", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # simulate data
  pu <- data.frame(id = seq_len(4), cost = c(1, 2, 1000, 3))
  species <- data.frame(id = seq_len(2), name = letters[1:2])
  rij <- matrix(c(1, 2, 0, 0, NA, 0, 1, 1), byrow = TRUE, nrow = 2)
  # create problem
  s <- problem(pu$cost, species, rij = rij) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(1,1)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0) %>%
       solve()
  # run tests
  expect_is(s, "numeric")
  expect_equal(c(s), c(1, 0, 0, 1))
})
