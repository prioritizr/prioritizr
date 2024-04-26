test_that("x = SpatRaster, y = SpatRaster (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- terra::rast(matrix(c(1, 2, NA, 3), ncol = 4))
  spp <- c(
    terra::rast(matrix(c(1, 2, 0, 0), ncol = 4)),
    terra::rast(matrix(c(NA, 0, 1, 1), ncol = 4))
  )
  names(spp) <- make.unique(names(spp))
  # solve problem
  s <-
    problem(costs, spp) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 1)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_inherits(s, "SpatRaster")
  expect_equal(c(terra::values(s)), c(1, 0, NA, 1))
  expect_true(is_comparable_raster(s, costs))
})

test_that("x = SpatRaster, y = ZonesSpatRaster (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- c(
    terra::rast(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7)),
    terra::rast(matrix(c(10, 10, 10, 10,  4,   1, NA), ncol = 7))
  )
  spp <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7))
  )
  names(spp) <- make.unique(names(spp))
  # create and solve problem
  s <-
    problem(costs, zones(spp[[1:2]], spp[[3:4]])) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_inherits(s, "SpatRaster")
  expect_equal(c(terra::values(s[[1]])), c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(c(terra::values(s[[2]])), c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x = sf, y = SpatRaster (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- terra::rast(matrix(1:4, byrow = TRUE, ncol = 2))
  costs <- sf::st_as_sf(terra::as.polygons(costs))
  costs$cost <- c(1, 2, NA, 3)
  spp <- c(
    terra::rast(matrix(c(1, 2, 0, 0), byrow = TRUE, ncol = 2)),
    terra::rast(matrix(c(NA, 0, 1, 1), byrow = TRUE, ncol = 2))
  )
  names(spp) <- make.unique(names(spp))
  # create and solve problem
  s <-
    problem(costs, spp, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 1)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_inherits(s, "sf")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

test_that("x = sf, y = ZonesSpatRaster (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- terra::rast(matrix(1:7, ncol = 7))
  costs <- sf::st_as_sf(terra::as.polygons(costs))
  costs$cost_1 <- c(1,  2,  NA, 3, 100, 100, NA)
  costs$cost_2 <- c(10, 10, 10, 10,  4,   1, NA)
  spp <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7))
  )
  # create and solve problem
  s <-
    problem(
      costs, zones(spp[[1:2]], spp[[3:4]]),
      cost_column = c("cost_1", "cost_2")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_inherits(s, "sf")
  expect_equal(s$solution_1_1, c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s$solution_1_2, c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x = sf, y = character (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- terra::rast(matrix(1:4, byrow = TRUE, ncol = 2))
  costs <- sf::st_as_sf(terra::as.polygons(costs))
  costs$cost <- c(1, 2, NA, 3)
  costs$spp1 <- c(1, 2, 0, 0)
  costs$spp2 <- c(NA, 0, 1, 1)
  # create and solve problem
  s <-
    problem(costs, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 1)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_inherits(s, "sf")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

test_that("x = sf, y = ZonesCharacter (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- terra::rast(matrix(1:7, byrow = TRUE, ncol = 7))
  costs <- sf::st_as_sf(terra::as.polygons(costs))
  costs$cost_1 <- c(1,  2,  NA, 3, 100, 100, NA)
  costs$cost_2 <- c(10, 10, 10, 10,  4,   1, NA)
  costs$spp1_z1 <- c(1,  2, 0, 0, 0, 0,  0)
  costs$spp2_z1 <- c(NA, 0, 1, 1, 0, 0,  0)
  costs$spp1_z2 <- c(1,  0, 0, 0, 1, 0,  0)
  costs$spp2_z2 <- c(0,  0, 0, 0, 0, 10, 0)
  # create and solve problem
  s <-
    problem(
      costs,
      zones(c("spp1_z1", "spp2_z1"), c("spp1_z2", "spp2_z2")),
      cost_column  = c("cost_1", "cost_2")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_inherits(s, "sf")
  expect_equal(s$solution_1_1, c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s$solution_1_2, c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x = sf, y = RasterStack (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- terra::rast(matrix(1:4, byrow = TRUE, ncol = 2))
  costs <- sf::st_as_sf(terra::as.polygons(costs))
  costs$cost <- c(1, 2, NA, 3)
  spp <- c(
    terra::rast(matrix(c(1, 2, 0, 0), byrow = TRUE, ncol = 2)),
    terra::rast(matrix(c(NA, 0, 1, 1), byrow = TRUE, ncol = 2))
  )
  # create and solve problem
  expect_warning(
    s <-
      problem(costs, raster::stack(spp), cost_column = "cost") %>%
      add_min_set_objective() %>%
      add_absolute_targets(c(1, 1)) %>%
      add_binary_decisions() %>%
      add_default_solver(gap = 0, verbose = FALSE) %>%
      solve_fixed_seed(),
    "deprecated"
  )
  # tests
  expect_inherits(s, "sf")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

test_that("x = sf, y = ZonesRaster (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  costs <- terra::rast(matrix(1:7, byrow = TRUE, ncol = 7))
  costs <- sf::st_as_sf(terra::as.polygons(costs))
  costs$cost_1 <- c(1,  2,  NA, 3, 100, 100, NA)
  costs$cost_2 <- c(10, 10, 10, 10,  4,   1, NA)
  spp <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7))
  )
  # solve problem
  expect_warning(
    expect_warning(
      s <-
        problem(
          costs,
          zones(raster::stack(spp[[1:2]]), raster::stack(spp[[3:4]])),
          cost_column = c("cost_1", "cost_2")
        ) %>%
        add_min_set_objective() %>%
        add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
        add_binary_decisions() %>%
        add_default_solver(gap = 0, verbose = FALSE) %>%
        solve_fixed_seed(),
      "deprecated"
    ),
    "deprecated"
  )
  # tests
  expect_inherits(s, "sf")
  expect_equal(s$solution_1_1, c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s$solution_1_2, c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x = data.frame, y = data.frame (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(id = seq_len(4), cost = c(1, 2, NA, 3))
  species <- data.frame(id = seq_len(2), name = letters[1:2])
  rij <- data.frame(
    pu = rep(1:4, 2),
    species = rep(1:2, each = 4),
    amount = c(1, 2, 0, 0, 0, 0, 1, 1)
  )
  # create and solve problem
  s <-
    problem(pu, species, "cost", rij = rij) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 1)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # run tests
  expect_inherits(s, "data.frame")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

test_that("x = data.frame, y = data.frame (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # simulate data
  costs <- data.frame(
    id = seq_len(7),
    cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
    cost_2 = c(10, 10, 10, 10,  4,   1, NA)
  )
  spp <- data.frame(id = 1:2, name = c("spp1", "spp2"))
  zone <- data.frame(id = 1:2, name = c("z1", "z2"))
  rij <- data.frame(
    pu = rep(1:7, 4),
    species = rep(rep(1:2, each = 7), 2),
    zone = rep(1:2, each = 14),
    amount = c(
      1,  2, 0, 0, 0, 0,  0,
      NA, 0, 1, 1, 0, 0,  0,
      1,  0, 0, 0, 1, 0,  0,
      0,  0, 0, 0, 0, 10, 0
    )
  )
  rij <- rij[!is.na(rij$amount), , drop = FALSE]
  # solve problem
  s <-
    problem(
      costs,
      spp,
      rij = rij, zone,
      cost_column = c("cost_1", "cost_2")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_inherits(s, "data.frame")
  expect_equal(s$solution_1_z1, c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s$solution_1_z2, c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x = numeric, y = data.frame (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(id = seq_len(4), cost = c(1, NA, 1000, 3))
  species <- data.frame(id = seq_len(2), name = letters[1:2])
  rij <- matrix(c(1, 2, 0, 0, NA, 0, 1, 1), byrow = TRUE, nrow = 2)
  # create and solve problem
  s <-
    problem(pu$cost, species, rij = rij) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 1)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # run tests
  expect_inherits(s, "numeric")
  expect_equal(c(s), c(1, NA, 0, 1))
})

test_that("x = matrix, y = data.frame (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- data.frame(
    id = seq_len(7),
    cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
    cost_2 = c(10, 10, 10, 10,  4,   1, NA),
    spp1_z1 = c(1,  2, 0, 0, 0, 0,  0),
    spp2_z1 = c(NA, 0, 1, 1, 0, 0,  0),
    spp1_z2 = c(1,  0, 0, 0, 1, 0,  0),
    spp2_z2 = c(0,  0, 0, 0, 0, 10, 0)
  )
  spp <- data.frame(id = 1:2, name = c("spp1", "spp2"))
  rij_matrix <- list(
    z1 = t(as.matrix(costs[, c("spp1_z1", "spp2_z1")])),
    z2 = t(as.matrix(costs[, c("spp1_z2", "spp2_z2")]))
  )
  # create and solve problem
  s <-
    problem(
      as.matrix(costs[, c("cost_1", "cost_2")]), spp,
      rij_matrix
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_inherits(s, "matrix")
  expect_equal(s[, "z1"], c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s[, "z2"], c(0, 0, 0,  0, 1, 0, NA))
})

test_that("numerical instability (error when force = FALSE)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # update data
  sim_pu_polygons$cost[1] <- 1e+35
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # tests
  expect_tidy_error(
    expect_warning(solve_fixed_seed(p), "numerical issues"),
    "failed presolve checks"
  )
})

test_that("infeasibility (error when force = FALSE)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.99) %>%
    add_linear_constraints(0, "<=", sim_pu_raster) %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # tests
  expect_tidy_error(solve_fixed_seed(p))
})

test_that("numerical instability (solution when force = TRUE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # update data
  sim_pu_polygons$cost[1] <- 1e+20
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_default_solver(first_feasible = TRUE, verbose = FALSE)
  # solve problem
  expect_warning(
    s <- solve_fixed_seed(p, force = TRUE),
    "numerical issues"
  )
  # tests
  expect_inherits(s, "sf")
})

test_that("x = RasterLayer, y = RasterStack (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- terra::rast(matrix(c(1, 2, NA, 3), ncol = 4))
  spp <- c(
    terra::rast(matrix(c(1, 2, 0, 0), ncol = 4)),
    terra::rast(matrix(c(NA, 0, 1, 1), ncol = 4))
  )
  names(spp) <- make.unique(names(spp))
  # create and solve problem
  expect_warning(
    s <-
      problem(raster::raster(costs), raster::stack(spp)) %>%
      add_min_set_objective() %>%
      add_absolute_targets(c(1, 1)) %>%
      add_binary_decisions() %>%
      add_default_solver(gap = 0, verbose = FALSE) %>%
      solve_fixed_seed(),
    "deprecated"
  )
  # tests
  expect_inherits(s, "RasterLayer")
  expect_equal(c(raster::values(s)), c(1, 0, NA, 1))
  expect_true(is_comparable_raster(s, raster::raster(costs)))
})

test_that("x = RasterStack, y = ZonesRaster (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- c(
    terra::rast(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7)),
    terra::rast(matrix(c(10, 10, 10, 10,  4,   1, NA), ncol = 7))
  )
  spp <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7))
  )
  names(spp) <- make.unique(names(spp))
  # create and solve problem
  expect_warning(
    expect_warning(
      s <-
        problem(
          raster::stack(costs),
          zones(raster::stack(spp[[1:2]]), raster::stack(spp[[3:4]]))
        ) %>%
        add_min_set_objective() %>%
        add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
        add_binary_decisions() %>%
        add_default_solver(gap = 0, verbose = FALSE) %>%
        solve_fixed_seed(),
      "deprecated"
    ),
    "deprecated"
  )
  # tests
  expect_inherits(s, "RasterStack")
  expect_equal(c(raster::values(s[[1]])), c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(c(raster::values(s[[2]])), c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x = Spatial, y = character (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  costs <- terra::rast(matrix(1:4, byrow = TRUE, ncol = 4))
  costs <- sf::st_as_sf(terra::as.polygons(costs))
  costs$cost <- c(1, 2, NA, 3)
  costs$spp1 <- c(1, 2, 0, 0)
  costs$spp2 <- c(NA, 0, 1, 1)
  # solve problem
  expect_warning(
    s <-
      problem(
        sf::as_Spatial(costs), c("spp1", "spp2"), cost_column = "cost"
      ) %>%
      add_min_set_objective() %>%
      add_absolute_targets(c(1, 1)) %>%
      add_binary_decisions() %>%
      add_default_solver(gap = 0, verbose = FALSE) %>%
      solve_fixed_seed(),
    "deprecated"
  )
  # tests
  expect_inherits(s, "SpatialPolygonsDataFrame")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

test_that("x = Spatial, y = ZonesCharacter (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- terra::rast(matrix(1:7, byrow = TRUE, ncol = 7))
  costs <- sf::st_as_sf(terra::as.polygons(costs))
  costs$cost_1 <- c(1,  2,  NA, 3, 100, 100, NA)
  costs$cost_2 <- c(10, 10, 10, 10,  4,   1, NA)
  costs$spp1_z1 <- c(1,  2, 0, 0, 0, 0,  0)
  costs$spp2_z1 <- c(NA, 0, 1, 1, 0, 0,  0)
  costs$spp1_z2 <- c(1,  0, 0, 0, 1, 0,  0)
  costs$spp2_z2 <- c(0,  0, 0, 0, 0, 10, 0)
  # create and solve problem
  expect_warning(
    s <-
      problem(
        sf::as_Spatial(costs),
        zones(c("spp1_z1", "spp2_z1"), c("spp1_z2", "spp2_z2")),
        cost_column  = c("cost_1", "cost_2")
      ) %>%
      add_min_set_objective() %>%
      add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
      add_binary_decisions() %>%
      add_default_solver(gap = 0, verbose = FALSE) %>%
      solve_fixed_seed(),
    "deprecated"
  )
  # tests
  expect_inherits(s, "SpatialPolygonsDataFrame")
  expect_equal(s$solution_1_1, c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s$solution_1_2, c(0, 0, 0,  0, 1, 0, NA))
})

test_that("x = Spatial, y = RasterStack (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- terra::rast(matrix(1:4, byrow = TRUE, ncol = 2))
  costs <- sf::st_as_sf(terra::as.polygons(costs))
  costs$cost <- c(1, 2, NA, 3)
  spp <- c(
    terra::rast(matrix(c(1, 2, 0, 0), byrow = TRUE, ncol = 2)),
    terra::rast(matrix(c(NA, 0, 1, 1), byrow = TRUE, ncol = 2))
  )
  # create and solve problem
  expect_warning(
    expect_warning(
      s <-
        problem(
          sf::as_Spatial(costs), raster::stack(spp), cost_column = "cost"
        ) %>%
        add_min_set_objective() %>%
        add_absolute_targets(c(1, 1)) %>%
        add_binary_decisions() %>%
        add_default_solver(gap = 0, verbose = FALSE) %>%
        solve_fixed_seed(),
      "deprecated"
    ),
    "deprecated"
  )
  # tests
  expect_inherits(s, "SpatialPolygonsDataFrame")
  expect_equal(s$solution_1, c(1, 0, NA, 1))
})

test_that("x = Spatial, y = ZonesRaster (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make data
  costs <- terra::rast(matrix(1:7, byrow = TRUE, ncol = 7))
  costs <- sf::st_as_sf(terra::as.polygons(costs))
  costs$cost_1 <- c(1,  2,  NA, 3, 100, 100, NA)
  costs$cost_2 <- c(10, 10, 10, 10,  4,   1, NA)
  spp <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7))
  )
  # solve problem
  expect_warning(
    expect_warning(
      expect_warning(
        s <-
          problem(
            sf::as_Spatial(costs),
            zones(raster::stack(spp[[1:2]]), raster::stack(spp[[3:4]])),
            cost_column = c("cost_1", "cost_2")
          ) %>%
          add_min_set_objective() %>%
          add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
          add_binary_decisions() %>%
          add_default_solver(gap = 0, verbose = FALSE) %>%
          solve_fixed_seed(),
        "deprecated"
      ),
      "deprecated"
    ),
    "deprecated"
  )
  # tests
  expect_inherits(s, "SpatialPolygonsDataFrame")
  expect_equal(s$solution_1_1, c(1, 0, NA, 1, 0, 0, NA))
  expect_equal(s$solution_1_2, c(0, 0, 0,  0, 1, 0, NA))
})
