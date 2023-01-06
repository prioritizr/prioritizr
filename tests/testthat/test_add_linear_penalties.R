context("add_linear_penalties")

test_that("minimum set objective (numeric, compile, single zone)", {
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # make and compile problem
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_penalties(3, c(terra::values(sim_features[[1]])) * 8)
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations
  pu_idx <- p1$planning_unit_indices()
  pen <- 3 * sim_features[[1]][pu_idx] * 8
  # tests
  expect_equal(o2$obj(), o1$obj() + pen)
  expect_equal(o2$A(), o1$A())
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), o1$rhs())
  expect_equal(o2$sense(), o1$sense())
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (matrix, compile, single zone)", {
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # make and compile problem
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_penalties(
      3, matrix(terra::values(sim_features[[1]]) * 8, ncol = 1))
  p3 <-
    p1 %>%
    add_linear_penalties(
      3,
      as_Matrix(
        matrix(terra::values(sim_features[[1]]) * 8, ncol = 1),
        "dgCMatrix"))
  o1 <- compile(p1)
  o2 <- compile(p2)
  o3 <- compile(p3)
  # calculations
  pu_idx <- p1$planning_unit_indices()
  pen <- 3 * sim_features[[1]][pu_idx] * 8
  # tests
  expect_equal(o2$obj(), o1$obj() + pen)
  expect_equal(o2$A(), o1$A())
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), o1$rhs())
  expect_equal(o2$sense(), o1$sense())
  expect_equal(o2$modelsense(), o1$modelsense())
  expect_equal(as.list(o2), as.list(o3))
})

test_that("minimum set objective (raster, compile, single zone)", {
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # make and compile problem
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_penalties(3, sim_features[[1]] * 8)
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations
  pu_idx <- p1$planning_unit_indices()
  pen <- 3 * sim_features[[1]][pu_idx] * 8
  # tests
  expect_equal(o2$obj(), o1$obj() + pen)
  expect_equal(o2$A(), o1$A())
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), o1$rhs())
  expect_equal(o2$sense(), o1$sense())
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (character/Spatial, compile, single zone)", {
  # make data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  sim_pu_polygons$penalty_data <- runif(nrow(sim_pu_polygons))  * 5
  # make and compile problem
  p1 <-
    problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_penalties(3, "penalty_data")
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations
  pu_idx <- p1$planning_unit_indices()
  pen <- 3 * sim_pu_polygons$penalty_data[pu_idx]
  # tests
  expect_equal(o2$obj(), o1$obj() + pen)
  expect_equal(o2$A(), o1$A())
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), o1$rhs())
  expect_equal(o2$sense(), o1$sense())
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (character/sf, compile, single zone)", {
  # make data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  sim_pu_polygons$penalty_data <- runif(nrow(sim_pu_polygons))  * 5
  # make and compile problem
  p1 <-
    problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_penalties(3, "penalty_data")
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations
  pu_idx <- p1$planning_unit_indices()
  pen <- 3 * sim_pu_polygons$penalty_data[pu_idx]
  # tests
  expect_equal(o2$obj(), o1$obj() + pen)
  expect_equal(o2$A(), o1$A())
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), o1$rhs())
  expect_equal(o2$sense(), o1$sense())
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (character/data.frame, compile, single zone)",
 {
  # make data
  pu <- data.frame(id = seq_len(10), cost = c(runif(1), NA, runif(8)),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA),
                   penalty_data = runif(10) * 5)
  # make and compile problem
  p1 <-
    problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_penalties(3, "penalty_data")
  o1 <- compile(p1)
  o2 <- compile(p2)
  pu_idx <- p1$planning_unit_indices()
  pen <- 3 * pu$penalty_data[pu_idx]
  # tests
  expect_equal(o2$obj(), o1$obj() + pen)
  expect_equal(o2$A(), o1$A())
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), o1$rhs())
  expect_equal(o2$sense(), o1$sense())
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  cost <- terra::rast(matrix(c(3, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- terra::rast(terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
                            terra::rast(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_linear_penalties(10, c(0, 1, 1, 0)) %>%
       add_absolute_targets(c(2, 10)) %>%
       add_locked_in_constraints(locked_in) %>%
       add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # test for correct solution
  expect_equal(terra::values(s), c(1, 1, 0, NA))
})

test_that("minimum set objective (matrix, compile, multiple zones)", {
  # make data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  m <- terra::values(sim_features[[seq_len(3)]])
  targ <- matrix(runif(number_of_features(sim_zones_features) *
                       number_of_zones(sim_zones_features)) * 10,
                 nrow = number_of_features(sim_zones_features),
                 ncol = number_of_zones(sim_zones_features))
  # make and compile problem
  p1 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_penalties(c(3, 5, 7), m)
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations
  pu_idx <- p1$planning_unit_indices()
  pen <- m[pu_idx, , drop = FALSE]
  for (i in seq_len(ncol(pen))) pen[, i ] <- pen[, i] * c(3, 5, 7)[i]
  # tests
  expect_equal(o2$obj(), o1$obj() + c(pen))
  expect_equal(o2$A(), o1$A())
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), o1$rhs())
  expect_equal(o2$sense(), o1$sense())
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (raster, compile, multiple zones)", {
  # make data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  m <- terra::values(sim_features[[seq_len(3)]])
  targ <- matrix(runif(number_of_features(sim_zones_features) *
                       number_of_zones(sim_zones_features)) * 10,
                 nrow = number_of_features(sim_zones_features),
                 ncol = number_of_zones(sim_zones_features))
  # make and compile problem
  p1 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_penalties(c(3, 5, 7), sim_features[[seq_len(3)]])
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations
  pu_idx <- p1$planning_unit_indices()
  pen <- m[pu_idx, , drop = FALSE]
  for (i in seq_len(ncol(pen))) pen[, i ] <- pen[, i ] * c(3, 5, 7)[i]
  # tests
  expect_equal(o2$obj(), o1$obj() + c(pen))
  expect_equal(o2$A(), o1$A())
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), o1$rhs())
  expect_equal(o2$sense(), o1$sense())
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (character/Spatial, compile, multiple zones)",
  {
  # make data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  sim_zones_pu_polygons$p1 <- runif(nrow(sim_zones_pu_polygons)) * 5
  sim_zones_pu_polygons$p2 <- runif(nrow(sim_zones_pu_polygons)) * 6
  sim_zones_pu_polygons$p3 <- runif(nrow(sim_zones_pu_polygons)) * 9
  targ <- matrix(runif(number_of_features(sim_zones_features) *
                       number_of_zones(sim_zones_features)) * 10,
                 nrow = number_of_features(sim_zones_features),
                 ncol = number_of_zones(sim_zones_features))
  # make and compile problem
  p1 <-
    problem(sim_zones_pu_polygons, sim_zones_features,
            cost_column = c("cost_1", "cost_2", "cost_3")) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_penalties(c(2, 9, 4), c("p1", "p2", "p3"))
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations
  pu_idx <- p1$planning_unit_indices()
  pen <- sim_zones_pu_polygons@data[pu_idx, c("p1", "p2", "p3"), drop = FALSE]
  pen <- as.matrix(pen)
  for (i in seq_len(ncol(pen))) pen[, i ] <- pen[, i] * c(2, 9, 4)[i]
  # tests
  expect_equal(o2$obj(), o1$obj() + c(pen))
  expect_equal(o2$A(), o1$A())
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), o1$rhs())
  expect_equal(o2$sense(), o1$sense())
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (character/sf, compile, multiple zones)", {
  # make data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()

  sim_zones_pu_polygons$p1 <- runif(nrow(sim_zones_pu_polygons)) * 5
  sim_zones_pu_polygons$p2 <- runif(nrow(sim_zones_pu_polygons)) * 6
  sim_zones_pu_polygons$p3 <- runif(nrow(sim_zones_pu_polygons)) * 9
  targ <- matrix(runif(number_of_features(sim_zones_features) *
                       number_of_zones(sim_zones_features)) * 10,
                 nrow = number_of_features(sim_zones_features),
                 ncol = number_of_zones(sim_zones_features))
  # make and compile problem
  p1 <-
    problem(sim_zones_pu_polygons, sim_zones_features,
            cost_column = c("cost_1", "cost_2", "cost_3")) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_penalties(c(2, 9, 4), c("p1", "p2", "p3"))
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations
  pu_idx <- p1$planning_unit_indices()
  pen <- sf::st_drop_geometry(sim_zones_pu_polygons)
  pen <- pen[pu_idx, c("p1", "p2", "p3"), drop = FALSE]
  pen <- as.matrix(pen)
  for (i in seq_len(ncol(pen))) pen[, i ] <- pen[, i ] * c(2, 9, 4)[i]
  # tests
  expect_equal(o2$obj(), o1$obj() + c(pen))
  expect_equal(o2$A(), o1$A())
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), o1$rhs())
  expect_equal(o2$sense(), o1$sense())
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that(paste("minimum set objective (character/data.frame, compile,",
                "multiple zones)"), {
  # make data
  pu <- data.frame(id = seq_len(10), cost_1 = c(NA, NA, runif(8)),
                   cost_2 = c(0.3, NA, runif(8)),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10),
                   p1 = runif(10) * 2, p2 = runif(10) * 4)
  targ <- matrix(runif(4) * 3, 2, 2)
  # make problem and compile
  p1 <-
    problem(pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
            cost_column = c("cost_1", "cost_2")) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_penalties(c(3, 8), c("p1", "p2"))
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations
  pu_idx <- p1$planning_unit_indices()
  pen <- as.matrix(pu[pu_idx, c("p1", "p2"), drop = FALSE])
  pen[, 1] <- pen[, 1] * 3
  pen[, 2] <- pen[, 2] * 8
  pen <- unname(c(pen))
  # tests
  expect_equal(o2$obj(), o1$obj() + c(pen))
  expect_equal(o2$A(), o1$A())
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), o1$rhs())
  expect_equal(o2$sense(), o1$sense())
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  costs <- terra::rast(
    terra::rast(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7)),
    terra::rast(matrix(c(10, 10, 10, 10,  4,   1, NA), ncol = 7)))
  spp <- terra::rast(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7)))
  # create problem
  p <- problem(costs, zones(spp[[1:2]], spp[[3:4]])) %>%
       add_min_set_objective() %>%
       add_linear_penalties(c(100, 150), matrix(ncol = 2, c(
         10, 0, 0, 0, 0, 0, 0,
         0,  0, 0, 0, 10, 0, 0))) %>%
       add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- p %>% solve()
  # tests
  expect_is(s, "RasterStack")
  expect_equal(terra::values(s[[1]]), c(0, 1, NA, 1, 0, 0, NA))
  expect_equal(terra::values(s[[2]]), c(1, 0, 0,  0, 0, 0, NA))
})

test_that("invalid inputs", {
  expect_error({
    sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
    problem(sim_pu_raster, sim_features) %>%
    add_linear_penalties(NA_real_, sim_features[[1]])
  })
  expect_error({
    sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
    problem(sim_pu_raster, sim_features) %>%
    add_linear_penalties(c(3, 3), sim_features[[1]])
  })
  expect_error({
    sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
    problem(sim_pu_raster, sim_features) %>%
    add_linear_penalties(c(3, 3), sim_features[[1:2]])
  })
  expect_error({
    sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
    problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
    add_linear_penalties(3, c("cost", "cost"))
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
    add_linear_penalties(3, "a")
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
    add_linear_penalties(3,
      terra::crop(sim_features[[1]], raster::extent(c(0, 0.5, 0, 0.5))))
  })
})
