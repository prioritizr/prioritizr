context("add_linear_constraints")

test_that("minimum set objective (numeric, compile, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create constraint data
  sim_data <- terra::setValues(
    sim_features[[1]], runif(ncell(sim_pu_raster)) * 1000
  )
  # create problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_constraints(3.124, "<=", terra::values(sim_data))
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), sim_data[pu_idx][[1]]))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (matrix, compile, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create constraint data
  sim_data <- terra::setValues(
    sim_features[[1]], runif(ncell(sim_pu_raster)) * 1000
  )
  # create problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_constraints(
      3.124, "<=", matrix(terra::values(sim_data), ncol = 1)
    )
  p3 <-
    p1 %>%
    add_linear_constraints(
      3.124,
      "<=",
      as_Matrix(matrix(terra::values(sim_data), ncol = 1), "dgCMatrix")
    )
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  o3 <- compile(p3)
  # calculations
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), sim_data[pu_idx][[1]]))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
  expect_equal(as.list(o2), as.list(o3))
})

test_that("minimum set objective (SpatRaster, compile, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create constraint data
  sim_data <- terra::setValues(
    sim_features[[1]], runif(ncell(sim_pu_raster)) * 1000
  )
  # create problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_constraints(3.124, "<=", sim_data)
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), sim_data[pu_idx][[1]]))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (Raster, compile, single zone)", {
  # import data
  sim_pu_raster <- raster::stack(get_sim_pu_raster())
  sim_features <- raster::stack(get_sim_features())
  # create constraint data
  sim_data <- raster::setValues(
    sim_features[[1]], runif(ncell(sim_pu_raster)) * 1000
  )
  # create problems
  expect_warning(
    p1 <-
      problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions(),
    "deprecated"
  )
  expect_warning(
    p2 <-
      p1 %>%
      add_linear_constraints(3.124, "<=", sim_data),
    "deprecated"
  )
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), sim_data[pu_idx]))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (character/Spatial, compile, single zone)", {
  # import data
  sim_pu_polygons <- sf::as_Spatial(get_sim_pu_polygons())
  sim_features <- raster::stack(get_sim_features())
  # create constraints data
  sim_pu_polygons$constraint_data <- runif(nrow(sim_pu_polygons)) * 1000
  # create problems
  expect_warning(
    p1 <-
      problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions(),
    "deprecated"
  )
  p2 <-
    p1 %>%
    add_linear_constraints(3.124, "<=", "constraint_data")
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), sim_pu_polygons$constraint_data))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (character/sf, compile, single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create constraints data
  sim_pu_polygons$constraint_data <- runif(nrow(sim_pu_polygons)) * 1000
  # create problems
  p1 <-
    problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_constraints(3.124, "<=", "constraint_data")
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), sim_pu_polygons$constraint_data))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that(
  "minimum set objective (character/data.frame, compile, single zone)", {
  # make data
  pu <- data.frame(
    id = seq_len(10),
    cost = c(runif(1), NA, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA),
    constraint_data = runif(10) * 5
  )
  # create problems
  p1 <-
    problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_constraints(3.124, "<=", "constraint_data")
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), pu$constraint_data[pu_idx]))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  cost <- terra::rast(matrix(c(1, 3, 2, NA), ncol = 4))
  constraint <- terra::rast(matrix(c(5, 0.5, 1, 0.1), ncol = 4))
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_linear_constraints(3, "<=", constraint) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # test for correct solution
  expect_equal(c(terra::values(s)), c(0, 1, 1, NA))
})

test_that("minimum set objective (matrix, compile, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create constraints data
  m <- sim_zones_features[[1]][[rep(1, number_of_zones(sim_zones_features))]]
  m <- as.matrix(m) * 600
  # create targets data
  targ <- matrix(
    runif(number_of_features(sim_zones_features) *
      number_of_zones(sim_zones_features)) * 10,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  # create problems
  p1 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_constraints(3.124, "<=", m)
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), c(m[pu_idx, ])))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (SpatRaster, compile, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create constraints data
  cd <- c(
    sim_zones_features[[1]][[1]] * 2,
    sim_zones_features[[1]][[2]] * 100,
    sim_zones_features[[1]][[3]] * 500
  )
  # create targets data
  targ <- matrix(
    runif(
      number_of_features(sim_zones_features) *
      number_of_zones(sim_zones_features)
    ) * 10,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  # create problems
  p1 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_constraints(3.124, "<=", cd)
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), c(as.matrix(cd[pu_idx]))))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (Raster, compile, multiple zones)", {
  # import data
  sim_zones_pu_raster <- raster::stack(get_sim_zones_pu_raster())
  sim_zones_features <- as.ZonesRaster(get_sim_zones_features())
  # create constraints data
  cd <- raster::stack(sim_zones_pu_raster * c(2, 100, 500))
  # create targets data
  targ <- matrix(
    runif(
      number_of_features(sim_zones_features) *
      number_of_zones(sim_zones_features)
    ) * 10,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  # create problems
  expect_warning(
    p1 <-
      problem(sim_zones_pu_raster, sim_zones_features) %>%
      add_min_set_objective() %>%
      add_absolute_targets(targ) %>%
      add_binary_decisions(),
    "deprecated"
  )
  expect_warning(
    p2 <-
      p1 %>%
      add_linear_constraints(3.124, "<=", cd),
    "deprecated"
  )
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), c(cd[pu_idx])))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that(
  "minimum set objective (character/Spatial, compile, multiple zones)", {
  # import data
  sim_zones_pu_polygons <- sf::as_Spatial(get_sim_zones_pu_polygons())
  sim_zones_features <- as.ZonesRaster(get_sim_zones_features())
  # create constraints data
  sim_zones_pu_polygons$c1 <- runif(nrow(sim_zones_pu_polygons)) * 5
  sim_zones_pu_polygons$c2 <- runif(nrow(sim_zones_pu_polygons)) * 6
  sim_zones_pu_polygons$c3 <- runif(nrow(sim_zones_pu_polygons)) * 9
  # create targets data
  targ <- matrix(
    runif(
      number_of_features(sim_zones_features) *
      number_of_zones(sim_zones_features)
    ) * 10,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  # create problems
  expect_warning(
    p1 <-
      problem(sim_zones_pu_polygons, sim_zones_features,
              cost_column = c("cost_1", "cost_2", "cost_3")) %>%
      add_min_set_objective() %>%
      add_absolute_targets(targ) %>%
      add_binary_decisions(),
    "deprecated"
  )
  p2 <-
    p1 %>%
    add_linear_constraints(3.124, "<=", c("c1", "c2", "c3"))
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(
    o2$A(),
    rbind(
      o1$A(),
      c(
        sim_zones_pu_polygons$c1, sim_zones_pu_polygons$c2,
        sim_zones_pu_polygons$c3
      )
    )
  )
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (character/sf, compile, multiple zones)", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create constraints data
  sim_zones_pu_polygons$c1 <- runif(nrow(sim_zones_pu_polygons)) * 5
  sim_zones_pu_polygons$c2 <- runif(nrow(sim_zones_pu_polygons)) * 6
  sim_zones_pu_polygons$c3 <- runif(nrow(sim_zones_pu_polygons)) * 9
  # create targets data
  targ <- matrix(
    runif(
      number_of_features(sim_zones_features) *
      number_of_zones(sim_zones_features)
    ) * 10,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  # create problems
  p1 <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      cost_column = c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_constraints(3.124, "<=", c("c1", "c2", "c3"))
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(
    o2$A(),
    rbind(
      o1$A(),
      c(
        sim_zones_pu_polygons$c1, sim_zones_pu_polygons$c2,
        sim_zones_pu_polygons$c3
      )
    )
  )
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that(
  paste(
    "minimum set objective (character/data.frame, compile,",
    "multiple zones)"
  ), {
  # make data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)), cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10), spp2_2 = runif(10),
    c1 = runif(10) * 2, c2 = runif(10) * 4
  )
  targ <- matrix(runif(4) * 3, 2, 2)
  # create problems
  p1 <-
    problem(
      pu,
      zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
      cost_column = c("cost_1", "cost_2")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  p2 <-
    p1 %>%
    add_linear_constraints(3.124, "<=", c("c1", "c2"))
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # calculations for tests
  pu_idx <- p1$planning_unit_indices()
  # tests
  expect_equal(o2$obj(), o1$obj())
  expect_equal(o2$A(), rbind(o1$A(), c(pu$c1[pu_idx], pu$c2[pu_idx])))
  expect_equal(o2$ub(), o1$ub())
  expect_equal(o2$lb(), o1$lb())
  expect_equal(o2$rhs(), c(o1$rhs(), 3.124))
  expect_equal(o2$sense(), c(o1$sense(), "<="))
  expect_equal(o2$modelsense(), o1$modelsense())
})

test_that("minimum set objective (solve, SpatRaster, multiple zones)", {
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
  cd <- matrix(
    c(
      10, 0, 0, 0, 0, 0, 0,
      0,  0, 0, 0, 10, 0, 0
    ),
    ncol = 2
  )
  # create problem
  p <-
    problem(costs, zones(spp[[1:2]], spp[[3:4]])) %>%
    add_min_set_objective() %>%
    add_linear_constraints(5, "<=", cd) %>%
    add_absolute_targets(matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_is(s, "SpatRaster")
  expect_equal(c(terra::values(s[[1]])), c(0, 1, NA, 1, 0, 0, NA))
  expect_equal(c(terra::values(s[[2]])), c(1, 0, 0,  0, 0, 0, NA))
})

test_that("invalid inputs", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_pu_polygons <- get_sim_pu_polygons()
  # tests
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_linear_constraints(NA_real_, "<=", sim_features[[1]])
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_linear_constraints(1e+100, ">=", sim_features[[1]])
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_linear_constraints(c(3, 3), "<=", sim_features[[1]])
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_linear_constraints(3, NA_character_, sim_features[[1]])
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_linear_constraints(3, "asdf", sim_features[[1]])
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_linear_constraints(3, "<=", sim_features[[c(1, 1)]])
  })
  expect_tidy_error({
    problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
    add_linear_constraints(3, "<=",  c("cost", "cost"))
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_linear_constraints(3, "<=", "asdf")
  })
  expect_tidy_error({
    problem(sim_pu_raster, sim_features) %>%
    add_linear_constraints(
      3, "<=",
      terra::crop(sim_features[[1]], terra::ext(c(0, 0.5, 0, 0.5)))
    )
  })
})
