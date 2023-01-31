context("planning_unit_solution_status")

test_that("numeric", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  p <- problem(
    pu$cost,
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    as.matrix(t(pu[, 3:4]))
  )
  # create a solution
  s <- rep(c(0, 1), 5)
  s[is.na(pu$cost)] <- NA_real_
  # extract solution status
  x <- planning_unit_solution_status(p, s)
  # create correct result
  y <- matrix(s[!is.na(s)], ncol = 1)
  # run tests
  expect_equal(x, y)
})

test_that("matrix (single zone)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  p <- problem(
    matrix(pu$cost, ncol = 1),
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    as.matrix(t(pu[, 3:4]))
  )
  # create a solution
  s <- matrix(rep(c(0, 1), 5), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # extract solution status
  x <- planning_unit_solution_status(p, s)
  # create correct result
  y <- matrix(s[!is.na(s)], ncol = 1)
  # run tests
  expect_equal(x, y)
})

test_that("matrix (multiple zones)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)), cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10), spp2_2 = runif(10)
  )
  # create problem
  p <- problem(
    as.matrix(pu[, 2:3]),
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))
  )
  # create a solution
  s <- matrix(c(rep(c(0, 0.5), 5), rep(c(0.5, 0), 5)), ncol = 2)
  s[is.na(as.matrix(pu[, c("cost_1", "cost_2")]))] <- NA_real_
  # extract solution status
  x <- planning_unit_solution_status(p, s)
  # create correct result
  y <- s[which(!is.na(pu$cost_1) | !is.na(pu$cost_2)), , drop = FALSE]
  # run tests
  expect_equal(x, y)
})

test_that("data.frame (single zone)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10), cost = c(0.2, NA, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  p <- problem(pu, c("spp1", "spp2"), cost_column = "cost")
  # create a solution
  s <- data.frame(solution = rep(c(0, 1), 5))
  s[[1]][is.na(pu$cost)] <- NA_real_
  # extract solution status
  x <- planning_unit_solution_status(p, s)
  # create correct result
  y <- matrix(s$solution[!is.na(pu$cost)], ncol = 1)
  colnames(y) <- "solution"
  # run tests
  expect_equal(x, y)
})

test_that("data.frame (multiple zone)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)), cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10), spp2_2 = runif(10)
  )
  # create problem
  p <- problem(
    pu,
    zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
    c("cost_1", "cost_2")
  )
  # create a solution
  s <- data.frame("z1" = rep(c(0, 0.5), 5), "z2" = rep(c(0.5, 0), 5))
  s[[1]][is.na(pu$cost_1)] <- NA_real_
  s[[2]][is.na(pu$cost_2)] <- NA_real_
  # extract solution status
  x <- planning_unit_solution_status(p, s)
  # create correct result
  y <- as.matrix(
    s[which(!is.na(pu$cost_1) | !is.na(pu$cost_2)), , drop = FALSE]
  )
  rownames(y) <- NULL
  # run tests
  expect_equal(x, y)
})

test_that("sf (single zone)", {
  # import data
  pu <- get_sim_pu_polygons()[seq_len(10), , drop = FALSE]
  pu$cost[1:5] <- NA
  pu$solution <- rep(c(0, 1), 5)
  pu$solution[is.na(pu$cost)] <- NA_real_
  pu$spp1 <- runif(10)
  pu$spp2 <- c(rpois(9, 1), NA)
  # create problem
  p <- problem(pu, c("spp1", "spp2"), "cost")
  # create a solution
  s <- pu[, "solution"]
  # extract solution status
  x <- planning_unit_solution_status(p, s)
  # create correct result
  y <- matrix(s$solution[!is.na(pu$cost)], ncol = 1)
  colnames(y) <- "solution"
  # run tests
  expect_equal(x, y)
})

test_that("sf (multiple zone)", {
  # import data
  pu <- get_sim_zones_pu_polygons()
  pu$spp1_1 <- c(NA, runif(nrow(pu) - 1))
  pu$spp2_1 <- c(rpois(nrow(pu) - 1, 1), NA)
  pu$spp1_2 <- c(NA, runif(nrow(pu) - 1))
  pu$spp2_2 <- rpois(nrow(pu), 1)
  pu$s1 <- rep(c(0, 0.5), nrow(pu) / 2)
  pu$s2 <- rep(c(0.5, 0), nrow(pu) / 2)
  pu$s1[is.na(pu$cost_1)] <- NA_real_
  pu$s2[is.na(pu$cost_2)] <- NA_real_
  # create problem
  p <- problem(
    pu,
    zones(
      z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
      feature_names = c("spp1", "spp2")
    ),
    c("cost_1", "cost_2")
  )
  # create a solution
  s <- pu[, c("s1", "s2")]
  # extract solution status
  x <- planning_unit_solution_status(p, s)
  # create correct result
  y <- as.matrix(sf::st_drop_geometry(
    s[which(!is.na(pu$cost_1) | !is.na(pu$cost_2)), , drop = FALSE]
  ))
  rownames(y) <- NULL
  # run tests
  expect_equal(x, y)
})

test_that("Spatial (single zone)", {
  # import data
  pu <- get_sim_pu_polygons()[seq_len(10), , drop = FALSE]
  pu$cost[1:5] <- NA
  pu$solution <- rep(c(0, 1), 5)
  pu$solution[is.na(pu$cost)] <- NA_real_
  pu$spp1 <- runif(10)
  pu$spp2 <- c(rpois(9, 1), NA)
  # create problems
  p1 <- problem(pu, c("spp1", "spp2"), "cost")
  expect_warning(
    p2 <- problem(sf::as_Spatial(pu), c("spp1", "spp2"), "cost"),
    "deprecated"
  )
  # calculations
  x <- planning_unit_solution_status(p1, pu[, "solution"])
  expect_warning(
    y <- planning_unit_solution_status(p2, sf::as_Spatial(pu[, "solution"])),
    "deprecated"
  )
  # run tests
  expect_equal(x, y)
})

test_that("Spatial (multiple zone)", {
  # import data
  pu <- get_sim_zones_pu_polygons()
  pu$spp1_1 <- c(NA, runif(nrow(pu) - 1))
  pu$spp2_1 <- c(rpois(nrow(pu) - 1, 1), NA)
  pu$spp1_2 <- c(NA, runif(nrow(pu) - 1))
  pu$spp2_2 <- rpois(nrow(pu), 1)
  pu$s1 <- rep(c(0, 0.5), nrow(pu) / 2)
  pu$s2 <- rep(c(0.5, 0), nrow(pu) / 2)
  pu$s1[is.na(pu$cost_1)] <- NA_real_
  pu$s2[is.na(pu$cost_2)] <- NA_real_
  # create problems
  p1 <- problem(
    pu,
    zones(
      z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
      feature_names = c("spp1", "spp2")
    ),
    c("cost_1", "cost_2")
  )
  expect_warning(
    p2 <- problem(
      sf::as_Spatial(pu),
      zones(
        z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
        feature_names = c("spp1", "spp2")
      ),
      c("cost_1", "cost_2")
    ),
    "deprecated"
  )
  # calculations
  x <- planning_unit_solution_status(p1, pu[, c("s1", "s2")])
  expect_warning(
    y <- planning_unit_solution_status(p2, sf::as_Spatial(pu[, c("s1", "s2")])),
    "deprecated"
  )
  # run tests
  expect_equal(x, y)
})

test_that("SpatRaster (single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # create a solution
  s <- terra::setValues(
    sim_pu_raster, rep(c(0, 1), terra::ncell(sim_pu_raster) / 2)
  )
  s[is.na(sim_pu_raster)] <- NA_real_
  # extract solution status
  x <- planning_unit_solution_status(p, s)
  # create correct result
  y <- matrix(c(na.omit(c(terra::values(s)))), ncol = 1)
  colnames(y) <- names(s)
  # run tests
  expect_equal(x, y)
})

test_that("SpatRaster (multiple zone)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  # create a solution
  s <- c(
    terra::setValues(
      sim_zones_pu_raster[[1]],
      rep(c(0, 0.2), terra::ncell(sim_zones_pu_raster) / 2)
    ),
    terra::setValues(
      sim_zones_pu_raster[[2]],
      rep(c(0.3, 0), terra::ncell(sim_zones_pu_raster) / 2)
    ),
    terra::setValues(
      sim_zones_pu_raster[[3]],
      rep(c(0.4, 0), terra::ncell(sim_zones_pu_raster) / 2)
    )
  )
  s[[1]][is.na(sim_zones_pu_raster[[1]])] <- NA_real_
  s[[2]][is.na(sim_zones_pu_raster[[2]])] <- NA_real_
  s[[3]][is.na(sim_zones_pu_raster[[3]])] <- NA_real_
  # extract solution status
  x <- planning_unit_solution_status(p, s)
  # create correct result
  y <- as.matrix(terra::as.data.frame(s, na.rm = FALSE))
  y <- y[rowSums(is.finite(y)) > 0, , drop = FALSE]
  # run tests
  expect_equal(x, y)
})

test_that("Raster (single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p1 <- problem(sim_pu_raster, sim_features)
  expect_warning(
    p2 <- problem(raster::raster(sim_pu_raster), raster::stack(sim_features)),
    "deprecated"
  )
  # create a solution
  s <- terra::setValues(
    sim_pu_raster, rep(c(0, 1), terra::ncell(sim_pu_raster) / 2)
  )
  s[is.na(sim_pu_raster)] <- NA_real_
  # extract solution status
  x <- planning_unit_solution_status(p1, s)
  # create correct result
  expect_warning(
    y <- planning_unit_solution_status(p2, raster::raster(s)),
    "deprecated"
  )
  colnames(y) <- names(s)
  # run tests
  expect_equal(x, y)
})

test_that("Raster (multiple zone)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p1 <- problem(sim_zones_pu_raster, sim_zones_features)
  expect_warning(
    p2 <- problem(
      raster::stack(sim_zones_pu_raster),
      as.ZonesRaster(sim_zones_features)
    ),
    "deprecated"
  )
  # create a solution
  s <- c(
    terra::setValues(
      sim_zones_pu_raster[[1]],
      rep(c(0, 0.2), terra::ncell(sim_zones_pu_raster) / 2)
    ),
    terra::setValues(
      sim_zones_pu_raster[[2]],
      rep(c(0.3, 0), terra::ncell(sim_zones_pu_raster) / 2)
    ),
    terra::setValues(
      sim_zones_pu_raster[[3]],
      rep(c(0.4, 0), terra::ncell(sim_zones_pu_raster) / 2)
    )
  )
  s[[1]][is.na(sim_zones_pu_raster[[1]])] <- NA_real_
  s[[2]][is.na(sim_zones_pu_raster[[2]])] <- NA_real_
  s[[3]][is.na(sim_zones_pu_raster[[3]])] <- NA_real_
  # extract solution status
  x <- planning_unit_solution_status(p1, s)
  expect_warning(
    y <- planning_unit_solution_status(p2, raster::stack(s)),
    "deprecated"
  )
  colnames(y) <- names(s)
  # run tests
  expect_equal(x, y)
})

test_that("invalid inputs", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create data
  pu <- get_sim_pu_polygons()[1:10, ]
  pu$id <- seq_len(nrow(pu))
  pu$cost[1:5] <- NA
  pu$solution <- rep(c(0, 1), 5)
  pu$spp1 <- runif(10)
  pu$spp2 <- c(rpois(9, 1), NA)
  # create problems
  p1 <- problem(
    pu$cost,
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    as.matrix(t(sf::st_drop_geometry(pu)[, 3:4]))
  )
  p2 <- problem(
    sf::st_drop_geometry(pu), c("spp1", "spp2"), cost_column = "cost"
  )
  p3 <- problem(pu, c("spp1", "spp2"), cost_column = "cost")
  p4 <- problem(sim_pu_raster, sim_features)
  # tests
  expect_tidy_error(
    planning_unit_solution_status(p1, rep(c(0, 1), 5)),
    "missing"
  )
  expect_tidy_error(
    planning_unit_solution_status(p1, matrix(rep(c(0, 1), 5), ncol = 1)),
    "missing"
  )
  expect_tidy_error(
    planning_unit_solution_status(p2, data.frame(solution = rep(c(0, 1), 5))),
    "missing"
  )
  expect_tidy_error(
    planning_unit_solution_status(p3, pu[, "solution"]),
    "missing"
  )
  expect_tidy_error(
    planning_unit_solution_status(
      p4,
      terra::setValues(
        sim_pu_raster, rep(c(0, 1), terra::ncell(sim_pu_raster) / 2)
      )
    ),
    "missing"
  )
})
