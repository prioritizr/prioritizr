context("planning_unit_solution_status")

test_that("numeric", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  p <- problem(pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               as.matrix(t(pu[, 3:4])))
  # create a solution
  s <- rep(c(0, 1), 5)
  s[is.na(pu$cost)] <- NA_real_
  # extract solution status
  r1 <- planning_unit_solution_status(p, s)
  # create correct result
  r2 <- matrix(s[!is.na(s)], ncol = 1)
  # run tests
  expect_equal(r1, r2)
})

test_that("matrix (single zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  p <- problem(matrix(pu$cost, ncol = 1),
               data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               as.matrix(t(pu[, 3:4])))
  # create a solution
  s <- matrix(rep(c(0, 1), 5), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # extract solution status
  r1 <- planning_unit_solution_status(p, s)
  # create correct result
  r2 <- matrix(s[!is.na(s)], ncol = 1)
  # run tests
  expect_equal(r1, r2)
})

test_that("matrix (multiple zones)", {
  # simulate data
  pu <- data.frame(id = seq_len(10),
                   cost_1 = c(NA, NA, runif(8)),
                   cost_2 = c(0.3, NA, runif(8)),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  # create problem
  p <- problem(as.matrix(pu[, 2:3]),
               data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7]))))
  # create a solution
  s <- matrix(c(rep(c(0, 0.5), 5), rep(c(0.5, 0), 5)), ncol = 2)
  s[is.na(as.matrix(pu[, c("cost_1", "cost_2")]))] <- NA_real_
  # extract solution status
  r1 <- planning_unit_solution_status(p, s)
  # create correct result
  r2 <- s[which(!is.na(pu$cost_1) | !is.na(pu$cost_2)), , drop = FALSE]
  # run tests
  expect_equal(r1, r2)
})

test_that("data.frame (single zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  p <- problem(pu, c("spp1", "spp2"), cost_column = "cost")
  # create a solution
  s <- data.frame(solution = rep(c(0, 1), 5))
  s[[1]][is.na(pu$cost)] <- NA_real_
  # extract solution status
  r1 <- planning_unit_solution_status(p, s)
  # create correct result
  r2 <- matrix(s$solution[!is.na(pu$cost)], ncol = 1)
  colnames(r2) <- "solution"
  # run tests
  expect_equal(r1, r2)
})

test_that("data.frame (multiple zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10),
                   cost_1 = c(NA, NA, runif(8)),
                   cost_2 = c(0.3, NA, runif(8)),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  # create problem
  p <- problem(pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
               c("cost_1", "cost_2"))
  # create a solution
  s <- data.frame("z1" = rep(c(0, 0.5), 5), "z2" = rep(c(0.5, 0), 5))
  s[[1]][is.na(pu$cost_1)] <- NA_real_
  s[[2]][is.na(pu$cost_2)] <- NA_real_
  # extract solution status
  r1 <- planning_unit_solution_status(p, s)
  # create correct result
  r2 <- as.matrix(s[which(!is.na(pu$cost_1) | !is.na(pu$cost_2)), ,
                    drop = FALSE])
  rownames(r2) <- NULL
  # run tests
  expect_equal(r1, r2)
})

test_that("Spatial (single zone)", {
  # load data
  sim_pu_polygons <- get_sim_pu_polygons()
  pu <- sim_pu_polygons[seq_len(10), , drop = FALSE]
  pu@proj4string <- as(sf::st_crs(32756), "CRS")
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
  r1 <- planning_unit_solution_status(p, s)
  # create correct result
  r2 <- matrix(s$solution[!is.na(pu$cost)], ncol = 1)
  colnames(r2) <- "solution"
  # run tests
  expect_equal(r1, r2)
})

test_that("Spatial (multiple zone)", {
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  pu <- sim_zones_pu_polygons
  pu@proj4string <- as(sf::st_crs(32756), "CRS")
  pu$spp1_1 <- c(NA, runif(nrow(pu) - 1))
  pu$spp2_1 <- c(rpois(nrow(pu) - 1, 1),
                                    NA)
  pu$spp1_2 <- c(NA, runif(nrow(pu) - 1))
  pu$spp2_2 <- rpois(nrow(pu), 1)
  pu$s1 <- rep(c(0, 0.5), nrow(pu) / 2)
  pu$s2 <- rep(c(0.5, 0), nrow(pu) / 2)
  pu$s1[is.na(pu$cost_1)] <- NA_real_
  pu$s2[is.na(pu$cost_2)] <- NA_real_
  # create problem
  p <- problem(pu,
               zones(z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
                     feature_names = c("spp1", "spp2")),
               c("cost_1", "cost_2"))
  # create a solution
  s <- pu[, c("s1", "s2")]
  # extract solution status
  r1 <- planning_unit_solution_status(p, s)
  # create correct result
  r2 <- as.matrix(s@data[which(!is.na(pu$cost_1) | !is.na(pu$cost_2)), ,
                         drop = FALSE])
  rownames(r2) <- NULL
  # run tests
  expect_equal(r1, r2)
})

test_that("sf (single zone)", {
  # load data
  sim_pu_polygons <- get_sim_pu_polygons()
  pu <- sim_pu_polygons[seq_len(10), , drop = FALSE]
  sf::st_crs(pu) <- sf::st_crs(32756)
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
  r1 <- planning_unit_solution_status(p, s)
  # create correct result
  r2 <- matrix(s$solution[!is.na(pu$cost)], ncol = 1)
  colnames(r2) <- "solution"
  # run tests
  expect_equal(r1, r2)
})

test_that("sf (multiple zone)", {
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  pu <- sim_zones_pu_polygons
  sf::st_crs(pu) <- sf::st_crs(32756)
  pu$spp1_1 <- c(NA, runif(nrow(pu) - 1))
  pu$spp2_1 <- c(rpois(nrow(pu) - 1, 1),
                                    NA)
  pu$spp1_2 <- c(NA, runif(nrow(pu) - 1))
  pu$spp2_2 <- rpois(nrow(pu), 1)
  pu$s1 <- rep(c(0, 0.5), nrow(pu) / 2)
  pu$s2 <- rep(c(0.5, 0), nrow(pu) / 2)
  pu$s1[is.na(pu$cost_1)] <- NA_real_
  pu$s2[is.na(pu$cost_2)] <- NA_real_
  # create problem
  p <- problem(pu,
               zones(z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
                     feature_names = c("spp1", "spp2")),
               c("cost_1", "cost_2"))
  # create a solution
  s <- pu[, c("s1", "s2")]
  # extract solution status
  r1 <- planning_unit_solution_status(p, s)
  # create correct result
  r2 <- as.matrix(sf::st_drop_geometry(
    s[which(!is.na(pu$cost_1) | !is.na(pu$cost_2)), , drop = FALSE]))
  rownames(r2) <- NULL
  # run tests
  expect_equal(r1, r2)
})

test_that("Raster (single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_pu_raster@crs <- as(sf::st_crs(32756), "CRS")
  sim_features@crs <- as(sf::st_crs(32756), "CRS")
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # create a solution
  s <- raster::setValues(sim_pu_raster,
                         rep(c(0, 1), raster::ncell(sim_pu_raster) / 2))
  s[is.na(sim_pu_raster)] <- NA_real_
  # extract solution status
  r1 <- planning_unit_solution_status(p, s)
  # create correct result
  r2 <- matrix(c(na.omit(raster::getValues(s))), ncol = 1)
  # run tests
  expect_equal(r1, r2)
})

test_that("Raster (multiple zone)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  sim_zones_pu_raster@crs <- as(sf::st_crs(32756), "CRS")
  sim_zones_features[[1]]@crs <- as(sf::st_crs(32756), "CRS")
  sim_zones_features[[2]]@crs <- as(sf::st_crs(32756), "CRS")
  sim_zones_features[[3]]@crs <- as(sf::st_crs(32756), "CRS")
  # create problem
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  # create a solution
  s <- raster::stack(
    raster::setValues(
      sim_pu_raster, rep(c(0, 0.2), raster::ncell(sim_pu_raster) / 2)),
    raster::setValues(
      sim_pu_raster, rep(c(0.3, 0), raster::ncell(sim_pu_raster) / 2)),
    raster::setValues(
      sim_pu_raster, rep(c(0.4, 0), raster::ncell(sim_pu_raster) / 2)))
  s[[1]][is.na(sim_zones_pu_raster[[1]])] <- NA_real_
  s[[2]][is.na(sim_zones_pu_raster[[2]])] <- NA_real_
  s[[3]][is.na(sim_zones_pu_raster[[3]])] <- NA_real_
  raster::crs(s) <- as(sf::st_crs(32756), "CRS")
  # extract solution status
  r1 <- planning_unit_solution_status(p, s)
  # create correct result
  r2 <- as.matrix(raster::as.data.frame(s, na.rm = FALSE))
  r2 <- r2[rowSums(is.finite(r2)) > 0, , drop = FALSE]
  # run tests
  expect_equal(r1, r2)
})

test_that("invalid inputs", {
  expect_error({
    # simulate data
    pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                     spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
    # create problem
    p <- problem(pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
                 as.matrix(t(pu[, 3:4])))
    # create a solution
    s <- rep(c(0, 1), 5)
    # extract solution status
    planning_unit_solution_status(p, s)
  })
  expect_error({
    # simulate data
    pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                     spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
    # create problem
    p <- problem(matrix(pu$cost, ncol = 1),
                 data.frame(id = seq_len(2), name = c("spp1", "spp2")),
                 as.matrix(t(pu[, 3:4])))
    # create a solution
    s <- matrix(rep(c(0, 1), 5), ncol = 1)
    # extract solution status
    planning_unit_solution_status(p, s)
  })
  expect_error({
    # simulate data
    pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                     spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
    # create problem
    p <- problem(pu, c("spp1", "spp2"), cost_column = "cost")
    # create a solution
    s <- data.frame(solution = rep(c(0, 1), 5))
    # extract solution status
    planning_unit_solution_status(p, s)
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
    p <- problem(pu, c("spp1", "spp2"), "cost")
    # create a solution
    s <- pu[, "solution"]
    # extract solution status
    planning_unit_solution_status(p, s)
  })
  expect_error({
    # load data
    sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
    # create problem
    p <- problem(sim_pu_raster, sim_features)
    # create a solution
    s <- raster::setValues(
      sim_pu_raster, rep(c(0, 1), raster::ncell(sim_pu_raster) / 2))
    s <- pu[, "solution"]
    # extract solution status
    planning_unit_solution_status(p, s)
  })
})
