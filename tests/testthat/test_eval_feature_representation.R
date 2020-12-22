context("eval_feature_representation")

test_that("numeric", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  x <- problem(pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               as.matrix(t(pu[, 3:4])))
  # create a solution
  y <- rep(c(0, 1), 5)
  y[is.na(pu$cost)] <- NA_real_
  attr(y, "runtime") <- 5
  # calculate representation
  r <- eval_feature_representation(x, y)
  # create correct result
  r2 <- tibble::tibble(feature = c("spp1", "spp2"),
                       absolute_held = c(sum(c(pu$spp1 * y)[!is.na(pu$cost)],
                                             na.rm = TRUE),
                                         sum(c(pu$spp2 * y)[!is.na(pu$cost)],
                                             na.rm = TRUE)),
                       relative_held = absolute_held /
                                       c(sum(pu$spp1, na.rm  = TRUE),
                                         sum(pu$spp2, na.rm  = TRUE)))
  # run tests
  expect_equal(r, r2)
  expect_equal(nrow(na.omit(r)), nrow(r))
})

test_that("matrix (single zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  x <- problem(matrix(pu$cost, ncol = 1),
               data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               as.matrix(t(pu[, 3:4])))
  # create a solution
  y <- matrix(rep(c(0, 1), 5), ncol = 1)
  y[is.na(pu$cost)] <- NA_real_
  # calculate representation
  r <- eval_feature_representation(x, y)
  # create correct result
  r2 <- tibble::tibble(feature = c("spp1", "spp2"),
                       absolute_held = c(sum(c(pu$spp1 * y)[!is.na(pu$cost)],
                                             na.rm = TRUE),
                                         sum(c(pu$spp2 * y)[!is.na(pu$cost)],
                                             na.rm = TRUE)),
                       relative_held = absolute_held /
                                       c(sum(pu$spp1, na.rm  = TRUE),
                                         sum(pu$spp2, na.rm  = TRUE)))
  # run tests
  expect_equal(r, r2)
  expect_equal(nrow(na.omit(r)), nrow(r))
})

test_that("matrix (multiple zones)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost_1 = c(NA, NA, runif(8)),
                   cost_2 = c(0.3, NA, runif(8)),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  # create problem
  x <- problem(as.matrix(pu[, 2:3]),
               data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7]))))
  # create a solution
  y <- matrix(c(rep(c(0, 0.5), 5), rep(c(0.5, 0), 5)), ncol = 2)
  y[is.na(as.matrix(pu[, c("cost_1", "cost_2")]))] <- NA_real_
  # calculate representation
  r <- eval_feature_representation(x, y)
  # create correct result
  pos <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  r2 <- tibble::tibble(
    feature = rep(c("spp1", "spp2"), 2),
    zone = rep(c("1", "2"), each = 2),
    absolute_held = c(sum(c(pu$spp1_1 * y[, 1])[pos], na.rm = TRUE),
                       sum(c(pu$spp2_1 * y[, 1])[pos], na.rm = TRUE),
                       sum(c(pu$spp1_2 * y[, 2])[pos], na.rm = TRUE),
                       sum(c(pu$spp2_2 * y[, 2])[pos], na.rm = TRUE)),
    relative_held = absolute_held / c(sum(pu$spp1_1, na.rm  = TRUE),
                                      sum(pu$spp2_1, na.rm  = TRUE),
                                      sum(pu$spp1_2, na.rm  = TRUE),
                                      sum(pu$spp2_2, na.rm  = TRUE)))
  # run tests
  expect_equal(r, r2)
  expect_equal(nrow(na.omit(r)), nrow(r))
})

test_that("data.frame (single zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  x <- problem(pu, c("spp1", "spp2"), cost_column = "cost")
  # create a solution
  y <- data.frame(solution = rep(c(0, 1), 5))
  y[[1]][is.na(pu$cost)] <- NA_real_
  # calculate representation
  r <- eval_feature_representation(x, y)
  # create correct result
  r2 <- tibble::tibble(
    feature = c("spp1", "spp2"),
    absolute_held = c(sum(c(pu$spp1 * y[[1]])[!is.na(pu$cost)], na.rm = TRUE),
                      sum(c(pu$spp2 * y[[1]])[!is.na(pu$cost)], na.rm = TRUE)),
    relative_held = absolute_held / c(sum(pu$spp1, na.rm  = TRUE),
                                      sum(pu$spp2, na.rm  = TRUE)))
  # run tests
  expect_equal(r, r2)
  expect_equal(nrow(na.omit(r)), nrow(r))
})

test_that("data.frame (multiple zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost_1 = c(NA, NA, runif(8)),
                   cost_2 = c(0.3, NA, runif(8)),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  # create problem
  x <- problem(pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
               c("cost_1", "cost_2"))
  # create a solution
  y <- data.frame("z1" = rep(c(0, 0.5), 5), "z2" = rep(c(0.5, 0), 5))
  y[[1]][is.na(pu$cost_1)] <- NA_real_
  y[[2]][is.na(pu$cost_2)] <- NA_real_
  # calculate representation
  r <- eval_feature_representation(x, y)
  # create correct result
  pos <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  r2 <- tibble::tibble(
    feature = rep(c("1", "2"), 2),
    zone = rep(c("1", "2"), each = 2),
    absolute_held = c(sum(c(pu$spp1_1 * y[, 1])[pos], na.rm = TRUE),
                      sum(c(pu$spp2_1 * y[, 1])[pos], na.rm = TRUE),
                      sum(c(pu$spp1_2 * y[, 2])[pos], na.rm = TRUE),
                      sum(c(pu$spp2_2 * y[, 2])[pos], na.rm = TRUE)),
    relative_held = absolute_held / c(sum(pu$spp1_1, na.rm  = TRUE),
                                      sum(pu$spp2_1, na.rm  = TRUE),
                                      sum(pu$spp1_2, na.rm  = TRUE),
                                      sum(pu$spp2_2, na.rm  = TRUE)))
  # run tests
  expect_equal(r, r2)
  expect_equal(nrow(na.omit(r)), nrow(r))
})

test_that("Spatial (single zone)", {
  # load data
  data(sim_pu_polygons)
  pu <- sim_pu_polygons
  pu@proj4string <- sp::CRS("+init=epsg:32756")
  pu$cost[1:5] <- NA
  pu$solution <- rep(c(0, 1), 5)
  pu$solution[is.na(pu$cost)] <- NA_real_
  pu$spp1 <- runif(10)
  pu$spp2 <- c(rpois(9, 1), NA)
  # create problem
  x <- problem(pu, c("spp1", "spp2"), "cost")
  # create a solution
  y <- pu[, "solution"]
  # calculate representation
  r <- eval_feature_representation(x, y)
  # create correct result
  r2 <- tibble::tibble(
    feature = c("spp1", "spp2"),
    absolute_held = c(sum(c(pu$spp1 * pu$solution)[!is.na(pu$cost)],
                          na.rm = TRUE),
                      sum(c(pu$spp2 * pu$solution)[!is.na(pu$cost)],
                          na.rm = TRUE)),
    relative_held = absolute_held / c(sum(pu$spp1, na.rm  = TRUE),
                                      sum(pu$spp2, na.rm  = TRUE)))
  # run tests
  expect_equal(r, r2)
  expect_equal(nrow(na.omit(r)), nrow(r))
})

test_that("Spatial (multiple zone)", {
  # load data
  data(sim_pu_zones_polygons)
  pu <- sim_pu_zones_polygons
  pu@proj4string <- sp::CRS("+init=epsg:32756")
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
  x <- problem(pu,
               zones(z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
                     feature_names = c("spp1", "spp2")),
               c("cost_1", "cost_2"))
  # create a solution
  y <- pu[, c("s1", "s2")]
  # calculate representation
  r <- eval_feature_representation(x, y)
  # create correct result
  pos <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  r2 <- tibble::tibble(
    feature = rep(c("spp1", "spp2"), 2),
    zone = rep(c("z1", "z2"), each = 2),
    absolute_held = c(sum(c(pu$spp1_1 * y@data[, 1])[pos], na.rm = TRUE),
                      sum(c(pu$spp2_1 * y@data[, 1])[pos], na.rm = TRUE),
                      sum(c(pu$spp1_2 * y@data[, 2])[pos], na.rm = TRUE),
                      sum(c(pu$spp2_2 * y@data[, 2])[pos], na.rm = TRUE)),
    relative_held = absolute_held / c(sum(pu$spp1_1, na.rm  = TRUE),
                                      sum(pu$spp2_1, na.rm  = TRUE),
                                      sum(pu$spp1_2, na.rm  = TRUE),
                                      sum(pu$spp2_2, na.rm  = TRUE)))
  # run tests
  expect_equal(r, r2)
  expect_equal(nrow(na.omit(r)), nrow(r))
})

test_that("sf (single zone)", {
  # load data
  data(sim_pu_sf)
  pu <- sim_pu_sf
  sf::st_crs(pu) <- sf::st_crs(32756)
  pu$cost[1:5] <- NA
  pu$solution <- rep(c(0, 1), 5)
  pu$solution[is.na(pu$cost)] <- NA_real_
  pu$spp1 <- runif(10)
  pu$spp2 <- c(rpois(9, 1), NA)
  # create problem
  x <- problem(pu, c("spp1", "spp2"), "cost")
  # create a solution
  y <- pu[, "solution"]
  # calculate representation
  r <- eval_feature_representation(x, y)
  # create correct result
  r2 <- tibble::tibble(
    feature = c("spp1", "spp2"),
    absolute_held = c(sum(c(pu$spp1 * pu$solution)[!is.na(pu$cost)],
                          na.rm = TRUE),
                      sum(c(pu$spp2 * pu$solution)[!is.na(pu$cost)],
                          na.rm = TRUE)),
    relative_held = absolute_held / c(sum(pu$spp1, na.rm  = TRUE),
                                      sum(pu$spp2, na.rm  = TRUE)))
  # run tests
  expect_equal(r, r2)
  expect_equal(nrow(na.omit(r)), nrow(r))
})

test_that("sf (multiple zone)", {
  # load data
  data(sim_pu_zones_sf)
  pu <- sim_pu_zones_sf
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
  x <- problem(pu,
               zones(z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
                     feature_names = c("spp1", "spp2")),
               c("cost_1", "cost_2"))
  # create a solution
  y <- sf::st_drop_geometry(pu[, c("s1", "s2")])
  # calculate representation
  r <- eval_feature_representation(x, y)
  # create correct result
  pos <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  r2 <- tibble::tibble(
    feature = rep(c("spp1", "spp2"), 2),
    zone = rep(c("z1", "z2"), each = 2),
    absolute_held = c(sum(c(pu$spp1_1 * y[, 1])[pos], na.rm = TRUE),
                      sum(c(pu$spp2_1 * y[, 1])[pos], na.rm = TRUE),
                      sum(c(pu$spp1_2 * y[, 2])[pos], na.rm = TRUE),
                      sum(c(pu$spp2_2 * y[, 2])[pos], na.rm = TRUE)),
    relative_held = absolute_held / c(sum(pu$spp1_1, na.rm  = TRUE),
                                      sum(pu$spp2_1, na.rm  = TRUE),
                                      sum(pu$spp1_2, na.rm  = TRUE),
                                      sum(pu$spp2_2, na.rm  = TRUE)))
  # run tests
  expect_equal(r, r2)
  expect_equal(nrow(na.omit(r)), nrow(r))
})

test_that("Raster (single zone)", {
  # load data
  data(sim_pu_raster, sim_features)
  sim_pu_raster@crs <- sp::CRS("+init=epsg:32756")
  sim_features@crs <- sp::CRS("+init=epsg:32756")
  # create problem
  x <- problem(sim_pu_raster, sim_features)
  # create a solution
  y <- raster::setValues(sim_pu_raster,
                         rep(c(0, 1), raster::ncell(sim_pu_raster) / 2))
  y[is.na(sim_pu_raster)] <- NA_real_
  # calculate representation
  r <- eval_feature_representation(x, y)
  # create correct result
  rij <- as.matrix(rij_matrix(sim_pu_raster, sim_features))
  s <- y[raster::Which(!is.na(sim_pu_raster), cells = TRUE)]
  r2 <- tibble::tibble(
    feature = names(sim_features),
    absolute_held = unname(rowSums(rij * matrix(s, ncol = length(s),
      nrow = nrow(rij), byrow = TRUE))),
    relative_held = unname(absolute_held / raster::cellStats(sim_features,
                                                             "sum")))
  # run tests
  expect_equal(r, r2)
  expect_equal(nrow(na.omit(r)), nrow(r))
})

test_that("Raster (multiple zone)", {
  # load data
  data(sim_pu_zones_stack, sim_features_zones)
  sim_pu_zones_stack@crs <- sp::CRS("+init=epsg:32756")
  sim_features_zones[[1]]@crs <- sp::CRS("+init=epsg:32756")
  sim_features_zones[[2]]@crs <- sp::CRS("+init=epsg:32756")
  sim_features_zones[[3]]@crs <- sp::CRS("+init=epsg:32756")
  # create problem
  x <- problem(sim_pu_zones_stack, sim_features_zones)
  # create a solution
  y <- raster::stack(
    raster::setValues(sim_pu_raster,
                      rep(c(0, 0.2), raster::ncell(sim_pu_raster) / 2)),
    raster::setValues(sim_pu_raster,
                      rep(c(0.3, 0), raster::ncell(sim_pu_raster) / 2)),
    raster::setValues(sim_pu_raster,
                      rep(c(0.4, 0), raster::ncell(sim_pu_raster) / 2)))
  y[[1]][is.na(sim_pu_zones_stack[[1]])] <- NA_real_
  y[[2]][is.na(sim_pu_zones_stack[[2]])] <- NA_real_
  y[[3]][is.na(sim_pu_zones_stack[[3]])] <- NA_real_
  raster::crs(y) <- sp::CRS("+init=epsg:32756")
  # calculate representation
  r <- eval_feature_representation(x, y)
  # create correct result
  rij <- list(as.matrix(rij_matrix(sim_pu_zones_stack,
                                   sim_features_zones[[1]])),
              as.matrix(rij_matrix(sim_pu_zones_stack,
                                   sim_features_zones[[2]])),
              as.matrix(rij_matrix(sim_pu_zones_stack,
                                   sim_features_zones[[3]])))
  s <- y[raster::Which(!is.na(sim_pu_zones_stack[[1]]) |
                       !is.na(sim_pu_zones_stack[[2]]) |
                       !is.na(sim_pu_zones_stack[[3]]), cells = TRUE)]
  r2 <- tibble::tibble(
    feature = rep(feature_names(sim_features_zones), 3),
    zone = rep(zone_names(sim_features_zones),
               each = number_of_features(sim_features_zones)),
    absolute_held = unname(c(sum(rij[[1]][1, ] * s[, 1], na.rm = TRUE),
                             sum(rij[[1]][2, ] * s[, 1], na.rm = TRUE),
                             sum(rij[[1]][3, ] * s[, 1], na.rm = TRUE),
                             sum(rij[[1]][4, ] * s[, 1], na.rm = TRUE),
                             sum(rij[[1]][5, ] * s[, 1], na.rm = TRUE),
                             sum(rij[[2]][1, ] * s[, 2], na.rm = TRUE),
                             sum(rij[[2]][2, ] * s[, 2], na.rm = TRUE),
                             sum(rij[[2]][3, ] * s[, 2], na.rm = TRUE),
                             sum(rij[[2]][4, ] * s[, 2], na.rm = TRUE),
                             sum(rij[[2]][5, ] * s[, 2], na.rm = TRUE),
                             sum(rij[[3]][1, ] * s[, 3], na.rm = TRUE),
                             sum(rij[[3]][2, ] * s[, 3], na.rm = TRUE),
                             sum(rij[[3]][3, ] * s[, 3], na.rm = TRUE),
                             sum(rij[[3]][4, ] * s[, 3], na.rm = TRUE),
                             sum(rij[[3]][5, ] * s[, 3], na.rm = TRUE))),
    relative_held = unname(c(
      absolute_held[1:5] / raster::cellStats(sim_features_zones[[1]], "sum"),
      absolute_held[6:10] / raster::cellStats(sim_features_zones[[2]], "sum"),
      absolute_held[11:15] / raster::cellStats(sim_features_zones[[3]],
                                               "sum"))))
  # run tests
  expect_equal(r, r2)
  expect_equal(nrow(na.omit(r)), nrow(r))
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
    r <- eval_feature_representation(x, y)
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
    r <- eval_feature_representation(x, y)
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
    r <- eval_feature_representation(x, y)
  })
  expect_error({
    # load data
    data(sim_pu_polygons)
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
    r <- eval_feature_representation(x, y)
  })
  expect_error({
    # load data
    data(sim_pu_raster, sim_features)
    # create problem
    x <- problem(sim_pu_raster, sim_features)
    # create a solution
    y <- raster::setValues(sim_pu_raster,
                           rep(c(0, 1), raster::ncell(sim_pu_raster) / 2))
    # calculate representation
    r <- eval_feature_representation(x, y)
  })
})
