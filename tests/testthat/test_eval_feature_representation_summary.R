context("eval_feature_representation_summary")

test_that("numeric", {
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA)
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
  # calculate representation
  r1 <- eval_feature_representation_summary(p, s)
  # create correct result
  r2 <- tibble::tibble(
    summary = "overall",
    feature = c("spp1", "spp2"),
    total_amount = c(
      sum(pu$spp1, na.rm  = TRUE),
      sum(pu$spp2, na.rm  = TRUE)
    ),
    absolute_held = c(
      sum(c(pu$spp1 * s)[!is.na(pu$cost)], na.rm = TRUE),
      sum(c(pu$spp2 * s)[!is.na(pu$cost)], na.rm = TRUE)
    ),
    relative_held = absolute_held / total_amount
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("matrix (single zone)", {
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  p <- problem(
    matrix(pu$cost, ncol = 1),
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    as.matrix(t(pu[, 3:4])))
  # create a solution
  s <- matrix(rep(c(0, 1), 5), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # calculate representation
  r1 <- eval_feature_representation_summary(p, s)
  # create correct result
  r2 <- tibble::tibble(
    summary = "overall",
    feature = c("spp1", "spp2"),
    total_amount = c(
      sum(pu$spp1, na.rm = TRUE),
      sum(pu$spp2, na.rm = TRUE)
    ),
    absolute_held = c(
      sum(c(pu$spp1 * s)[!is.na(pu$cost)], na.rm = TRUE),
      sum(c(pu$spp2 * s)[!is.na(pu$cost)], na.rm = TRUE)
    ),
    relative_held = absolute_held / total_amount
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("matrix (multiple zones)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)),
    cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10),
    spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10),
    spp2_2 = runif(10)
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
  # calculate representation
  r1 <- eval_feature_representation_summary(p, s)
  # create correct result
  idx <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  r2 <- tibble::tibble(
    summary = c(rep(c("overall", "1", "2"), each = 2)),
    feature = rep(c("spp1", "spp2"), 3),
    total_amount = c(
      sum(pu$spp1_1, pu$spp1_2, na.rm  = TRUE),
      sum(pu$spp2_1, pu$spp2_2, na.rm  = TRUE),
      sum(pu$spp1_1, na.rm  = TRUE),
      sum(pu$spp2_1, na.rm  = TRUE),
      sum(pu$spp1_2, na.rm  = TRUE),
      sum(pu$spp2_2, na.rm  = TRUE)
    ),
    absolute_held = c(
      sum(c(pu$spp1_1 * s[, 1])[idx], c(pu$spp1_2 * s[, 2])[idx], na.rm = TRUE),
      sum(c(pu$spp2_1 * s[, 1])[idx], c(pu$spp2_2 * s[, 2])[idx], na.rm = TRUE),
      sum(c(pu$spp1_1 * s[, 1])[idx], na.rm = TRUE),
      sum(c(pu$spp2_1 * s[, 1])[idx], na.rm = TRUE),
      sum(c(pu$spp1_2 * s[, 2])[idx], na.rm = TRUE),
      sum(c(pu$spp2_2 * s[, 2])[idx], na.rm = TRUE)
    ),
    relative_held = absolute_held / total_amount
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("data.frame (single zone)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost = c(0.2, NA, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  p <- problem(pu, c("spp1", "spp2"), cost_column = "cost")
  # create a solution
  s <- data.frame(solution = rep(c(0, 1), 5))
  s[[1]][is.na(pu$cost)] <- NA_real_
  # calculate representation
  r1 <- eval_feature_representation_summary(p, s)
  # create correct result
  r2 <- tibble::tibble(
    summary = "overall",
    feature = c("spp1", "spp2"),
    total_amount = c(
      sum(pu$spp1, na.rm  = TRUE),
      sum(pu$spp2, na.rm  = TRUE)
    ),
    absolute_held = c(
      sum(c(pu$spp1 * s[[1]])[!is.na(pu$cost)], na.rm = TRUE),
      sum(c(pu$spp2 * s[[1]])[!is.na(pu$cost)], na.rm = TRUE)
    ),
    relative_held = absolute_held / total_amount
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("data.frame (multiple zone)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)),
    cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10),
    spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10),
    spp2_2 = runif(10)
  )
  # create problem
  p <- problem(
    pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
    cost_column = c("cost_1", "cost_2")
  )
  # create a solution
  s <- data.frame("z1" = rep(c(0, 0.5), 5), "z2" = rep(c(0.5, 0), 5))
  s[[1]][is.na(pu$cost_1)] <- NA_real_
  s[[2]][is.na(pu$cost_2)] <- NA_real_
  # calculate representation
  r1 <- eval_feature_representation_summary(p, s)
  # create correct result
  idx <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  r2 <- tibble::tibble(
    summary = rep(c("overall", "1", "2"), each = 2),
    feature = rep(c("1", "2"), 3),
    total_amount = c(
      sum(pu$spp1_1, pu$spp1_2, na.rm  = TRUE),
      sum(pu$spp2_1, pu$spp2_2, na.rm  = TRUE),
      sum(pu$spp1_1, na.rm  = TRUE),
      sum(pu$spp2_1, na.rm  = TRUE),
      sum(pu$spp1_2, na.rm  = TRUE),
      sum(pu$spp2_2, na.rm  = TRUE)
    ),
    absolute_held = c(
      sum(c(pu$spp1_1 * s[, 1])[idx], c(pu$spp1_2 * s[, 2])[idx], na.rm = TRUE),
      sum(c(pu$spp2_1 * s[, 1])[idx], c(pu$spp2_2 * s[, 2])[idx], na.rm = TRUE),
      sum(c(pu$spp1_1 * s[, 1])[idx], na.rm = TRUE),
      sum(c(pu$spp2_1 * s[, 1])[idx], na.rm = TRUE),
      sum(c(pu$spp1_2 * s[, 2])[idx], na.rm = TRUE),
      sum(c(pu$spp2_2 * s[, 2])[idx], na.rm = TRUE)
    ),
    relative_held = absolute_held / total_amount
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("sf (single zone)", {
  # create data
  pu <- get_sim_pu_polygons()[seq_len(10), ]
  suppressWarnings(sf::st_crs(pu) <- sf::st_crs(32756))
  pu$cost[1:5] <- NA
  pu$solution <- rep(c(0, 1), 5)
  pu$solution[is.na(pu$cost)] <- NA_real_
  pu$spp1 <- runif(10)
  pu$spp2 <- c(rpois(9, 1), NA)
  # create problem
  p <- problem(pu, c("spp1", "spp2"), "cost")
  # create a solution
  s <- pu[, "solution"]
  # calculate representation
  r1 <- eval_feature_representation_summary(p, s)
  # create correct result
  r2 <- tibble::tibble(
    summary = rep("overall", 2),
    feature = c("spp1", "spp2"),
    total_amount = c(
      sum(pu$spp1, na.rm  = TRUE),
      sum(pu$spp2, na.rm  = TRUE)
    ),
    absolute_held = c(
      sum(c(pu$spp1 * pu$solution)[!is.na(pu$cost)], na.rm = TRUE),
      sum(c(pu$spp2 * pu$solution)[!is.na(pu$cost)], na.rm = TRUE)
    ),
    relative_held = absolute_held / total_amount
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("sf (multiple zone)", {
  # create data
  pu <- get_sim_zones_pu_polygons()
  suppressWarnings(sf::st_crs(pu) <- sf::st_crs(32756))
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
    pu, cost_column = c("cost_1", "cost_2"),
    zones(
      z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
      feature_names = c("spp1", "spp2")
    )
  )
  # create a solution
  s <- pu[, c("s1", "s2")]
  # calculate representation
  r1 <- eval_feature_representation_summary(p, s)
  # create correct result
  idx <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  r2 <- tibble::tibble(
    summary = rep(c("overall", "z1", "z2"), each = 2),
    feature = rep(c("spp1", "spp2"), 3),
    total_amount = c(
      sum(pu$spp1_1, pu$spp1_2, na.rm  = TRUE),
      sum(pu$spp2_1, pu$spp2_2, na.rm  = TRUE),
      sum(pu$spp1_1, na.rm  = TRUE),
      sum(pu$spp2_1, na.rm  = TRUE),
      sum(pu$spp1_2, na.rm  = TRUE),
      sum(pu$spp2_2, na.rm  = TRUE)
    ),
    absolute_held = c(
      sum(c(pu$spp1_1 * s$s1)[idx], c(pu$spp1_2 * s$s2)[idx], na.rm = TRUE),
      sum(c(pu$spp2_1 * s$s1)[idx], c(pu$spp2_2 * s$s2)[idx], na.rm = TRUE),
      sum(c(pu$spp1_1 * s$s1)[idx], na.rm = TRUE),
      sum(c(pu$spp2_1 * s$s1)[idx], na.rm = TRUE),
      sum(c(pu$spp1_2 * s$s2)[idx], na.rm = TRUE),
      sum(c(pu$spp2_2 * s$s2)[idx], na.rm = TRUE)
    ),
    relative_held = absolute_held / total_amount
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("Spatial (single zone)", {
  # create data
  pu <- get_sim_pu_polygons()[seq_len(10), ]
  pu$cost[1:5] <- NA
  pu$solution <- rep(c(0, 1), nrow(pu) / 2)
  pu$solution[is.na(pu$cost)] <- NA_real_
  pu$spp1 <- runif(10)
  pu$spp2 <- c(rpois(9, 1), NA)
  # create problem
  p1 <- problem(pu, c("spp1", "spp2"), "cost")
  expect_warning(
    p2 <- problem(sf::as_Spatial(pu), c("spp1", "spp2"), "cost"),
    "deprecated"
  )
  # create a solution
  s <- pu[, "solution"]
  # calculate representation
  r1 <- eval_feature_representation_summary(p1, s)
  expect_warning(
    r2 <- eval_feature_representation_summary(p2, sf::as_Spatial(s)),
    "deprecated"
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("Spatial (multiple zones)", {
  # load data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  pu <- sim_zones_pu_polygons
  suppressWarnings(sf::st_crs(pu) <- sf::st_crs(32756))
  pu$spp1_1 <- c(NA, runif(nrow(pu) - 1))
  pu$spp2_1 <- c(rpois(nrow(pu) - 1, 1), NA)
  pu$spp1_2 <- c(NA, runif(nrow(pu) - 1))
  pu$spp2_2 <- rpois(nrow(pu), 1)
  pu$s1 <- rep(c(0, 0.5), nrow(pu) / 2)
  pu$s2 <- rep(c(0.5, 0), nrow(pu) / 2)
  pu$s1[is.na(pu$cost_1)] <- NA_real_
  pu$s2[is.na(pu$cost_2)] <- NA_real_
  # create problem
  p1 <- problem(
    pu,
    cost_column = c("cost_1", "cost_2"),
    zones(
      z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
      feature_names = c("spp1", "spp2")
    )
  )
  expect_warning(
    p2 <- problem(
      sf::as_Spatial(pu),
      cost_column = c("cost_1", "cost_2"),
      zones(
        z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
        feature_names = c("spp1", "spp2")
      )
    ),
    "deprecated"
  )
  # create a solution
  s <- pu[, c("s1", "s2")]
  # calculate representation
  r1 <- eval_feature_representation_summary(p1, s)
  expect_warning(
    r2 <- eval_feature_representation_summary(p2, sf::as_Spatial(s)),
    "deprecated"
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("SpatRaster (single zone)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <- problem(sim_pu_raster, sim_features)
  # create a solution
  s <- terra::setValues(
    sim_pu_raster, rep(c(0, 1),
    terra::ncell(sim_pu_raster) / 2)
  )
  s[is.na(sim_pu_raster)] <- NA_real_
  # calculate representation
  r1 <- eval_feature_representation_summary(p, s)
  # create correct result
  rij <- as.matrix(rij_matrix(sim_pu_raster, sim_features))
  s <- c(s[!is.na(sim_pu_raster)])
  r2 <- tibble::tibble(
    summary = rep("overall", terra::nlyr(sim_features)),
    feature = names(sim_features),
    total_amount = terra::global(sim_features, "sum", na.rm = TRUE)[[1]],
    absolute_held = unname(
      rowSums(rij * matrix(s, ncol = length(s), nrow = nrow(rij), byrow = TRUE))
    ),
    relative_held = absolute_held / total_amount
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("SpatRaster (multiple zone)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  # create a solution
  s <- c(
    raster::setValues(
      sim_zones_pu_raster[[1]], rep(c(0, 0.2),
      terra::ncell(sim_zones_pu_raster) / 2)
    ),
    raster::setValues(
      sim_zones_pu_raster[[1]], rep(c(0.3, 0),
      terra::ncell(sim_zones_pu_raster) / 2)
    ),
    raster::setValues(
      sim_zones_pu_raster[[1]], rep(c(0.4, 0),
      terra::ncell(sim_zones_pu_raster) / 2)
    )
  )
  s[[1]][is.na(sim_zones_pu_raster[[1]])] <- NA_real_
  s[[2]][is.na(sim_zones_pu_raster[[2]])] <- NA_real_
  s[[3]][is.na(sim_zones_pu_raster[[3]])] <- NA_real_
  # calculate representation
  r1 <- eval_feature_representation_summary(p, s)
  # create correct result
  rij <- list(
    as.matrix(rij_matrix(sim_zones_pu_raster, sim_zones_features[[1]])),
    as.matrix(rij_matrix(sim_zones_pu_raster, sim_zones_features[[2]])),
    as.matrix(rij_matrix(sim_zones_pu_raster, sim_zones_features[[3]]))
  )
  s <- s[min(is.na(sim_zones_pu_raster)) == 0]
  r2 <- tibble::tibble(
    summary = rep(
      c("overall", zone_names(sim_zones_features)),
      each = number_of_features(sim_zones_features)),
    feature = rep(feature_names(sim_zones_features), 4),
    total_amount = unname(c(
      terra::global(sim_zones_features[[1]], "sum", na.rm = TRUE)[[1]] +
      terra::global(sim_zones_features[[2]], "sum", na.rm = TRUE)[[1]] +
      terra::global(sim_zones_features[[3]], "sum", na.rm = TRUE)[[1]],
      terra::global(sim_zones_features[[1]], "sum", na.rm = TRUE)[[1]],
      terra::global(sim_zones_features[[2]], "sum", na.rm = TRUE)[[1]],
      terra::global(sim_zones_features[[3]], "sum", na.rm = TRUE)[[1]]
    )),
    absolute_held = unname(c(
      sum(
        rij[[1]][1, ] * s[, 1],
        rij[[2]][1, ] * s[, 2],
        rij[[3]][1, ] * s[, 3],
        na.rm = TRUE
      ),
      sum(
        rij[[1]][2, ] * s[, 1],
        rij[[2]][2, ] * s[, 2],
        rij[[3]][2, ] * s[, 3],
        na.rm = TRUE
      ),
      sum(
        rij[[1]][3, ] * s[, 1],
        rij[[2]][3, ] * s[, 2],
        rij[[3]][3, ] * s[, 3],
        na.rm = TRUE
      ),
      sum(
        rij[[1]][4, ] * s[, 1],
        rij[[2]][4, ] * s[, 2],
        rij[[3]][4, ] * s[, 3],
        na.rm = TRUE
      ),
      sum(
        rij[[1]][5, ] * s[, 1],
        rij[[2]][5, ] * s[, 2],
        rij[[3]][5, ] * s[, 3],
        na.rm = TRUE
      ),
      sum(rij[[1]][1, ] * s[, 1], na.rm = TRUE),
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
      sum(rij[[3]][5, ] * s[, 3], na.rm = TRUE)
    )),
    relative_held = absolute_held / total_amount
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("Raster (single zone)", {
  # load data
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
    sim_pu_raster, rep(c(0, 1),
    terra::ncell(sim_pu_raster) / 2)
  )
  s[is.na(sim_pu_raster)] <- NA_real_
  # calculate results
  r1 <- eval_feature_representation_summary(p1, s)
  expect_warning(
    r2 <- eval_feature_representation_summary(p2, raster::raster(s)),
    "deprecated"
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("Raster (multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create problems
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
    raster::setValues(
      sim_zones_pu_raster[[1]], rep(c(0, 0.2),
      terra::ncell(sim_zones_pu_raster) / 2)
    ),
    raster::setValues(
      sim_zones_pu_raster[[1]], rep(c(0.3, 0),
      terra::ncell(sim_zones_pu_raster) / 2)
    ),
    raster::setValues(
      sim_zones_pu_raster[[1]], rep(c(0.4, 0),
      terra::ncell(sim_zones_pu_raster) / 2)
    )
  )
  s[[1]][is.na(sim_zones_pu_raster[[1]])] <- NA_real_
  s[[2]][is.na(sim_zones_pu_raster[[2]])] <- NA_real_
  s[[3]][is.na(sim_zones_pu_raster[[3]])] <- NA_real_
  # calculate representation
  r1 <- eval_feature_representation_summary(p1, s)
  expect_warning(
    r2 <- eval_feature_representation_summary(p2, raster::stack(s)),
    "deprecated"
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("invalid inputs", {
  # create data
  pu <- data.frame(
    id = seq_len(10), cost = c(0.2, NA, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
  )
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_pu_polygons <- get_sim_pu_polygons()
  # tests
  expect_error(
    {
      # create problem
      p <- problem(
        pu$cost,
        data.frame(id = seq_len(2), name = c("spp1", "spp2")),
        as.matrix(t(pu[, 3:4]))
      )
      # create a solution
      s <- rep(c(0, 1), 5)
      # calculate representation
      eval_feature_representation_summary(p, s)
    },
    "NA values in the solution"
  )
  expect_error(
    {
      p <- problem(matrix(pu$cost, ncol = 1),
                   data.frame(id = seq_len(2), name = c("spp1", "spp2")),
                   as.matrix(t(pu[, 3:4])))
      # create a solution
      s <- matrix(rep(c(0, 1), 5), ncol = 1)
      # calculate representation
      eval_feature_representation_summary(p, s)
    },
    "NA values in the solution"
  )
  expect_error(
    {
      # create problem
      p <- problem(pu, c("spp1", "spp2"), cost_column = "cost")
      # create a solution
      s <- data.frame(solution = rep(c(0, 1), 5))
      # calculate representation
      eval_feature_representation_summary(p, s)
    },
    "NA values in the solution"
  )
  expect_error(
    {
      # create data
      pu <- sim_pu_polygons[seq_len(10), ]
      pu$cost[1:5] <- NA
      pu$solution <- rep(c(0, 1), 5)
      pu$spp1 <- runif(10)
      pu$spp2 <- c(rpois(9, 1), NA)
      # create problem
      p <- problem(pu, c("spp1", "spp2"), "cost")
      # create a solution
      s <- pu[, "solution"]
      # calculate representation
      eval_feature_representation_summary(p, s)
    },
    "NA values in the solution"
  )
  expect_error(
    {
      # create problem
      p <- problem(sim_pu_raster, sim_features)
      # create a solution
      s <- terra::setValues(
        sim_pu_raster,
        rep(c(0, 1), terra::ncell(sim_pu_raster) / 2)
      )
      # calculate representation
      eval_feature_representation_summary(p, s)
    },
    "NA values in the solution"
  )
})
