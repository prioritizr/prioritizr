context("feature_abundances")

test_that("data.frame (na.rm = FALSE, single zone)", {
  # create data
  pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                 spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  p <- problem(pu, c("spp1", "spp2"), cost_column = "cost")
  # calculate abundances
  x <- feature_abundances(p, na.rm = FALSE)
  # run tests
  expect_is(x, "tbl_df")
  expect_equal(names(x), c("feature", "absolute_abundance",
                           "relative_abundance"))
  expect_equal(x$feature, c("spp1", "spp2"))
  expect_equal(x$absolute_abundance,
               c(sum(pu$spp1), sum(pu$spp2, na.rm = TRUE)))
  expect_equal(x$relative_abundance,
              c(sum(pu$spp1) / sum(pu$spp1),
                sum(pu$spp2, na.rm = TRUE) / sum(pu$spp2, na.rm = TRUE)))
})

test_that("data.frame (na.rm = TRUE, single zone)", {
  # create data
  pu <- data.frame(id = seq_len(10), cost = c(0.2, NA, runif(8)),
                 spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  p <- problem(pu, c("spp1", "spp2"), cost_column = "cost")
  # calculate abundances
  x <- feature_abundances(p, na.rm = TRUE)
  # run tests
  expect_is(x, "tbl_df")
  expect_equal(names(x), c("feature", "absolute_abundance",
                           "relative_abundance"))
  expect_equal(x$feature, c("spp1", "spp2"))
  expect_equal(x$absolute_abundance,
               c(sum(pu$spp1[!is.na(pu$cost)]),
                 sum(pu$spp2[!is.na(pu$cost)], na.rm = TRUE)))
  expect_equal(x$relative_abundance,
               c(sum(pu$spp1[!is.na(pu$cost)]) / sum(pu$spp1, na.rm = TRUE),
                 sum(pu$spp2[!is.na(pu$cost)], na.rm = TRUE) /
                 sum(pu$spp2, na.rm = TRUE)))
})

test_that("raster (na.rm = FALSE, single zone)", {
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features)
  # calculate abundances
  x <- feature_abundances(p, na.rm = FALSE)
  # run tests
  expect_is(x, "tbl_df")
  expect_equal(names(x), c("feature", "absolute_abundance",
                           "relative_abundance"))
  expect_equal(x$feature, names(sim_features))
  expect_equal(x$absolute_abundance,
               unname(raster::cellStats(sim_features, "sum")))
  expect_equal(x$relative_abundance, rep(1, terra::nlyr(sim_features)))
})

test_that("raster (na.rm = TRUE, single zone)", {
  # make data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features)
  # calculate abundances
  x <- feature_abundances(p, na.rm = TRUE)
  # run tests
  expect_is(x, "tbl_df")
  expect_equal(names(x), c("feature", "absolute_abundance",
                           "relative_abundance"))
  expect_equal(x$feature, names(sim_features))
  expect_equal(x$absolute_abundance,
               unname(colSums(sim_features[!is.na(sim_pu_raster)],
                              na.rm = TRUE)))
  expect_equal(x$relative_abundance,
               unname(colSums(sim_features[!is.na(sim_pu_raster)],
                              na.rm = TRUE)) /
               unname(raster::cellStats(sim_features, "sum")))
})

test_that("data.frame (na.rm = FALSE, multiple zones)", {
  # make data
  pu <- data.frame(id = seq_len(10), cost_1 = c(NA, NA, runif(8)),
                   cost_2 = c(0.3, NA, runif(8)),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  p <- problem(pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2"),
                         zone_names = c("z1", "z2"),
                         feature_names = c("spp1", "spp2")),
               c("cost_1", "cost_2"))
  # calculate abundances
  x <- feature_abundances(p, na.rm = FALSE)
  # run tests
  expect_is(x, "tbl_df")
  expect_equal(names(x), c("feature", "zone", "absolute_abundance",
                           "relative_abundance"))
  expect_equal(x$feature, rep(c("spp1", "spp2"), 2))
  expect_equal(x$zone, rep(c("z1", "z2"), each = 2))
  expect_equal(x$absolute_abundance,
               unname(colSums(pu[, c("spp1_1", "spp2_1", "spp1_2", "spp2_2")],
                              na.rm = TRUE)))
  expect_equal(x$relative_abundance, rep(1, 4))
})

test_that("data.frame (na.rm = TRUE, multiple zones)", {
  # make data
  pu <- data.frame(id = seq_len(10), cost_1 = c(NA, NA, runif(8)),
                   cost_2 = c(0.3, NA, runif(8)),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  p <- problem(pu, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2"),
                         zone_names = c("z1", "z2"),
                         feature_names = c("spp1", "spp2")),
               c("cost_1", "cost_2"))
  # calculate abundances
  x <- feature_abundances(p, na.rm = TRUE)
  # run tests
  expect_is(x, "tbl_df")
  expect_equal(names(x), c("feature", "zone", "absolute_abundance",
                           "relative_abundance"))
  expect_equal(x$feature, rep(c("spp1", "spp2"), 2))
  expect_equal(x$zone, rep(c("z1", "z2"), each = 2))
  expect_equal(x$absolute_abundance,
               unname(c(colSums(pu[!is.na(pu$cost_1), c("spp1_1", "spp2_1")],
                                na.rm = TRUE),
                        colSums(pu[!is.na(pu$cost_2), c("spp1_2", "spp2_2")],
                                na.rm = TRUE))))
  expect_equal(x$relative_abundance,
               unname(c(colSums(pu[!is.na(pu$cost_1), c("spp1_1", "spp2_1")],
                                na.rm = TRUE),
                        colSums(pu[!is.na(pu$cost_2), c("spp1_2", "spp2_2")],
                                na.rm = TRUE))) /
               unname(colSums(pu[, c("spp1_1", "spp2_1", "spp1_2", "spp2_2")],
                      na.rm = TRUE)))
})

test_that("raster (na.rm = FALSE, multiple zones)", {
  # make data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  # calculate abundances
  x <- feature_abundances(p, na.rm = FALSE)
  # run tests
  expect_is(x, "tbl_df")
  expect_equal(names(x), c("feature", "zone", "absolute_abundance",
                           "relative_abundance"))
  expect_equal(x$feature, rep(feature_names(sim_zones_features),
                              number_of_zones(sim_zones_features)))
  expect_equal(x$zone, rep(zone_names(sim_zones_features),
                           each = number_of_features(sim_zones_features)))
  expect_equal(x$absolute_abundance,
               unname(raster::cellStats(terra::rast(as.list(
                 sim_zones_features)), "sum")))
   expect_equal(x$relative_abundance,
                rep(1, number_of_zones(sim_zones_features) *
                       number_of_features(sim_zones_features)))
})

test_that("raster (na.rm = TRUE, multiple zones)", {
  # make data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  p <- problem(sim_zones_pu_raster, sim_zones_features)
  # calculate abundances
  x <- feature_abundances(p, na.rm = TRUE)
  # run tests
  expect_is(x, "tbl_df")
  expect_equal(names(x), c("feature", "zone", "absolute_abundance",
                           "relative_abundance"))
  expect_equal(x$feature, rep(feature_names(sim_zones_features),
                              number_of_zones(sim_zones_features)))
  expect_equal(x$zone, rep(zone_names(sim_zones_features),
                           each = number_of_features(sim_zones_features)))
  expect_equal(x$absolute_abundance,
    unname(raster::cellStats(
      terra::rast(as.list(sim_zones_features)) *
      !is.na(sim_zones_pu_raster[[rep(seq_len(3),
        each = number_of_features(sim_zones_features))]]),
      "sum")))
  expect_equal(x$relative_abundance,
    unname(raster::cellStats(
      terra::rast(as.list(sim_zones_features)) *
      !is.na(sim_zones_pu_raster[[rep(seq_len(3),
        each = number_of_features(sim_zones_features))]]),
      "sum")) /
    unname(raster::cellStats(terra::rast(as.list(sim_zones_features)),
      "sum")))
})
