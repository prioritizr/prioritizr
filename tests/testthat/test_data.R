test_that("simulated single zone data", {
  # load data
  d1 <- get_sim_pu_raster()
  d2 <- get_sim_locked_in_raster()
  d3 <- get_sim_locked_out_raster()
  d4 <- get_sim_pu_polygons()
  d5 <- get_sim_pu_points()
  d6 <- get_sim_pu_lines()
  d7 <- get_sim_features()
  d8 <- get_sim_phylogeny()
  # tests
  ## classes
  expect_inherits(d1, "SpatRaster")
  expect_inherits(d2, "SpatRaster")
  expect_inherits(d3, "SpatRaster")
  expect_inherits(d4, "sf")
  expect_inherits(d5, "sf")
  expect_inherits(d6, "sf")
  expect_inherits(d7, "SpatRaster")
  expect_inherits(d8, "phylo")
  ## compatibility
  expect_true(is_comparable_raster(d1, d2))
  expect_true(is_comparable_raster(d1, d3))
  expect_true(is_comparable_raster(d1, d7))
  expect_true(is_spatial_extents_overlap(d4, d7))
  expect_true(is_spatial_extents_overlap(d5, d7))
  expect_true(is_spatial_extents_overlap(d6, d7))
  expect_true(setequal(names(d7), d8$tip.label))
})

test_that("simulated complex data", {
  # load data
  d1 <- get_sim_complex_pu_raster()
  d2 <- get_sim_complex_locked_in_raster()
  d3 <- get_sim_complex_locked_out_raster()
  d4 <- get_sim_complex_features()
  d5 <- get_sim_complex_historical_features()
  # tests
  ## classes
  expect_inherits(d1, "SpatRaster")
  expect_inherits(d2, "SpatRaster")
  expect_inherits(d3, "SpatRaster")
  expect_inherits(d4, "SpatRaster")
  ## compatibility
  expect_true(is_comparable_raster(d1, d2))
  expect_true(is_comparable_raster(d1, d3))
  expect_true(is_comparable_raster(d1, d4))
  expect_true(is_comparable_raster(d1, d5))
  expect_true(identical(names(d4), names(d5)))
})

test_that("simulated multi-zone data", {
  # load data
  d1 <- get_sim_zones_pu_raster()
  d2 <- get_sim_zones_pu_polygons()
  d3 <- get_sim_zones_features()
  # tests
  ## classes
  expect_inherits(d1, "SpatRaster")
  expect_inherits(d2, "sf")
  expect_inherits(d3, "ZonesSpatRaster")
  ## compatibility
  expect_true(is_comparable_raster(d1, d3))
  expect_true(is_spatial_extents_overlap(d2, d3))
})
