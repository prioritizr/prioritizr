context("internal functions")

test_that("is_comparable_raster (valid inputs)", {
  # load data
  data(sim_pu_raster, sim_features)
  # expect true
  expect_true(is_comparable_raster(sim_pu_raster, sim_features))
  expect_true(assertthat::assert_that(
    is_comparable_raster(sim_pu_raster, sim_features)))
  # expect false
  expect_false({
    data(sim_pu_raster, sim_features)
    sim_features@crs <- sp::CRS("+init=epsg:4326")
    is_comparable_raster(sim_pu_raster, sim_features)
  })
  expect_false({
    data(sim_pu_raster, sim_features)
    is_comparable_raster(
      raster::crop(sim_pu_raster, raster::extent(sim_pu_raster, 1, 5, 1, 5)),
                   sim_features)
  })
  # expect error
  expect_error({
    data(sim_pu_raster, sim_features)
    sim_features@crs <- sp::CRS("+init=epsg:4326")
    assertthat::assert_that(is_comparable_raster(sim_pu_raster, sim_features))
  })
  expect_error({
    data(sim_pu_raster, sim_features)
    assertthat::assert_that(is_comparable_raster(
      raster::crop(sim_pu_raster, raster::extent(sim_pu_raster, 1, 5, 1, 5)),
                   sim_features))
  })
})

test_that("is_comparable_raster (invalid inputs)", {
  data(sim_pu_raster, sim_features)
  expect_error({
    is_comparable_raster(sim_pu_raster, "a")
  })
})
