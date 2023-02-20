context("assertions_raster")

test_that("is_comparable_raster", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  # tests
  expect_true(is_comparable_raster(sim_pu_raster, sim_features))
  expect_true(is_comparable_raster(sim_zones_pu_raster, sim_zones_pu_raster))
  expect_error(
    {
      sim_pu_raster <- get_sim_pu_raster()
      sim_features <- get_sim_features()
      terra::crs(sim_features) <- as.character(sf::st_crs(4326))[[2]]
      assert(is_comparable_raster(sim_pu_raster, sim_features))
    },
    "comparable"
  )
  expect_error(
    {
      sim_pu_raster <- get_sim_pu_raster()
      sim_features <- get_sim_features()
      sim_pu_raster <- terra::crop(sim_pu_raster, terra::ext(1, 5, 1, 5))
      assert(is_comparable_raster(sim_pu_raster, sim_features))
    },
    "comparable"
  )
  expect_error(
    {
      sim_pu_raster <- get_sim_pu_raster()
      sim_features <- get_sim_features()
      terra::crs(sim_features) <- as.character(sf::st_crs(4326))[[2]]
      assert(is_comparable_raster(sim_features, sim_pu_raster))
    },
    "comparable"
  )
  expect_error(
    {
      sim_pu_raster <- get_sim_pu_raster()
      sim_features <- get_sim_features()
      sim_pu_raster <- terra::crop(sim_pu_raster, terra::ext(1, 5, 1, 5))
      assert(is_comparable_raster(sim_features, sim_pu_raster))
    },
    "comparable"
  )
  expect_error(is_comparable_raster(sim_pu_raster, "a"))
})

test_that("is_pu_comparable_raster", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features)
  # tests
  expect_true(is_pu_comparable_raster(p, sim_pu_raster))
  expect_error(
    {
      sim_pu_raster <- get_sim_pu_raster()
      terra::crs(sim_pu_raster) <- as.character(sf::st_crs(4326))[[2]]
      assert(is_pu_comparable_raster(p, sim_pu_raster))
    },
    "comparable"
  )
  expect_error(
    {
      sim_pu_raster <- get_sim_pu_raster()
      sim_pu_raster <- terra::crop(sim_pu_raster, terra::ext(1, 5, 1, 5))
      assert(is_pu_comparable_raster(p, sim_pu_raster))
    },
    "comparable"
  )
  expect_error(is_pu_comparable_raster(sim_pu_raster, "a"))
})
