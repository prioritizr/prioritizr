context("add_default_solver")

test_that("raster planning unit data", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(any_solvers_installed())
  # make data
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_default_solver(time_limit = 5, verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "Raster"))
  expect_equal(raster::nlayers(s), 1)
  expect_true(raster::compareRaster(sim_pu_raster, s, res = TRUE, crs = TRUE,
              tolerance = 1e-5, stopiffalse = FALSE))
})

test_that("spatial planning unit data", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(any_solvers_installed())
  # make data
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_default_solver(time_limit = 5, verbose = FALSE)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "SpatialPolygonsDataFrame"))
  expect_equal(length(s), length(sim_pu_polygons))
  expect_equal(s@polygons, sim_pu_polygons@polygons)
  expect_true(sf::st_crs(s@proj4string) ==
              sf::st_crs(sim_pu_polygons@proj4string))
})
