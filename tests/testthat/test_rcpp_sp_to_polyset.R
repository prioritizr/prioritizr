context("rcpp_sp_to_polyset")

test_that("SpatialPolygons", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  # calculate data
  x <- as.data.frame(
    rcpp_sp_to_polyset(sf::as_Spatial(sim_pu_polygons)@polygons, "Polygons")
  )
  # import correct result
  p <- system.file("testdata", "polyset_polygons.rds", package = "prioritizr")
  y <- readRDS(p)
  # tests
  expect_equal(x[[1]], y[[1]])
  expect_equal(x[[2]], y[[2]])
  expect_equal(x[[3]], y[[3]])
})

test_that("SpatialLines", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  # calculate data
  d <- as(sf::as_Spatial(sim_pu_polygons), "SpatialLines")
  x <- as.data.frame(rcpp_sp_to_polyset(d@lines, "Lines"))
  # import correct result
  p <- system.file("testdata", "polyset_lines.rds", package = "prioritizr")
  y <- readRDS(p)
  # tests
  expect_equal(x[[1]], y[[1]])
  expect_equal(x[[2]], y[[2]])
  expect_equal(x[[3]], y[[3]])
})
