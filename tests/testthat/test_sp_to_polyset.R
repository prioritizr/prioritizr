context("sp to PolySet")

test_that("SpatialPolygons", {
  data(sim_pu_polygons)
  p <- system.file("testdata", "polyset_polygons.rds", package = "prioritizr")
  x <- as.data.frame(rcpp_sp_to_polyset(sim_pu_polygons@polygons, "Polygons"))
  y <- readRDS(p)
  expect_equal(x[[1]], y[[1]])
  expect_equal(x[[2]], y[[2]])
  expect_equal(x[[3]], y[[3]])
})

test_that("SpatialLines", {
  data(sim_pu_polygons)
  p <- system.file("testdata", "polyset_lines.rds", package = "prioritizr")
  d <- as(sim_pu_polygons, "SpatialLines")
  x <- as.data.frame(rcpp_sp_to_polyset(d@lines, "Lines"))
  y <- readRDS(p)
  expect_equal(x[[1]], y[[1]])
  expect_equal(x[[2]], y[[2]])
  expect_equal(x[[3]], y[[3]])
})
