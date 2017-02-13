context('sp to PolySet')

test_that('SpatialPolygons', {
  data(sim_pu_polygons)
  x <- as.data.frame(rcpp_sp_to_polyset(sim_pu_polygons@polygons, 'Polygons'))
  s <- as.data.frame(maptools::SpatialPolygons2PolySet(sim_pu_polygons))
  expect_equal(x[[1]], s[[1]])
  expect_equal(x[[2]], s[[2]])
  expect_equal(x[[3]], s[[3]])
})

test_that('SpatialLines', {
  data(sim_pu_polygons)
  sim_pu_polygons <- as(sim_pu_polygons, 'SpatialLines')
  x <- as.data.frame(rcpp_sp_to_polyset(sim_pu_polygons@lines, 'Lines'))
  s <- as.data.frame(maptools::SpatialLines2PolySet(sim_pu_polygons))
  expect_equal(x[[1]], s[[1]])
  expect_equal(x[[2]], s[[2]])
  expect_equal(x[[3]], s[[3]])
})
