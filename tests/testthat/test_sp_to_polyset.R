context('sp to PolySet')

test_that('SpatialPolygons', {
  data(tas_pu)
  x <- as.data.frame(rcpp_sp_to_polyset(tas_pu@polygons, 'Polygons'))
  s <- as.data.frame(maptools::SpatialPolygons2PolySet(tas_pu))
  expect_equal(x[[1]], s[[1]])
  expect_equal(x[[2]], s[[2]])
  expect_equal(x[[3]], s[[3]])
})

test_that('SpatialLines', {
  data(tas_pu)
  tas_pu <- as(tas_pu, 'SpatialLines')
  x <- as.data.frame(rcpp_sp_to_polyset(tas_pu@lines, 'Lines'))
  s <- as.data.frame(maptools::SpatialLines2PolySet(tas_pu))
  expect_equal(x[[1]], s[[1]])
  expect_equal(x[[2]], s[[2]])
  expect_equal(x[[3]], s[[3]])
})
