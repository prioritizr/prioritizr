test_that("works", {
  # data creation based on https://github.com/r-spatial/sf/blob/master/R/sfg.R
  p1 <- sf::st_point(c(1, 2))
  pts <- matrix(1:10, , 2)
  mp1 <- sf::st_multipoint(pts)
  ls1 <- sf::st_linestring(pts)
  outer <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)
  hole1 <- matrix(c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)
  hole2 <- matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE)
  pts <- list(outer, hole1, hole2)
  ml1 <- sf::st_multilinestring(pts)
  pl1 <- sf::st_polygon(pts)
  pol1 <- list(outer, hole1, hole2)
  pol2 <- list(outer + 12, hole1 + 12)
  pol3 <- list(outer + 24)
  mp <- list(pol1,pol2,pol3)
  mpl1 <- sf::st_multipolygon(mp)
  gc1 <- sf::st_geometrycollection(list(p1, ls1, pl1, mp1))
  # create sf object from all possible geometry classes
  pts_geom <- sf::st_as_sf(sf::st_sfc(list(p1)))
  lns_geom <- sf::st_as_sf(sf::st_sfc(list(ls1)))
  ply_geom <- sf::st_as_sf(sf::st_sfc(list(pl1)))
  mply_geom <- sf::st_as_sf(sf::st_sfc(list(mpl1)))
  all_geom  <- sf::st_as_sf(sf::st_sfc(list(p1, mp1, ls1, ml1, pl1, mpl1, gc1)))
  # tests
  expect_equal(st_geometry_classes(pts_geom), "POINT")
  expect_equal(st_geometry_classes(lns_geom), "LINESTRING")
  expect_equal(st_geometry_classes(ply_geom), "POLYGON")
  expect_equal(st_geometry_classes(mply_geom), "MULTIPOLYGON")
  expect_equal(
    st_geometry_classes(all_geom),
    c(
      "POINT", "MULTIPOINT", "LINESTRING",
      "MULTILINESTRING", "POLYGON",
      "MULTIPOLYGON", "GEOMETRYCOLLECTION"
    )
  )
})
