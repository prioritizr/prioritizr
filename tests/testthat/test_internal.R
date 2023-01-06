context("internal functions")

test_that("is_comparable_raster (valid inputs)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # expect true
  expect_true(is_comparable_raster(sim_pu_raster, sim_features))
  expect_true(assertthat::assert_that(
    is_comparable_raster(sim_pu_raster, sim_features)))
  # expect false
  expect_false({
    sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
    sim_features@crs <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
    is_comparable_raster(sim_pu_raster, sim_features)
  })
  expect_false({
    sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
    is_comparable_raster(
      raster::crop(sim_pu_raster, raster::extent(sim_pu_raster, 1, 5, 1, 5)),
                   sim_features)
  })
  # expect error
  expect_error({
    sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
    sim_features@crs <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
    assertthat::assert_that(is_comparable_raster(sim_pu_raster, sim_features))
  })
  expect_error({
    sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
    assertthat::assert_that(is_comparable_raster(
      raster::crop(sim_pu_raster, raster::extent(sim_pu_raster, 1, 5, 1, 5)),
                   sim_features))
  })
})

test_that("is_comparable_raster (invalid inputs)", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  expect_error({
    is_comparable_raster(sim_pu_raster, "a")
  })
})

test_that("geometry_classes", {
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
  x <- sf::st_as_sf(st_sfc(list(p1, mp1, ls1, ml1, pl1, mpl1, gc1)))
  # tests
  expect_equal(geometry_classes(x), c("POINT", "MULTIPOINT", "LINESTRING",
                                      "MULTILINESTRING", "POLYGON",
                                      "MULTIPOLYGON", "GEOMETRYCOLLECTION"))
})

test_that("intersecting_extents", {
  expect_true(intersecting_extents(
    sim_pu_polygons[1, ], sim_pu_polygons[1:10, ]))
  expect_true(intersecting_extents(
    sim_pu_polygons[1, ], sim_pu_polygons[1:10, ]))
  expect_true(intersecting_extents(
    sim_pu_polygons[1, ], sim_pu_polygons[1:10, ]))
  expect_true(intersecting_extents(
    sim_pu_polygons[1, ], sim_pu_raster))
  expect_true(intersecting_extents(
    sim_pu_polygons[1, ], sim_pu_raster))
  expect_true(intersecting_extents(
    raster::crop(sim_pu_raster, sim_pu_polygons[1, ]), sim_pu_raster))
  expect_false(intersecting_extents(
    sim_pu_polygons[1, ], sim_pu_polygons[7, ]))
  expect_false(intersecting_extents(
    sim_pu_polygons[1, ], sim_pu_polygons[7, ]))
  expect_false(intersecting_extents(
    sim_pu_polygons[1, ], sim_pu_polygons[7, ]))
  expect_false(intersecting_extents(
    sim_pu_polygons[1, ], raster::crop(sim_pu_raster, sim_pu_polygons[7, ])))
  expect_false(intersecting_extents(
    sim_pu_polygons[1, ], raster::crop(sim_pu_raster, sim_pu_polygons[7, ])))
  expect_false(intersecting_extents(
    raster::crop(sim_pu_raster, sim_pu_polygons[1, ]),
    raster::crop(sim_pu_raster, sim_pu_polygons[7, ])))
})

test_that("align_text", {
  expect_equal(
    align_text("animals: horse\npig\nbear", 9),
    "animals: horse\n         pig\n         bear"
  )
})

test_that("matrix_to_triplet_dataframe", {
  skip_if_not(utils::packageVersion("Matrix") >= 1.3)
  expect_equal(
    matrix_to_triplet_dataframe(
      Matrix::sparseMatrix(i = 1:3, j = c(1, 1, 2), x = 4:6, repr = "T")
    ),
    data.frame(i = 1:3, j = c(1, 1, 2), x = 4:6)
  )
  expect_equal(
    matrix_to_triplet_dataframe(
      Matrix::sparseMatrix(i = 1:3, j = c(1, 1, 2), x = 4:6, repr = "C")
    ),
    data.frame(i = 1:3, j = c(1, 1, 2), x = 4:6)
  )
  expect_equal(
    matrix_to_triplet_dataframe(
      Matrix::sparseMatrix(
        i = 1:3, j = c(1, 1, 2), x = 4:6, repr = "C", symmetric = TRUE
      )
    ),
    data.frame(i = 1:3, j = c(1, 1, 2), x = 4:6)
  )
})
