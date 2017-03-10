context("boundary_matrix")

test_that("SpatialPolygons (squares)", {
  # data
  x <- raster::rasterToPolygons(raster::raster(matrix(0:8, byrow = TRUE,
                                                      ncol = 3),
                                               xmn = 0, xmx = 3, ymn = 0,
                                               ymx = 3), n = 4)
  b <- boundary_matrix(x)
  s <- Matrix::sparseMatrix(
    i = c(0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8) + 1,
    j = c(0, 1, 3, 1, 2, 4, 2, 5, 3, 4, 6, 5, 7, 5, 8, 6, 7, 7, 8, 8) + 1,
    x = c(2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2),
    symmetric = TRUE, giveCsparse = FALSE)
  # tests
  expect_true(all(b == s))
})

test_that("SpatialPolygons (hexagons)", {
  # data
  set.seed(401)
  x <- sp::spsample(as(raster::extent(c(0, 5, 0, 5)), "SpatialPolygons"),
                    type = "hexagonal", cellsize = 1 * sqrt(3))
  x <- sp::HexPoints2SpatialPolygons(x)
  b <- boundary_matrix(x)
  s <- Matrix::sparseMatrix(
    i = c(0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 7),
    j = c(0, 1, 3, 1, 2, 3, 4, 2, 4, 3, 4, 5, 6, 4, 6, 7, 5, 6, 6, 7, 7),
    x = c(4, 1, 1, 2, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 2, 1, 4),
    index1 = FALSE, symmetric = TRUE)
  # tests
  expect_true(inherits(b, "dsCMatrix"))
  expect_true(min(b - s) < 1e-10)
})

test_that("SpatialPolygons (real data - simple shapes)", {
  # load data
  data(tas_pu, package = "prioritizrdata")
  x <- tas_pu[c(300, 279),]
  b <- boundary_matrix(x)
  # calculate total length
  total_length <- rgeos::gLength(x, byid = TRUE)
  shared_length <- rgeos::gLength(rgeos::gIntersection(
    methods::as(x[1, ], "SpatialLines"), methods::as(x[2, ], "SpatialLines")))
  # make correct matrix
  s <- Matrix::sparseMatrix(
    i = c(0, 0, 1),
    j = c(0, 1, 1),
    x = c(total_length[1] - shared_length, shared_length,
          total_length[2] - shared_length),
    index1 = FALSE, symmetric = TRUE)
  # tests
  expect_true(inherits(b, "dsCMatrix"))
  expect_true(min(b - s) < 1e-10)
})

test_that("SpatialPolygons (real data - complex shapes)", {
  skip_if_not_installed("prioritizrdata")
  # load data
  data(tas_pu, package = "prioritizrdata")
  x <- tas_pu[c(2,4), ]
  b <- boundary_matrix(x)
  # calculate total length
  total_length <- rgeos::gLength(x, byid = TRUE)
  shared_length <- rgeos::gLength(rgeos::gIntersection(
    methods::as(x[1, ], "SpatialLines"), methods::as(x[2, ], "SpatialLines")))
  # make correct matrix
  s <- Matrix::sparseMatrix(
    i = c(0, 0, 1),
    j = c(0, 1, 1),
    x = c(total_length[1] - shared_length, shared_length,
          total_length[2] - shared_length),
    index1 = FALSE, symmetric = TRUE)
  # tests
  expect_true(inherits(b, "dsCMatrix"))
  expect_true(min(b - s) < 1e-10)
})

test_that("RasterLayer", {
  # data
  x <- raster::raster(matrix(c(NA, 2:9), ncol = 3),
               xmn = 0, ymn = 0, xmx = 6, ymx = 3)
  b <- boundary_matrix(x)
  s <- boundary_matrix(raster::rasterToPolygons(x, n = 4))
  # tests
  expect_true(all(b == s))
})

test_that("SpatialLines", {
  # data
  x <- sp::SpatialLines(list(
    sp::Lines(ID = "1", list(sp::Line(matrix(
      c(
      0, 0,
      1, 1,
      2, 2), ncol = 2, byrow = TRUE)))),
    sp::Lines(ID = "2", list(sp::Line(matrix(
      c(
      2, 2,
      3, 3,
      4, 4), ncol = 2, byrow = TRUE)))),
    sp::Lines(ID = "3", list(sp::Line(matrix(
      c(
      5, 5,
      7, 7), ncol = 2, byrow = TRUE))))))
  # tests
  expect_error(boundary_matrix(x))
})

test_that("SpatialPoints", {
  # data
  x <- sp::SpatialPoints(coords = matrix(runif(10), ncol = 2))
  # tests
  expect_error(boundary_matrix(x))
})
