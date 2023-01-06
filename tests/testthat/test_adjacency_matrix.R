context("adjacency matrix")

test_that("RasterLayer", {
  # data
  x <- raster::raster(matrix(c(NA, 2:9), ncol = 3),
                      xmn = 0, ymn = 0, xmx = 6, ymx = 3)
  m <- adjacency_matrix(x, directions = 4)
  s <- boundary_matrix(x)
  s[s > 0] <- 1
  Matrix::diag(s) <- 0
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_true(all(m == s))
})

test_that("SpatialPolygons (connected data)", {
  # data
  r <- raster::raster(matrix(0:8, byrow = TRUE, ncol = 3),
                      xmn = 0, xmx = 3, ymn = 0, ymx = 3)
  s <- adjacency_matrix(r, directions = 8)
  x <- raster::rasterToPolygons(r, n = 4)
  m <- adjacency_matrix(x)
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("SpatialPolygons (unconnected data)", {
  sim_pu_polygons <- get_sim_pu_polygons()
  expect_true(all(Matrix::sparseMatrix(i = integer(0), j = integer(0),
                                       x = numeric(0), dims = c(2, 2)) ==
                  adjacency_matrix(sim_pu_polygons[c(1, 3), ])))
})

test_that("sf (connected data)", {
  # data
  r <- raster::raster(matrix(0:8, byrow = TRUE, ncol = 3),
                      xmn = 0, xmx = 3, ymn = 0, ymx = 3)
  s <- adjacency_matrix(r, directions = 8)
  x <- sf::st_as_sf(raster::rasterToPolygons(r, n = 4))
  m <- adjacency_matrix(x)
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("sf (unconnected data)", {
  sim_pu_polygons <- get_sim_pu_polygons()
  expect_true(all(Matrix::sparseMatrix(i = integer(0), j = integer(0),
                                       x = numeric(0), dims = c(2, 2)) ==
                  adjacency_matrix(sim_pu_polygons[c(1, 3), ])))
})

test_that("SpatialLines (connected data)", {
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
      7, 7), ncol = 2, byrow = TRUE)))),
    sp::Lines(ID = "4", list(sp::Line(matrix(
      c(
      0, 1,
      4, 1), ncol = 2, byrow = TRUE))))))
  m <- adjacency_matrix(x)
  s <- Matrix::sparseMatrix(
    i = c(1, 1),
    j = c(2, 4),
    x = c(1, 1),
    dims = c(4, 4), symmetric = TRUE)
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("SpatialLines (unconnected data)", {
  sim_pu_lines <- get_sim_pu_lines()
  expect_true(all(Matrix::sparseMatrix(i = integer(0), j = integer(0),
                                       x = numeric(0), dims = c(2, 2)) ==
                  adjacency_matrix(sim_pu_lines[c(1, 3), ])))
})

test_that("SpatialPoints (connected data)", {
  # data
  x <- sp::SpatialPoints(matrix(c(
    0, 0,
    0, 5,
    0, 10,
    100, 100
  ), byrow = TRUE, ncol = 2))
  expect_error(adjacency_matrix(x))
})

test_that("SpatialPoints (unconnected data)", {
  sim_pu_points <- get_sim_pu_points()
  expect_error(adjacency_matrix(sim_pu_points))
})
