context("proximity matrix")

test_that("RasterLayer (adjacent non-NA pixels are proximal)", {
  # data
  x <- raster::raster(matrix(c(NA, 2:9), ncol = 3),
                      xmn = 0, ymn = 0, xmx = 3, ymx = 3)
  m <- proximity_matrix(x, distance = 1)
  s <- boundary_matrix(x)
  s[s > 0] <- 1
  Matrix::diag(s) <- 0
  s <- Matrix::drop0(as(s, "symmetricMatrix"))
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_true(all(m == s))
})

test_that("RasterLayer (all non-NA pixels are proximal)", {
  # data
  x <- raster::raster(matrix(c(NA, 2:8, NA), byrow = TRUE, ncol = 3),
                      xmn = 0, xmx = 3, ymn = 0, ymx = 3)
  m <- proximity_matrix(x, distance = 100)
  s <- matrix(1, ncol = 9, nrow = 9)
  diag(s) <- 0
  s[, 1] <- 0
  s[, 9] <- 0
  s[1, ] <- 0
  s[9, ] <- 0
  s <- as(as_Matrix(s, "dgCMatrix"), "symmetricMatrix")
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("RasterLayer (none are proximal)", {
  # data
  x <- raster::raster(matrix(c(NA, 2:8, NA), byrow = TRUE, ncol = 3),
                      xmn = 0, xmx = 3, ymn = 0, ymx = 3)
  m <- proximity_matrix(x, distance = 1e-3)
  s <- Matrix::sparseMatrix(i = integer(0), j = integer(0),
                            x = numeric(0), dims = c(2, 2))
  s <- as(s, "symmetricMatrix")
  m <- proximity_matrix(sim_pu_polygons[c(1, 3), ], distance = 0.01)
  # tests
  expect_equal(s, m)
})

test_that("RasterLayer (all polygons are proximal)", {
  # data
  x <- raster::raster(matrix(0:8, byrow = TRUE, ncol = 3),
                      xmn = 0, xmx = 3, ymn = 0, ymx = 3)
  x <- raster::rasterToPolygons(x, n = 4)
  m <- proximity_matrix(x, distance = 100)
  s <- matrix(1, ncol = 9, nrow = 9)
  diag(s) <- 0
  s <- as(as_Matrix(s, "dgCMatrix"), "symmetricMatrix")
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("RasterLayer (multiple layers)", {
  # data
  x <- raster::stack(
    raster::raster(
      matrix(c(NA, NA, 3:9), ncol = 3), xmn = 0, ymn = 0, xmx = 3, ymx = 3
    ),
    raster::raster(
      matrix(c(NA, 2:9), ncol = 3), xmn = 0, ymn = 0, xmx = 3, ymx = 3
    )
  )
  m <- proximity_matrix(x, distance = 1)
  s <- boundary_matrix(x)
  s[s > 0] <- 1
  Matrix::diag(s) <- 0
  s <- Matrix::drop0(as(s, "symmetricMatrix"))
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_true(all(m == s))
  expect_true(all(m[1, ] == 0))
  expect_true(all(m[, 1] == 0))
  expect_gt(min(Matrix::rowSums(m)[-1]), 0)
})

test_that("SpatialPolygons (adjacent polygons are proximal)", {
  # data
  x <- raster::raster(matrix(0:8, byrow = TRUE, ncol = 3),
                      xmn = 0, xmx = 3, ymn = 0, ymx = 3)
  x <- raster::rasterToPolygons(x, n = 4)
  s <- adjacency_matrix(x)
  m <- proximity_matrix(x, distance = 0.1)
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("SpatialPolygons (all polygons are proximal)", {
  # data
  x <- raster::raster(matrix(0:8, byrow = TRUE, ncol = 3),
                      xmn = 0, xmx = 3, ymn = 0, ymx = 3)
  x <- raster::rasterToPolygons(x, n = 4)
  m <- proximity_matrix(x, distance = 100)
  s <- matrix(1, ncol = 9, nrow = 9)
  diag(s) <- 0
  s <- as(as_Matrix(s, "dgCMatrix"), "symmetricMatrix")
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("SpatialPolygons (no polygons are proximal)", {
  data(sim_pu_polygons)
  s <- Matrix::sparseMatrix(i = integer(0), j = integer(0),
                            x = numeric(0), dims = c(2, 2))
  s <- as(s, "symmetricMatrix")
  m <- proximity_matrix(sim_pu_polygons[c(1, 3), ], distance = 0.01)
  expect_equal(s, m)
})

test_that("sf (adjacent polygons are proximal)", {
  # data
  x <- raster::raster(matrix(0:8, byrow = TRUE, ncol = 3),
                      xmn = 0, xmx = 3, ymn = 0, ymx = 3)
  x <- sf::st_as_sf(raster::rasterToPolygons(x, n = 4))
  s <- adjacency_matrix(x)
  m <- proximity_matrix(x, distance = 0.1)
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("sf (all polygons are proximal)", {
  # data
  x <- raster::raster(matrix(0:8, byrow = TRUE, ncol = 3),
                      xmn = 0, xmx = 3, ymn = 0, ymx = 3)
  x <- sf::st_as_sf(raster::rasterToPolygons(x, n = 4))
  m <- proximity_matrix(x, distance = 100)
  s <- matrix(1, ncol = 9, nrow = 9)
  diag(s) <- 0
  s <- as(as_Matrix(s, "dgCMatrix"), "symmetricMatrix")
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("sf (no polygons are proximal)", {
  data(sim_pu_polygons)
  x <- st_as_sf(sim_pu_polygons[c(1, 3), ])
  s <- Matrix::sparseMatrix(i = integer(0), j = integer(0),
                            x = numeric(0), dims = c(2, 2))
  s <- as(s, "symmetricMatrix")
  m <- proximity_matrix(x, distance = 0.01)
  expect_equal(s, m)
})

test_that("SpatialLines (intersecting lines are proximal)", {
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
  m <- proximity_matrix(x, distance = 1e-3)
  s <- Matrix::sparseMatrix(
    i = c(1, 1),
    j = c(2, 4),
    x = c(1, 1),
    dims = c(4, 4), symmetric = TRUE)
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("SpatialLines (no proximal lines)", {
  data(sim_pu_lines)
  x <- sim_pu_lines[c(1, 3), ]
  m <- proximity_matrix(x, distance = 1e-3)
  s <- Matrix::sparseMatrix(i = integer(0), j = integer(0),
                            x = numeric(0), dims = c(2, 2))
  s <- as(s, "symmetricMatrix")
  expect_equal(s, m)
})

test_that("SpatialPoints (some points are proximal)", {
  # data
  r <- raster::raster(matrix(0:8, byrow = TRUE, ncol = 3),
                      xmn = 0, xmx = 3, ymn = 0, ymx = 3)
  x <- suppressWarnings({
    sf::as_Spatial(
      sf::st_centroid(sf::st_as_sf(raster::rasterToPolygons(r, n = 4)))
    )
  })
  s <- adjacency_matrix(r)
  m <- proximity_matrix(x, distance = 1.0)
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("SpatialPoints (no points are proximal)", {
  # data
  r <- raster::raster(matrix(0:8, byrow = TRUE, ncol = 3),
                      xmn = 0, xmx = 3, ymn = 0, ymx = 3)
  x <- suppressWarnings({
    sf::as_Spatial(
      sf::st_centroid(sf::st_as_sf(raster::rasterToPolygons(r, n = 4)))
    )
  })
  m <- proximity_matrix(x, distance = 1e-3)
  s <- Matrix::sparseMatrix(i = integer(0), j = integer(0),
                            x = numeric(0), dims = c(9, 9))
  s <- as(s, "symmetricMatrix")
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("invalid input", {
  x <- sf::st_sf(
    id = c(1, 2),
    geom = sf::st_sfc(
      sf::st_point(x = c(1, 2)),
      sf::st_geometrycollection(
        list(
          sf::st_point(x = c(10, 20)),
          sf::st_point(x = c(100, 200))
        )
      )
    )
  )
  expect_error(proximity_matrix(x, 2))
  expect_error(proximity_matrix(4, 1))
})
