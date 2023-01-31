context("proximity_matrix")

test_that("SpatRaster (adjacent non-NA pixels are proximal)", {
  # create data
  x <- terra::rast(
    matrix(c(NA, 2:9), ncol = 3),
    extent = terra::ext(0, 3, 0, 3)
  )
  # create matrix
  m <- proximity_matrix(x, distance = 1)
  # calculate correct results
  s <- boundary_matrix(x)
  s[s > 0] <- 1
  Matrix::diag(s) <- 0
  s <- Matrix::drop0(as(s, "symmetricMatrix"))
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_true(all(m == s))
})

test_that("SpatRaster (all non-NA pixels are proximal)", {
  # create data
  x <- terra::rast(
    matrix(c(NA, 2:8, NA), byrow = TRUE, ncol = 3),
    extent = terra::ext(0, 3, 0, 3)
  )
  # create matrix
  m <- proximity_matrix(x, distance = 100)
  # create correct results
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

test_that("SpatRaster (none are proximal)", {
  # create data
  x <- terra::rast(
    matrix(c(NA, 2:8, NA), byrow = TRUE, ncol = 3),
    extent = terra::ext(0, 3, 0, 3)
  )
  # create matrix
  m <- proximity_matrix(x, distance = 1e-3)
  # create correct results
  s <- Matrix::sparseMatrix(
    i = integer(0), j = integer(0), x = numeric(0), dims = c(9, 9)
  )
  s <- as(s, "symmetricMatrix")
  # tests
  expect_equal(s, m)
})

test_that("SpatRaster (multiple layers)", {
  # create data
  x <- c(
    terra::rast(
      matrix(c(NA, NA, 3:9), ncol = 3),
      extent = terra::ext(0, 3, 0, 3)
    ),
    terra::rast(
      matrix(c(NA, 2:9), ncol = 3),
      extent = terra::ext(0, 3, 0, 3)
    )
  )
  # create matrix
  m <- proximity_matrix(x, distance = 1)
  # calculate correct result
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

test_that("sf (all polygons are proximal)", {
  # create data
  x <- terra::rast(
    matrix(0:8, byrow = TRUE, ncol = 3),
    extent = terra::ext(0, 3, 0, 3)
  )
  x <- sf::st_as_sf(terra::as.polygons(x))
  # calculate matrix
  m <- proximity_matrix(x, distance = 100)
  # create correct matrix
  s <- matrix(1, ncol = 9, nrow = 9)
  diag(s) <- 0
  s <- as(as_Matrix(s, "dgCMatrix"), "symmetricMatrix")
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("sf (adjacent polygons are proximal)", {
  # create data
  x <- terra::rast(
    matrix(0:8, byrow = TRUE, ncol = 3),
    extent = terra::ext(0, 3, 0, 3)
  )
  x <- sf::st_as_sf(terra::as.polygons(x))
  # calculate matrix
  m <- proximity_matrix(x, distance = 0.1)
  # calculate correct result
  s <- adjacency_matrix(x)
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("sf (all polygons are proximal)", {
  # create data
  x <- terra::rast(
    matrix(0:8, byrow = TRUE, ncol = 3),
    extent = terra::ext(0, 3, 0, 3)
  )
  x <- sf::st_as_sf(terra::as.polygons(x))
  # create matrix
  m <- proximity_matrix(x, distance = 100)
  # create correct result
  s <- matrix(1, ncol = 9, nrow = 9)
  diag(s) <- 0
  s <- as(as_Matrix(s, "dgCMatrix"), "symmetricMatrix")
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("sf (no polygons are proximal)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  # create matrix
  m <- proximity_matrix(sim_pu_polygons[c(1, 3), ], distance = 0.01)
  # calculate correct result
  s <- Matrix::sparseMatrix(
    i = integer(0), j = integer(0), x = numeric(0), dims = c(2, 2)
  )
  s <- as(s, "symmetricMatrix")
  # tests
  expect_equal(s, m)
})

test_that("sf (adjacent polygons are proximal)", {
  # create data
  x <- terra::rast(
    matrix(0:8, byrow = TRUE, ncol = 3),
    extent = terra::ext(0, 3, 0, 3)
  )
  x <- sf::st_as_sf(terra::as.polygons(x))
  # create matrix
  m <- proximity_matrix(x, distance = 0.1)
  # calculate correct results
  s <- adjacency_matrix(x)
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("sf (all polygons are proximal)", {
  # create data
  x <- terra::rast(
    matrix(0:8, byrow = TRUE, ncol = 3),
    extent = terra::ext(0, 3, 0, 3)
  )
  x <- sf::st_as_sf(terra::as.polygons(x))
  # create matrix
  m <- proximity_matrix(x, distance = 100)
  # create correct result
  s <- matrix(1, ncol = 9, nrow = 9)
  diag(s) <- 0
  s <- as(as_Matrix(s, "dgCMatrix"), "symmetricMatrix")
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("sf (intersecting lines are proximal)", {
  # create data
  x <- sf::st_as_sf(
    tibble::tibble(
      geometry = sf::st_sfc(
        sf::st_linestring(
          matrix(
            c(
              0, 0,
              1, 1,
              2, 2
            ),
            ncol = 2, byrow = TRUE
          )
        ),
        sf::st_linestring(
          matrix(
            c(
              2, 2,
              3, 3,
              4, 4
            ),
            ncol = 2, byrow = TRUE
          )
        ),
        sf::st_linestring(
          matrix(
            c(
              5, 5,
              7, 7
            ),
            ncol = 2, byrow = TRUE
          )
        ),
        sf::st_linestring(
          matrix(
            c(
              0, 1,
              4, 1
            ),
            ncol = 2, byrow = TRUE
          )
        )
      )
    )
  )
  # create matrix
  m <- proximity_matrix(x, distance = 1e-3)
  # create correct matrix
  s <- Matrix::sparseMatrix(
    i = c(1, 1),
    j = c(2, 4),
    x = c(1, 1),
    dims = c(4, 4),
    symmetric = TRUE
  )
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("sf (no proximal lines)", {
  # import data
  sim_pu_lines <- get_sim_pu_lines()
  # create data
  x <- sim_pu_lines[c(1, 3), ]
  # create matrix
  m <- proximity_matrix(x, distance = 1e-3)
  # create correct matrix
  s <- Matrix::sparseMatrix(
    i = integer(0), j = integer(0), x = numeric(0), dims = c(2, 2)
  )
  s <- as(s, "symmetricMatrix")
  # tests
  expect_equal(s, m)
})

test_that("sf (some points are proximal)", {
  # create data
  r <- terra::rast(
    matrix(0:8, byrow = TRUE, ncol = 3),
    extent = terra::ext(0, 3, 0, 3)
  )
  x <- suppressWarnings(sf::st_centroid(sf::st_as_sf(terra::as.polygons(r))))
  # create matrix
  m <- proximity_matrix(x, distance = 1.0)
  # calculate correct result
  s <- adjacency_matrix(r)
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("sf (no points are proximal)", {
  # create data
  r <- terra::rast(
    matrix(0:8, byrow = TRUE, ncol = 3),
    extent = terra::ext(0, 3, 0, 3)
  )
  x <- suppressWarnings(sf::st_centroid(sf::st_as_sf(terra::as.polygons(r))))
  # create matrix
  m <- proximity_matrix(x, distance = 1e-3)
  # create correct result
  s <- Matrix::sparseMatrix(
    i = integer(0), j = integer(0), x = numeric(0), dims = c(9, 9)
  )
  s <- as(s, "symmetricMatrix")
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_equal(s, m)
})

test_that("Spatial", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  # create matrices
  x <- proximity_matrix(sim_pu_polygons, 2)
  y <- proximity_matrix(sf::as_Spatial(sim_pu_polygons), 2)
  # tests
  expect_equal(x, y)
})

test_that("Raster (single layer)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  # create matrices
  x <- proximity_matrix(sim_pu_raster, 2)
  y <- proximity_matrix(raster::raster(sim_pu_raster), 2)
  # tests
  expect_equal(x, y)
})

test_that("Raster (multiple layers)", {
  # import data
  sim_features <- get_sim_features()
  # create matrices
  x <- proximity_matrix(sim_features, 2)
  y <- proximity_matrix(raster::stack(sim_features), 2)
  # tests
  expect_equal(x, y)
})

test_that("invalid input", {
  # create data
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
  # tests
  expect_tidy_error(
    proximity_matrix(x, 2),
    "GEOMETRYCOLLECTION"
  )
  expect_tidy_error(
    proximity_matrix(4, 1),
    "must be"
  )
})
