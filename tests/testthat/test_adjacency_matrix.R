test_that("SpatRaster", {
  # create data
  x <- terra::rast(
    matrix(c(NA, 2:9), ncol = 3),
    extent = terra::ext(0, 6, 0, 3)
  )
  # create matrices
  m <- adjacency_matrix(x, directions = 4)
  s <- boundary_matrix(x)
  s[s > 0] <- 1
  Matrix::diag(s) <- 0
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_true(all(m == s))
})

test_that("sf (polygons, connected data)", {
  # import data
  r <- terra::rast(
    matrix(0:8, byrow = TRUE, ncol = 3),
    extent = terra::ext(0, 3, 0, 3)
  )
  x <- sf::st_as_sf(terra::as.polygons(r))
  # create matrices
  s <- adjacency_matrix(r, directions = 8)
  m <- adjacency_matrix(x)
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_true(all(m == s))
})

test_that("sf (polygons, unconnected data)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  # create matrices
  m <- adjacency_matrix(sim_pu_polygons[c(1, 3), ])
  s <- Matrix::sparseMatrix(
    i = integer(0), j = integer(0), x = numeric(0), dims = c(2, 2)
  )
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_true(all(m == s))
})

test_that("sf (lines, connected data)", {
  # create data
  x <- sf::st_sf(
    geometry = sf::st_sfc(list(
      sf::st_linestring(matrix(c(0, 0, 1, 1, 2, 2), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(2, 2, 3, 3, 4, 4), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(5, 5, 7, 7), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(0, 1, 4, 1), ncol = 2, byrow = TRUE))
    ))
  )
  # create matrices
  m <- adjacency_matrix(x)
  s <- Matrix::sparseMatrix(
    i = c(1, 1), j = c(2, 4), x = c(1, 1), dims = c(4, 4), symmetric = TRUE
  )
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_true(all(s == m))
})

test_that("sf (lines, unconnected data)", {
  # import data
  sim_pu_lines <- get_sim_pu_lines()
  # create matrices
  m <- adjacency_matrix(sim_pu_lines[c(1, 3), ])
  s <- Matrix::sparseMatrix(
    i = integer(0), j = integer(0), x = numeric(0), dims = c(2, 2)
  )
  # tests
  expect_true(inherits(m, "dsCMatrix"))
  expect_true(all(s == m))
})

test_that("sf (points)", {
  # import data
  x <- get_sim_pu_points()
  # tests
  expect_tidy_error(adjacency_matrix(x))
})

test_that("Spatial", {
  # polygons
  m1 <- adjacency_matrix(get_sim_pu_polygons())
  expect_warning(
    m2 <- adjacency_matrix(sf::as_Spatial(get_sim_pu_polygons())),
    "deprecated"
  )
  expect_equal(m1, m2)
  # lines
  m1 <- adjacency_matrix(get_sim_pu_lines())
  expect_warning(
    m2 <- adjacency_matrix(sf::as_Spatial(get_sim_pu_lines())),
    "deprecated"
  )
  expect_equal(m1, m2)
  # points
  expect_tidy_error(
    suppressWarnings(
      adjacency_matrix(sf::as_Spatial(get_sim_pu_points()))
    )
  )
})

test_that("Raster", {
  m1 <- adjacency_matrix(get_sim_pu_raster())
  expect_warning(
    m2 <- adjacency_matrix(raster::raster(get_sim_pu_raster())),
    "deprecated"
  )
  expect_equal(m1, m2)
})

test_that("invalid input", {
  # create data
  data(iris)
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
    adjacency_matrix(x),
    "GEOMETRYCOLLECTION"
  )
  expect_tidy_error(
    adjacency_matrix(4),
    "must be"
  )
  expect_tidy_error(
    adjacency_matrix(iris),
    "must be"
  )
})
