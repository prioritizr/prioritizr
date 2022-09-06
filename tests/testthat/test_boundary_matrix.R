context("boundary_matrix")

test_that("SpatialPolygons (squares)", {
  # data
  d <- raster::rasterToPolygons(raster::raster(matrix(0:8, byrow = TRUE,
                                                      ncol = 3),
                                               xmn = 0, xmx = 3, ymn = 0,
                                               ymx = 3), n = 4)
  x <- boundary_matrix(d)
  y <- triplet_sparse_matrix(
    i = c(0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8) + 1,
    j = c(0, 1, 3, 1, 2, 4, 2, 5, 3, 4, 6, 5, 7, 5, 8, 6, 7, 7, 8, 8) + 1,
    x = c(2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2),
    symmetric = TRUE)
  # tests
  expect_is(x, "dsCMatrix")
  expect_true(all(x == y))
})

test_that("SpatialPolygons (hexagons)", {
  # data
  set.seed(401)
  d <- sp::spsample(as(raster::extent(c(0, 5, 0, 5)), "SpatialPolygons"),
                    type = "hexagonal", cellsize = 1 * sqrt(3))
  d <- sp::HexPoints2SpatialPolygons(d)
  x <- boundary_matrix(d)
  y <- Matrix::sparseMatrix(
    i = c(0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 7),
    j = c(0, 1, 3, 1, 2, 3, 4, 2, 4, 3, 4, 5, 6, 4, 6, 7, 5, 6, 6, 7, 7),
    x = c(4, 1, 1, 2, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 2, 1, 4),
    index1 = FALSE, symmetric = TRUE)
  # tests
  expect_is(x, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})

test_that("SpatialPolygons (real data - simple shapes)", {
  skip_if_not_installed("prioritizrdata")
  # load data
  data(tas_pu, package = "prioritizrdata")
  d <- tas_pu[c(300, 279), ]
  x <- boundary_matrix(d)
  # calculate total length
  d2 <- sf::st_geometry(sf::st_cast(sf::st_as_sf(d), "MULTILINESTRING"))
  total_length <- as.numeric(sf::st_length(d2))
  shared_length <- as.numeric(
    sf::st_length(sf::st_intersection(d2[[1]], d2[[2]])))
  # make correct matrix
  y <- Matrix::sparseMatrix(
    i = c(0, 0, 1),
    j = c(0, 1, 1),
    x = c(total_length[1] - shared_length, shared_length,
          total_length[2] - shared_length),
    index1 = FALSE, symmetric = TRUE)
  # tests
  expect_is(x, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})

test_that("SpatialPolygons (real data - complex shapes)", {
  skip_if_not_installed("prioritizrdata")
  # load data
  data(tas_pu, package = "prioritizrdata")
  d <- tas_pu[c(2, 4), ]
  x <- boundary_matrix(d)
  # calculate total length
  d2 <- sf::st_geometry(sf::st_cast(sf::st_as_sf(d), "MULTILINESTRING"))
  total_length <- as.numeric(sf::st_length(d2))
  shared_length <- as.numeric(sf::st_length(
    sf::st_intersection(d2[[1]], d2[[2]])))
  # make correct matrix
  y <- Matrix::sparseMatrix(
    i = c(0, 0, 1),
    j = c(0, 1, 1),
    x = c(total_length[1] - shared_length, shared_length,
          total_length[2] - shared_length),
    index1 = FALSE, symmetric = TRUE)
  # tests
  expect_is(x, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})

test_that("SpatialPolygons (vertices not aligned)", {
  # data
  d <- raster::spPolygons(
    matrix(c(0,  0, 3,  3,  0, -1, 1, 1, -1, -1), ncol = 2),
    matrix(c(0, 0, 3, 3, 0, 1, 3, 3, 1, 1), ncol = 2),
    matrix(c(3, 3, 5, 5, 3, 0, 3, 3, 0, 0), ncol = 2))
  # make boundary matrix
  x <- boundary_matrix(d)
  # make correct matrix
  d2 <- sf::st_geometry(sf::st_cast(sf::st_as_sf(d), "MULTILINESTRING"))
  y <- matrix(nrow = 3, ncol = 3)
  for (i in seq_len(3)) {
    for (j in seq_len(3)) {
      if (i != j)
        y[i, j] <-
          as.numeric(sf::st_length(sf::st_intersection(d2[[i]], d2[[j]])))
    }
  }
  total_length <- as.numeric(sf::st_length(d2))
  diag(y) <- total_length - rowSums(y, na.rm = TRUE)
  y <- Matrix::Matrix(y, sparse = TRUE)
  # tests
  expect_is(x, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})

test_that("sf (squares)", {
  # data
  d <- raster::rasterToPolygons(raster::raster(
    matrix(0:8, byrow = TRUE, ncol = 3),
    xmn = 0, xmx = 3, ymn = 0, ymx = 3), n = 4)
  # create matrices
  x <- boundary_matrix(d)
  y <- boundary_matrix(sf::st_as_sf(d))
  # tests
  expect_is(x, "dsCMatrix")
  expect_is(y, "dsCMatrix")
  expect_true(all(x == y))
})

test_that("sf (hexagons)", {
  # data
  set.seed(401)
  d <- sp::spsample(as(raster::extent(c(0, 5, 0, 5)), "SpatialPolygons"),
                    type = "hexagonal", cellsize = 1 * sqrt(3))
  d <- sp::HexPoints2SpatialPolygons(d)
  # create matrices
  x <- boundary_matrix(d)
  y <- boundary_matrix(sf::st_as_sf(d))
  # tests
  expect_is(x, "dsCMatrix")
  expect_is(y, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})

test_that("sf (real data - simple shapes)", {
  skip_if_not_installed("prioritizrdata")
  # load data
  data(tas_pu, package = "prioritizrdata")
  d <- sf::st_as_sf(tas_pu[c(300, 279), ])
  # create matrices
  x <- boundary_matrix(d)
  y <- boundary_matrix(sf::as_Spatial(d))
  # tests
  expect_is(x, "dsCMatrix")
  expect_is(y, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})

test_that("sf (real data - complex shapes)", {
  skip_if_not_installed("prioritizrdata")
  # load data
  data(tas_pu, package = "prioritizrdata")
  d <- tas_pu[c(2, 4), ]
  # create matrices
  x <- boundary_matrix(d)
  y <- boundary_matrix(sf::st_as_sf(d))
  # tests
  expect_is(x, "dsCMatrix")
  expect_is(y, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})

test_that("sf (vertices not aligned)", {
  # data
  d <- raster::spPolygons(
    matrix(c(0,  0, 3,  3,  0, -1, 1, 1, -1, -1), ncol = 2),
    matrix(c(0, 0, 3, 3, 0, 1, 3, 3, 1, 1), ncol = 2),
    matrix(c(3, 3, 5, 5, 3, 0, 3, 3, 0, 0), ncol = 2))

  # create matrices
  x <- boundary_matrix(d)
  y  <- boundary_matrix(sf::st_as_sf(d))
  # tests
  expect_is(x, "dsCMatrix")
  expect_is(y, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})


test_that("RasterLayer", {
  # data
  d <- raster::raster(matrix(c(NA, 2:9), ncol = 3),
               xmn = 0, ymn = 0, xmx = 6, ymx = 3)
  x <- boundary_matrix(d)
  y <- boundary_matrix(raster::rasterToPolygons(d, n = 4))
  y <- cbind(0, y)
  y <- rbind(0, y)
  # tests
  expect_is(x, "dsCMatrix")
  expect_is(y, "dgCMatrix")
  expect_true(all(x == y))
})

test_that("RasterStack", {
  # data
  d <- raster::raster(matrix(c(NA, 2:9), ncol = 3),
                      xmn = 0, ymn = 0, xmx = 6, ymx = 3)
  d <- stack(d, d, d)
  d[[1]][2] <- NA
  x <- boundary_matrix(d)
  y <- boundary_matrix(raster::rasterToPolygons(d[[2]], n = 4))
  y <- cbind(0, y)
  y <- rbind(0, y)
  # tests
  expect_is(x, "dsCMatrix")
  expect_is(y, "dgCMatrix")
  expect_true(all(x == y))
})

test_that("SpatialLines", {
  # data
  d <- sp::SpatialLines(list(
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
  expect_error(boundary_matrix(d))
})

test_that("SpatialPoints", {
  # data
  d <- sp::SpatialPoints(coords = matrix(runif(10), ncol = 2))
  # tests
  expect_error(boundary_matrix(d))
})

test_that("invalid inputs", {
  expect_error(boundary_matrix(iris), "spatial format")
  expect_error(
    boundary_matrix(
      sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_point(c(1, 2)))
      )
    ),
    "no boundaries"
  )
  expect_error(
    boundary_matrix(
      sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_linestring(matrix(1:15, , 3)))
      )
    ),
    "no boundaries"
  )
  expect_error(
    boundary_matrix(
      sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_geometrycollection())
      )
    ),
    "geometry collection"
  )
})
