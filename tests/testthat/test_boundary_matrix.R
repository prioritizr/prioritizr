context("boundary_matrix")

test_that("SpatialPolygons (squares)", {
  # data
  x <- raster::rasterToPolygons(raster::raster(matrix(0:8, byrow = TRUE,
                                                      ncol = 3),
                                               xmn = 0, xmx = 3, ymn = 0,
                                               ymx = 3), n = 4)
  b1 <- boundary_matrix(x)
  s <- Matrix::sparseMatrix(
    i = c(0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8) + 1,
    j = c(0, 1, 3, 1, 2, 4, 2, 5, 3, 4, 6, 5, 7, 5, 8, 6, 7, 7, 8, 8) + 1,
    x = c(2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2),
    symmetric = TRUE, giveCsparse = FALSE)
  # tests
  expect_true(all(b1 == s))
  # experimental functionality tests
  skip_on_os("mac")
  b2 <- boundary_matrix(x, TRUE)
  expect_true(all(b2 == s))
})

test_that("SpatialPolygons (hexagons)", {
  # data
  set.seed(401)
  x <- sp::spsample(as(raster::extent(c(0, 5, 0, 5)), "SpatialPolygons"),
                    type = "hexagonal", cellsize = 1 * sqrt(3))
  x <- sp::HexPoints2SpatialPolygons(x)
  b1 <- boundary_matrix(x)
  s <- Matrix::sparseMatrix(
    i = c(0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 7),
    j = c(0, 1, 3, 1, 2, 3, 4, 2, 4, 3, 4, 5, 6, 4, 6, 7, 5, 6, 6, 7, 7),
    x = c(4, 1, 1, 2, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 2, 1, 4),
    index1 = FALSE, symmetric = TRUE)
  # tests
  expect_is(b1, "dsCMatrix")
  expect_lte(max(abs(b1 - s)), 1e-8)
  # experimental functionality
  skip_on_os("mac")
  b2 <- boundary_matrix(x, TRUE)
  expect_is(b2, "dsCMatrix")
  expect_lte(max(abs(b2 - s)), 1e-8)
})

test_that("SpatialPolygons (real data - simple shapes)", {
  skip_if_not_installed("prioritizrdata")
  # load data
  data(tas_pu, package = "prioritizrdata")
  x <- tas_pu[c(300, 279), ]
  b1 <- boundary_matrix(x)
  # calculate total length
  x2 <- sf::st_geometry(sf::st_cast(sf::st_as_sf(x), "MULTILINESTRING"))
  total_length <- as.numeric(sf::st_length(x2))
  shared_length <- as.numeric(sf::st_length(sf::st_intersection(x2[[1]],
                                                                x2[[2]])))
  # make correct matrix
  s <- Matrix::sparseMatrix(
    i = c(0, 0, 1),
    j = c(0, 1, 1),
    x = c(total_length[1] - shared_length, shared_length,
          total_length[2] - shared_length),
    index1 = FALSE, symmetric = TRUE)
  # tests
  expect_is(b1, "dsCMatrix")
  expect_lte(max(abs(b1 - s)), 1e-8)
  # experimental functionality
  skip_on_os("mac")
  b2 <- boundary_matrix(x, TRUE)
  expect_is(b2, "dsCMatrix")
  expect_lte(max(abs(b2 - s)), 1e-8)
})

test_that("SpatialPolygons (real data - complex shapes)", {
  skip_if_not_installed("prioritizrdata")
  # load data
  data(tas_pu, package = "prioritizrdata")
  x <- tas_pu[c(2, 4), ]
  b1 <- boundary_matrix(x)
  # calculate total length
  x2 <- sf::st_geometry(sf::st_cast(sf::st_as_sf(x), "MULTILINESTRING"))
  total_length <- as.numeric(sf::st_length(x2))
  shared_length <- as.numeric(sf::st_length(sf::st_intersection(x2[[1]],
                                                                x2[[2]])))
  # make correct matrix
  s <- Matrix::sparseMatrix(
    i = c(0, 0, 1),
    j = c(0, 1, 1),
    x = c(total_length[1] - shared_length, shared_length,
          total_length[2] - shared_length),
    index1 = FALSE, symmetric = TRUE)
  # tests
  expect_is(b1, "dsCMatrix")
  expect_lte(max(abs(b1 - s)), 1e-8)
  # experimental functionality
  skip_on_os("mac")
  b2 <- boundary_matrix(x, TRUE)
  expect_is(b2, "dsCMatrix")
  expect_lte(max(abs(b2 - s)), 1e-8)
})

test_that("SpatialPolygons (vertices not aligned)", {
  # data
  x <- raster::spPolygons(
    matrix(c(0,  0, 3,  3,  0, -1, 1, 1, -1, -1), ncol = 2),
    matrix(c(0, 0, 3, 3, 0, 1, 3, 3, 1, 1), ncol = 2),
    matrix(c(3, 3, 5, 5, 3, 0, 3, 3, 0, 0), ncol = 2))
  # make boundary matrix
  b1 <- boundary_matrix(x)
  # make correct matrix
  x2 <- sf::st_geometry(sf::st_cast(sf::st_as_sf(x), "MULTILINESTRING"))
  s <- matrix(nrow = 3, ncol = 3)
  for (i in seq_len(3)) {
    for (j in seq_len(3)) {
      if (i != j)
        s[i, j] <-
          as.numeric(sf::st_length(sf::st_intersection(x2[[i]], x2[[j]])))
    }
  }
  total_length <- as.numeric(sf::st_length(x2))
  diag(s) <- total_length - rowSums(s, na.rm = TRUE)
  s <- Matrix::Matrix(s, sparse = TRUE)
  # tests
  expect_is(b1, "dsCMatrix")
  expect_lte(max(abs(b1 - s)), 1e-8)
  # experimental functionality
  # ensure that boundary calculations repeatedly give the correct answer,
  # in a previous version the STR method attempted to access a
  # non-existent element in an array causing an incorrect result
  # in approx. one in five runs
  skip_on_os("mac")
  for (i in seq_len(2000)) {
    b2 <- boundary_matrix(x, TRUE)
    expect_lte(max(abs(b2 - s)), 1e-8)
  }
})

test_that("sf (squares)", {
  # data
  x <- raster::rasterToPolygons(raster::raster(matrix(0:8, byrow = TRUE,
                                                      ncol = 3),
                                               xmn = 0, xmx = 3, ymn = 0,
                                               ymx = 3), n = 4)
  y <- sf::st_as_sf(x)
  # create matrices
  s <- boundary_matrix(x)
  b1 <- boundary_matrix(y)
  # tests
  expect_true(all(b1 == s))
  # experimental functionality
  skip_on_os("mac")
  b2 <- boundary_matrix(y, TRUE)
  expect_is(b2, "dsCMatrix")
  expect_true(all(b2 == s))
})

test_that("sf (hexagons)", {
  # data
  set.seed(401)
  x <- sp::spsample(as(raster::extent(c(0, 5, 0, 5)), "SpatialPolygons"),
                    type = "hexagonal", cellsize = 1 * sqrt(3))
  x <- sp::HexPoints2SpatialPolygons(x)
  y <- sf::st_as_sf(x)
  # create matrices
  s <- boundary_matrix(x)
  b1 <- boundary_matrix(y)
  # tests
  expect_is(b1, "dsCMatrix")
  expect_lte(max(abs(b1 - s)), 1e-8)
  # experimental functionality
  skip_on_os("mac")
  b2 <- boundary_matrix(y, TRUE)
  expect_is(b2, "dsCMatrix")
  expect_lte(max(abs(b2 - s)), 1e-8)
})

test_that("sf (real data - simple shapes)", {
  skip_if_not_installed("prioritizrdata")
  # load data
  data(tas_pu, package = "prioritizrdata")
  x <- tas_pu[c(300, 279), ]
  y <- sf::st_as_sf(x)
  # create matrices
  s <- boundary_matrix(x)
  b1 <- boundary_matrix(y)
  # tests
  expect_is(b1, "dsCMatrix")
  expect_lte(max(abs(b1 - s)), 1e-8)
  # experimental functionality
  skip_on_os("mac")
  b2 <- boundary_matrix(y, TRUE)
  expect_is(b2, "dsCMatrix")
  expect_lte(max(abs(b2 - s)), 1e-8)
})

test_that("sf (real data - complex shapes)", {
  skip_if_not_installed("prioritizrdata")
  # load data
  data(tas_pu, package = "prioritizrdata")
  x <- tas_pu[c(2, 4), ]
  y <- sf::st_as_sf(x)
  # create matrices
  s <- boundary_matrix(x)
  b1 <- boundary_matrix(y)
  # tests
  expect_is(b1, "dsCMatrix")
  expect_lte(max(abs(b1 - s)), 1e-8)
  # experimental functionality
  skip_on_os("mac")
  b2 <- boundary_matrix(y, TRUE)
  expect_is(b2, "dsCMatrix")
  expect_lte(max(abs(b2 - s)), 1e-8)
})

test_that("sf (vertices not aligned)", {
  # data
  x <- raster::spPolygons(
    matrix(c(0,  0, 3,  3,  0, -1, 1, 1, -1, -1), ncol = 2),
    matrix(c(0, 0, 3, 3, 0, 1, 3, 3, 1, 1), ncol = 2),
    matrix(c(3, 3, 5, 5, 3, 0, 3, 3, 0, 0), ncol = 2))
  y <- sf::st_as_sf(x)
  # create matrices
  s <- boundary_matrix(x)
  b1 <- boundary_matrix(y)
  # tests
  expect_is(b1, "dsCMatrix")
  expect_lte(max(abs(b1 - s)), 1e-8)
  # experimental functionality
  skip_on_os("mac")
  b2 <- boundary_matrix(y, TRUE)
  expect_is(b2, "dsCMatrix")
  expect_lte(max(abs(b2 - s)), 1e-8)
})


test_that("RasterLayer", {
  # data
  x <- raster::raster(matrix(c(NA, 2:9), ncol = 3),
               xmn = 0, ymn = 0, xmx = 6, ymx = 3)
  b <- boundary_matrix(x)
  s <- boundary_matrix(raster::rasterToPolygons(x, n = 4))
  s <- cbind(0, s)
  s <- rbind(0, s)
  # tests
  expect_true(all(b == s))
  # experimental functionality
  skip_on_os("mac")
  expect_error(boundary_matrix(x, TRUE))
})

test_that("RasterStack", {
  # data
  x <- raster::raster(matrix(c(NA, 2:9), ncol = 3),
                      xmn = 0, ymn = 0, xmx = 6, ymx = 3)
  x <- stack(x, x, x)
  x[[1]][2] <- NA
  b <- boundary_matrix(x)
  s <- boundary_matrix(raster::rasterToPolygons(x[[2]], n = 4))
  s <- cbind(0, s)
  s <- rbind(0, s)
  # tests
  expect_true(all(b == s))
  # experimental functionality
  skip_on_os("mac")
  expect_error(boundary_matrix(x, TRUE))
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
