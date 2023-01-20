context("boundary_matrix")

test_that("SpatRaster (single layer)", {
  # create data
  d <- terra::rast(
    matrix(c(NA, 2:9), ncol = 3),
    ext = terra::ext(0, 6, 0, 3)
  )
  # create matrices
  x <- boundary_matrix(d)
  # create correct matrix
  y <- boundary_matrix(sf::st_as_sf(terra::as.polygons(d)))
  y <- cbind(0, y)
  y <- rbind(0, y)
  # tests
  expect_is(x, "dsCMatrix")
  expect_true(all(x == y))
})

test_that("SpatRaster (multiple layer)", {
  # create data
  d <- terra::rast(
    matrix(c(NA, 2:9), ncol = 3),
    ext = terra::ext(0, 6, 0, 3)
  )
  d <- terra::rast(list(d, d, d))
  d[[1]][2] <- NA
  # create matrices
  x <- boundary_matrix(d)
  # create correct matrix
  y <- boundary_matrix(sf::st_as_sf(terra::as.polygons(d[[2]])))
  y <- cbind(0, y)
  y <- rbind(0, y)
  # tests
  expect_is(x, "dsCMatrix")
  expect_true(all(x == y))
})

test_that("sf (squares)", {
  # create data
  d <- sf::st_as_sf(
    terra::as.polygons(
      terra::rast(
        matrix(0:8, byrow = TRUE, ncol = 3),
        ext = terra::ext(0, 3, 0, 3)
      )
    )
  )
  # create matrices
  x <- boundary_matrix(d)
  y <- triplet_sparse_matrix(
    i = 1 + c(
      0L, 0L, 1L, 1L, 2L, 3L, 3L, 4L, 4L, 5L, 6L, 7L, 0L, 1L, 2L,
      3L, 4L, 5L, 6L, 7L, 8L, 1L, 3L, 2L, 4L, 5L, 4L, 6L, 5L, 7L, 8L,
      7L, 8L
    ),
    j = 1 + c(
      1L, 3L, 2L, 4L, 5L, 4L, 6L, 5L, 7L, 8L, 7L, 8L, 0L, 1L, 2L,
      3L, 4L, 5L, 6L, 7L, 8L, 0L, 0L, 1L, 1L, 2L, 3L, 3L, 4L, 4L, 5L,
      6L, 7L
    ),
    x = c(
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4,
      4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    )
  )
  # tests
  expect_is(x, "dsCMatrix")
  expect_true(all(x == y))
})

test_that("sf (hexagons)", {
  # create data
  d <- structure(list(geometry = structure(list(structure(list(structure(c(
    0.268723672750073,
    1.13474907653451, 2.00077448031895, 2.00077448031895, 1.13474907653451,
    0.268723672750073, 0.268723672750073, 1.95862773503177, 2.45862773503177,
    1.95862773503177, 0.958627735031769, 0.458627735031769, 0.958627735031769,
    1.95862773503177
  ), dim = c(7L, 2L), dimnames = list(NULL, c(
    "x",
    "y"
  )))), class = c("XY", "POLYGON", "sfg")), structure(list(structure(c(
    2.00077448031895,
    2.86679988410339, 3.73282528788783, 3.73282528788783, 2.86679988410339,
    2.00077448031895, 2.00077448031895, 1.95862773503177, 2.45862773503177,
    1.95862773503177, 0.958627735031769, 0.458627735031769, 0.958627735031769,
    1.95862773503177
  ), dim = c(7L, 2L), dimnames = list(NULL, c(
    "x",
    "y"
  )))), class = c("XY", "POLYGON", "sfg")), structure(list(structure(c(
    3.73282528788783,
    4.59885069167227, 5.4648760954567, 5.4648760954567, 4.59885069167227,
    3.73282528788783, 3.73282528788783, 1.95862773503177, 2.45862773503177,
    1.95862773503177, 0.958627735031769, 0.458627735031769, 0.958627735031769,
    1.95862773503177
  ), dim = c(7L, 2L), dimnames = list(NULL, c(
    "x",
    "y"
  )))), class = c("XY", "POLYGON", "sfg")), structure(list(structure(c(
    1.13474907653451,
    2.00077448031895, 2.86679988410339, 2.86679988410339, 2.00077448031895,
    1.13474907653451, 1.13474907653451, 3.45862773503177, 3.95862773503177,
    3.45862773503177, 2.45862773503177, 1.95862773503177, 2.45862773503177,
    3.45862773503177
  ), dim = c(7L, 2L), dimnames = list(NULL, c(
    "x",
    "y"
  )))), class = c("XY", "POLYGON", "sfg")), structure(list(structure(c(
    2.86679988410339,
    3.73282528788783, 4.59885069167227, 4.59885069167227, 3.73282528788783,
    2.86679988410339, 2.86679988410339, 3.45862773503177, 3.95862773503177,
    3.45862773503177, 2.45862773503177, 1.95862773503177, 2.45862773503177,
    3.45862773503177
  ), dim = c(7L, 2L), dimnames = list(NULL, c(
    "x",
    "y"
  )))), class = c("XY", "POLYGON", "sfg")), structure(list(structure(c(
    0.268723672750073,
    1.13474907653451, 2.00077448031895, 2.00077448031895, 1.13474907653451,
    0.268723672750073, 0.268723672750073, 4.95862773503177, 5.45862773503177,
    4.95862773503177, 3.95862773503177, 3.45862773503177, 3.95862773503177,
    4.95862773503177
  ), dim = c(7L, 2L), dimnames = list(NULL, c(
    "x",
    "y"
  )))), class = c("XY", "POLYGON", "sfg")), structure(list(structure(c(
    2.00077448031895,
    2.86679988410339, 3.73282528788783, 3.73282528788783, 2.86679988410339,
    2.00077448031895, 2.00077448031895, 4.95862773503177, 5.45862773503177,
    4.95862773503177, 3.95862773503177, 3.45862773503177, 3.95862773503177,
    4.95862773503177
  ), dim = c(7L, 2L), dimnames = list(NULL, c(
    "x",
    "y"
  )))), class = c("XY", "POLYGON", "sfg")), structure(list(structure(c(
    3.73282528788783,
    4.59885069167227, 5.4648760954567, 5.4648760954567, 4.59885069167227,
    3.73282528788783, 3.73282528788783, 4.95862773503177, 5.45862773503177,
    4.95862773503177, 3.95862773503177, 3.45862773503177, 3.95862773503177,
    4.95862773503177
  ), dim = c(7L, 2L), dimnames = list(NULL, c(
    "x",
    "y"
  )))), class = c("XY", "POLYGON", "sfg"))), class = c(
    "sfc_POLYGON",
    "sfc"
  ), precision = 0, bbox = structure(c(
    xmin = 0.268723672750073,
    ymin = 0.458627735031769, xmax = 5.4648760954567, ymax = 5.45862773503177
  ), class = "bbox"), crs = structure(list(
    input = NA_character_,
    wkt = NA_character_
  ), class = "crs"), n_empty = 0L)), row.names = c(
    NA,
    8L
  ), class = c("sf", "data.frame"), sf_column = "geometry", agr = structure(integer(0), class = "factor", levels = c(
    "constant",
    "aggregate", "identity"
  ), names = character(0)))
  d$id <- seq_len(nrow(d))
  # create matrices
  x <- boundary_matrix(d)
  y <- Matrix::sparseMatrix(
    i = c(
      0, 1, 3, 0, 1, 2, 3, 4, 1, 2, 4, 0, 1, 3, 4, 5, 6, 1, 2, 3,
      4, 6, 7, 3, 5, 6, 3, 4, 5, 6, 7, 4, 6, 7
    ),
    j = c(
      0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4,
      4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7
    ),
    x = c(
      6, 1, 1, 1, 6, 1, 1, 1, 1, 6,
      1, 1, 1, 6, 1, 1, 1, 1, 1, 1, 6,
      1, 1, 1, 6, 1, 1, 1, 1, 6, 1, 1, 1, 6
    ),
    index1 = FALSE
  )
  # tests
  expect_is(x, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})

test_that("sf (lines)", {
  # create data
  d <- get_sim_pu_lines()
  # tests
  expect_error(boundary_matrix(d), "no boundaries")
})

test_that("sf (points)", {
  # data
  d <- get_sim_pu_points()
  # tests
  expect_error(boundary_matrix(d), "no boundaries")
})

test_that("sf (simple shapes)", {
  skip_if_not_installed("prioritizrdata", minimum_version = "3.0.0")
  # create data
  tas_pu <- prioritizrdata::get_tas_pu()
  d <- tas_pu[c(300, 279), ]
  # create matrices
  x <- boundary_matrix(d)
  # calculate total length
  d2 <- sf::st_geometry(sf::st_cast(sf::st_as_sf(d), "MULTILINESTRING"))
  total_length <- as.numeric(sf::st_length(d2))
  shared_length <- as.numeric(
    sf::st_length(sf::st_intersection(d2[[1]], d2[[2]]))
  )
  # make correct matrix
  y <- Matrix::sparseMatrix(
    i = c(0, 0, 1),
    j = c(0, 1, 1),
    x = c(
      total_length[1] - shared_length, shared_length,
      total_length[2] - shared_length
    ),
    index1 = FALSE,
    symmetric = TRUE
  )
  # tests
  expect_is(x, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})

test_that("sf (complex shapes)", {
  skip_if_not_installed("prioritizrdata", minimum_version = "3.0.0")
  # create data
  tas_pu <- prioritizrdata::get_tas_pu()
  d <- tas_pu[c(2, 4), ]
  # create matrices
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
    index1 = FALSE,
    symmetric = TRUE
  )
  # tests
  expect_is(x, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})

test_that("sf (vertices not aligned)", {
  # data
  d <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(matrix(c(0,  0, 3,  3,  0, -1, 1, 1, -1, -1), ncol = 2))
      ),
      sf::st_polygon(
        list(matrix(c(0, 0, 3, 3, 0, 1, 3, 3, 1, 1), ncol = 2))
      ),
      sf::st_polygon(
        list(matrix(c(3, 3, 5, 5, 3, 0, 3, 3, 0, 0), ncol = 2))
      )
    )
  )
  # make boundary matrices
  x <- boundary_matrix(d)
  # make correct matrix
  d2 <- sf::st_geometry(sf::st_cast(sf::st_as_sf(d), "MULTILINESTRING"))
  y <- matrix(nrow = 3, ncol = 3)
  for (i in seq_len(3)) {
    for (j in seq_len(3)) {
      if (i != j) {
        y[i, j] <- as.numeric(
          sf::st_length(sf::st_intersection(d2[[i]], d2[[j]]))
        )
      }
    }
  }
  diag(y) <- as.numeric(sf::st_length(d2))
  y <- Matrix::Matrix(y, sparse = TRUE)
  # tests
  expect_is(x, "dsCMatrix")
  expect_lte(max(abs(x - y)), 1e-8)
})

test_that("Spatial", {
  # polygons
  x <- boundary_matrix(get_sim_pu_polygons())
  expect_warning(
    y <- boundary_matrix(sf::as_Spatial(get_sim_pu_polygons())),
    "deprecated"
  )
  expect_is(x, "dsCMatrix")
  expect_is(y, "dsCMatrix")
  expect_true(all(x == y))
  # lines
  expect_error(
    suppressWarnings(
      boundary_matrix(sf::as_Spatial(get_sim_pu_lines()))
    )
  )
  # points
  expect_error(
    suppressWarnings(
      boundary_matrix(sf::as_Spatial(get_sim_pu_points()))
    )
  )
})

test_that("Raster", {
  # RasterLayer
  x <- boundary_matrix(get_sim_pu_raster())
  expect_warning(
    y <- boundary_matrix(raster::raster(get_sim_pu_raster())),
    "deprecated"
  )
  expect_is(x, "dsCMatrix")
  expect_is(y, "dsCMatrix")
  expect_true(all(x == y))
  # RasterStack
  r <- c(get_sim_pu_raster(), get_sim_pu_raster())
  r[[1]][2] <- NA
  x <- boundary_matrix(r)
  expect_warning(
    y <- boundary_matrix(raster::stack(r)),
    "deprecated"
  )
  expect_is(x, "dsCMatrix")
  expect_is(y, "dsCMatrix")
  expect_true(all(x == y))
})

test_that("invalid inputs", {
  expect_error(boundary_matrix(iris), "spatial format")
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
