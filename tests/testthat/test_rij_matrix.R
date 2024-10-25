test_that("x = SpatRaster (single layer), y = SpatRaster (single layer)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create matrix
  x1 <- rij_matrix(sim_pu_raster, sim_features[[1]], memory = FALSE)
  x2 <- rij_matrix(sim_pu_raster, sim_features[[1]], memory = TRUE)
  x3 <- rij_matrix(sim_pu_raster, sim_features[[1]], memory = NA)
  # calculate correct matrix
  included <- terra::cells(is.na(sim_pu_raster), 0)[[1]]
  y <- as.matrix(sim_features[[1]][included], ncol = 1)
  y <- Matrix::t(as_Matrix(y, "dgCMatrix"))
  # run tests
  expect_true(inherits(x1, "dgCMatrix"))
  expect_equal(rownames(x1), names(sim_features[[1]]))
  expect_equal(x1, y)
  expect_equal(x1, x2)
  expect_equal(x1, x3)
})

test_that("x = SpatRaster (single layer), y = SpatRaster (multiple layers)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create matrix
  x1 <- rij_matrix(sim_pu_raster, sim_features, memory = FALSE)
  x2 <- rij_matrix(sim_pu_raster, sim_features, memory = TRUE)
  x3 <- rij_matrix(sim_pu_raster, sim_features, memory = NA)
  # calculate correct matrix
  included <- terra::cells(is.na(sim_pu_raster), 0)[[1]]
  y <- as.matrix(sim_features[included])
  y[is.na(y)] <- 0
  y <- Matrix::t(as_Matrix(y, "dgCMatrix"))
  # run tests
  expect_true(inherits(x1, "dgCMatrix"))
  expect_equal(rownames(x1), names(sim_features))
  expect_equal(x1, y)
  expect_equal(x1, x2)
  expect_equal(x3, x2)
})

test_that(
  "x = SpatRaster (multiple layers), y = SpatRaster (multiple layers)", {
  # create data
  costs <- c(
    terra::rast(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7)),
    terra::rast(matrix(c(10, 10, 10, 10,  4,   1, NA), ncol = 7))
  )
  spp <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7))
  )
  names(spp) <- make.unique(names(spp))
  # calculate matrix
  x1 <- rij_matrix(costs, spp, memory = FALSE)
  x2 <- rij_matrix(costs, spp, memory = TRUE)
  x3 <- rij_matrix(costs, spp, memory = NA)
  # calculate correct matrix
  included <- terra::cells(min(is.na(costs)), 0)[[1]]
  y <- as.matrix(spp[included])
  y[is.na(y)] <- 0
  y <- Matrix::t(as_Matrix(y, "dgCMatrix"))
  # run tests
  expect_true(inherits(x1, "dgCMatrix"))
  expect_equal(rownames(x1), names(spp))
  expect_equal(x1, y)
  expect_equal(x1, x2)
  expect_equal(x1, x3)
})

test_that("x = sf (polygons), y = SpatRaster (multiple layers)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # calculate matrix
  x1 <- rij_matrix(sim_pu_polygons, sim_features, fun = "mean", memory = FALSE)
  x2 <- rij_matrix(sim_pu_polygons, sim_features, fun = "mean", memory = TRUE)
  x3 <- rij_matrix(sim_pu_polygons, sim_features, fun = "mean", memory = NA)
  # calculate correct result
  y <- terra::extract(
    sim_features, sim_pu_polygons, fun = mean, ID = FALSE, na.rm = TRUE
  )
  y <- Matrix::t(as_Matrix(as.matrix(y), "dgCMatrix"))
  # run tests
  expect_true(inherits(x1, "dgCMatrix"))
  expect_equal(rownames(x1), names(sim_features))
  expect_equal(x1, y, tolerance = 1e-6)
  expect_equal(x1, x2)
  expect_equal(x1, x3)
})

test_that("x = sf (polygons), y = SpatRaster (indices)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # calculate matrix
  idx <- c(1, 3, 4, 5)
  x1 <- rij_matrix(
    sim_pu_polygons, sim_features, fun = "mean", memory = FALSE, idx = idx
  )
  x2 <- rij_matrix(
    sim_pu_polygons, sim_features, fun = "mean", memory = TRUE, idx = idx
  )
  x3 <- rij_matrix(
    sim_pu_polygons, sim_features, fun = "mean", memory = NA, , idx = idx
  )
  # calculate correct result
  y <- terra::extract(
    sim_features, sim_pu_polygons[idx, , drop = FALSE],
    fun = mean, ID = FALSE, na.rm = TRUE
  )
  y <- Matrix::t(as_Matrix(as.matrix(y), "dgCMatrix"))
  # run tests
  expect_true(inherits(x1, "dgCMatrix"))
  expect_equal(rownames(x1), names(sim_features))
  expect_equal(x1, y, tolerance = 1e-6)
  expect_equal(x1, x2)
  expect_equal(x1, x3)
})

test_that("x = sf (lines), y = SpatRaster (multiple layers)", {
  # import data
  sim_pu_lines <- get_sim_pu_lines()
  sim_features <- get_sim_features()
  # calculate matrix
  x1 <- rij_matrix(sim_pu_lines, sim_features, fun = "mean", memory = FALSE)
  x2 <- rij_matrix(sim_pu_lines, sim_features, fun = "mean", memory = TRUE)
  x3 <- rij_matrix(sim_pu_lines, sim_features, fun = "mean", memory = NA)
  # calculate correct results
  y <- terra::extract(
    sim_features, terra::vect(sim_pu_lines), fun = mean, ID = FALSE,
    na.rm = TRUE
  )
  y <- Matrix::t(as_Matrix(as.matrix(y), "dgCMatrix"))
  # run tests
  expect_true(inherits(x1, "dgCMatrix"))
  expect_equal(rownames(x1), names(sim_features))
  expect_equal(x1, y, tolerance = 1e-6)
  expect_equal(x1, x2)
  expect_equal(x1, x3)
})

test_that("x = sf (points), y = SpatRaster (multiple layers)", {
  # import data
  sim_pu_points <- get_sim_pu_points()
  sim_features <- get_sim_features()
  # calculate matrix
  x1 <- rij_matrix(sim_pu_points, sim_features, memory = FALSE)
  x2 <- rij_matrix(sim_pu_points, sim_features, memory = TRUE)
  x3 <- rij_matrix(sim_pu_points, sim_features, memory = NA)
  # calculate correct results
  y <- terra::extract(sim_features, sf::st_coordinates(sim_pu_points))
  y <- Matrix::t(as_Matrix(as.matrix(y), "dgCMatrix"))
  # run tests
  expect_true(inherits(x1, "dgCMatrix"))
  expect_equal(rownames(x1), names(sim_features))
  expect_equal(x1, y, tolerance = 1e-6)
  expect_equal(x1, x2)
  expect_equal(x1, x3)
})

test_that("x = sf, y = SpatRaster (multiple layers, fun = mean)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # calculate matrix
  x1 <- rij_matrix(sim_pu_polygons, sim_features, fun = "mean", memory = FALSE)
  x2 <- rij_matrix(sim_pu_polygons, sim_features, fun = "mean", memory = TRUE)
  x3 <- rij_matrix(sim_pu_polygons, sim_features, fun = "mean", memory = NA)
  # calculate correct result
  suppressWarnings({
    y <- exactextractr::exact_extract(
      sim_features, sim_pu_polygons, fun = "mean", progress = FALSE
    )
  })
  y <- Matrix::t(as_Matrix(as.matrix(y), "dgCMatrix"))
  rownames(y) <- names(sim_features)
  # run tests
  expect_true(inherits(x1, "dgCMatrix"))
  expect_equal(rownames(x1), names(sim_features))
  expect_equal(x1, y, tolerance = 1e-6)
  expect_equal(x1, x2)
  expect_equal(x1, x3)
})

test_that("x = sf, y = SpatRaster (multiple layers, fun = sum)", {
  # create data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # calculate matrix
  x1 <- rij_matrix(sim_pu_polygons, sim_features, memory = FALSE)
  x2 <- rij_matrix(sim_pu_polygons, sim_features, memory = TRUE)
  x3 <- rij_matrix(sim_pu_polygons, sim_features, memory = NA)
  # calculate correct result
  suppressWarnings({
    y <- exactextractr::exact_extract(
      sim_features, sim_pu_polygons, fun = "sum", progress = FALSE
    )
  })
  y <- Matrix::t(as_Matrix(as.matrix(y), "dgCMatrix"))
  rownames(y) <- names(sim_features)
  # run tests
  expect_true(inherits(x1, "dgCMatrix"))
  expect_equal(rownames(x1), names(sim_features))
  expect_equal(x1, y, tolerance = 1e-6)
  expect_equal(x1, x2)
  expect_equal(x1, x3)
})

test_that("x = sf (complex polygons), y = SpatRaster (fun = mean)", {
  skip_on_cran()
  skip_if_not_installed("prioritizrdata", minimum_version = "0.3.0")
  # import data
  tas_pu <- prioritizrdata::get_tas_pu()
  tas_features <- prioritizrdata::get_tas_features()
  # subset data
  tas_features <- tas_features[[c(4, 6, 10)]]
  # run calculations
  x1 <- rij_matrix(tas_pu, tas_features, fun = "mean", memory = FALSE)
  x2 <- rij_matrix(tas_pu, tas_features, fun = "mean", memory = TRUE)
  x3 <- rij_matrix(tas_pu, tas_features, fun = "mean", memory = NA)
  # calculate correct result
  suppressWarnings({
    y <- exactextractr::exact_extract(
      tas_features, tas_pu, fun = "mean", progress = FALSE
    )
  })
  y <- Matrix::t(as_Matrix(as.matrix(y), "dgCMatrix"))
  y[!is.finite(y)] <- 0
  rownames(y) <- names(tas_features)
  # run tests
  # note that exactextractr uses floats and not doubles, so it has reduced
  # precision than our summary Rcpp and R summary functions which uses doubles
  expect_true(inherits(x1, "dgCMatrix"))
  expect_equal(rownames(x1), names(tas_features))
  expect_equal(x1, y, tolerance = 1e-6)
  expect_equal(x1, x2)
  expect_equal(x1, x3)
})

test_that("x = sf (complex polygons), y = SpatRaster (fun = sum)", {
  skip_on_cran()
  skip_if_not_installed("prioritizrdata", minimum_version = "0.3.0")
  # import data
  tas_pu <- prioritizrdata::get_tas_pu()
  tas_features <- prioritizrdata::get_tas_features()
  # subset data
  tas_features <- tas_features[[c(4, 6, 10)]]
  # run calculations
  x1 <- rij_matrix(tas_pu, tas_features, fun = "sum", memory = FALSE)
  x2 <- rij_matrix(tas_pu, tas_features, fun = "sum", memory = TRUE)
  x3 <- rij_matrix(tas_pu, tas_features, fun = "sum", memory = NA)
  # calculate correct result using exact exactextractr
  suppressWarnings({
    y <- exactextractr::exact_extract(
      tas_features, tas_pu, fun = "sum", progress = FALSE
    )
  })
  y <- Matrix::t(as_Matrix(as.matrix(y), "dgCMatrix"))
  y[!is.finite(y)] <- 0
  rownames(y) <- names(tas_features)
  # calculate correct result using R
  suppressWarnings({
    z <- exactextractr::exact_extract(
      tas_features, tas_pu, fun = NULL, progress = FALSE
    )
  })
  z <- sapply(z, function(x) {
    v <- x[, seq_len(ncol(x) - 1), drop = FALSE]
    p <- x[, ncol(x), drop = TRUE]
    colSums(sweep(v, 1, p, "*"), na.rm = TRUE)
  })
  z <- as_Matrix(as.matrix(z), "dgCMatrix")
  z[!is.finite(y)] <- 0
  rownames(z) <- names(tas_features)
  # run tests
  # note that exactextractr uses floats and not doubles, so it has reduced
  # precision than our summary Rcpp and R summary functions which uses doubles
  expect_true(inherits(x1, "dgCMatrix"))
  expect_equal(rownames(x1), names(tas_features))
  expect_equal(x1, y, tolerance = 1e-6)
  expect_equal(x1, z, tolerance = 1e-6)
  expect_equal(x1, x2)
  expect_equal(x1, x3)
})

test_that("x = RasterLayer, y = RasterStack", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create matrix
  x <- rij_matrix(sim_pu_raster, sim_features)
  # calculate correct matrix
  expect_warning(
    y <- rij_matrix(
      raster::raster(sim_pu_raster),
      raster::stack(sim_features)
    ),
    "deprecated"
  )
  # run tests
  expect_true(inherits(x, "dgCMatrix"))
  expect_equal(rownames(x), names(sim_features))
  expect_equal(x, y)
})

test_that("x = Spatial, y = RasterStack", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create matrix
  x <- rij_matrix(sim_pu_polygons, sim_features)
  # calculate correct matrix
  expect_warning(
    expect_warning(
      y <- rij_matrix(
        sf::as_Spatial(sim_pu_polygons),
        raster::stack(sim_features)
      ),
      "deprecated"
    ),
    "deprecated"
  )
  # run tests
  expect_true(inherits(x, "dgCMatrix"))
  expect_equal(rownames(x), names(sim_features))
  expect_equal(x, y)
})
