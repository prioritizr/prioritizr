context("rij_matrix")

test_that("x=RasterLayer, y=RasterLayer", {
  # create data
  data(sim_pu_raster, sim_features)
  m <- rij_matrix(sim_pu_raster, sim_features[[1]])
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_equal(m, {
    included <- raster::Which(!is.na(sim_pu_raster), cells = TRUE)
    m <- as(matrix(sim_features[[1]][included], nrow = 1), "dgCMatrix")
    m
  })
})

test_that("x=RasterLayer, y=RasterStack", {
  # create data
  data(sim_pu_raster, sim_features)
  m <- rij_matrix(sim_pu_raster, sim_features)
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_equal(m, {
    included <- raster::Which(!is.na(sim_pu_raster), cells = TRUE)
    m <- sim_features[included]
    m[is.na(m)] <- 0
    m <- Matrix::t(as(m, "dgCMatrix"))
    m
  })
})

test_that("x=RasterLayer, y=RasterStack (data size > raster maxmemory)", {
  # create data
  raster::rasterOptions(todisk = TRUE)
  data(sim_pu_raster, sim_features)
  m <- rij_matrix(sim_pu_raster, sim_features)
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_false(raster::canProcessInMemory(sim_features))
  expect_equal(m, {
    included <- raster::Which(!is.na(sim_pu_raster), cells = TRUE)
    m <- sim_features[included]
    m[is.na(m)] <- 0
    m <- Matrix::t(as(m, "dgCMatrix"))
    m
  })
  raster::rasterOptions(default = TRUE)
})


test_that("x=RasterStack, y=RasterStack", {
  # create data
  costs <- raster::stack(
    raster::raster(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7)),
    raster::raster(matrix(c(10, 10, 10, 10,  4,   1, NA), ncol = 7)))
  spp <- raster::stack(
    raster::raster(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    raster::raster(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    raster::raster(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    raster::raster(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7)))
  m <- rij_matrix(costs, spp)
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_equal(m, {
    included <- raster::Which(max(!is.na(costs)) > 0, cells = TRUE)
    m <- spp[included]
    m[is.na(m)] <- 0
    m <- Matrix::t(as(m, "dgCMatrix"))
    m
  })
})

test_that("x=SpatialPolygons, y=RasterStack", {
  # create data
  data(sim_pu_polygons, sim_features)
  m <- rij_matrix(sim_pu_polygons, sim_features, fun = "mean")
  # calculate correct result
  m2 <- raster::extract(sim_features, sim_pu_polygons, fun = mean,
                        na.rm = TRUE, sp = FALSE)
  m2 <- as(m2, "dgCMatrix")
  m2 <- Matrix::t(m2)
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_lte(max(abs(m - m2)), 1e-6) # note that fast extract does area
                                     # weighted mean unlike raster
})

test_that("x=SpatialLines, y=RasterStack", {
  # create data
  data(sim_pu_lines, sim_features)
  m <- rij_matrix(sim_pu_lines, sim_features, fun = "mean")
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_equal(m, {
    m <- raster::extract(sim_features, sim_pu_lines, fun = mean, na.rm = TRUE,
                         sp = FALSE)
    m <- as(m, "dgCMatrix")
    Matrix::t(m)
  })
})

test_that("x=SpatialPoints, y=RasterStack", {
  # create data
  data(sim_pu_points, sim_features)
  m <- rij_matrix(sim_pu_points, sim_features)
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_equal(m, {
    m <- raster::extract(sim_features, sim_pu_points, na.rm = TRUE, sp = FALSE)
    m <- as(m, "dgCMatrix")
    Matrix::t(m)
  })
})

test_that("x=sf, y=RasterStack (mean)", {
  # create data
  data(sim_pu_sf, sim_features)
  m <- rij_matrix(sim_pu_sf, sim_features, fun = "mean")
  # calculate correct result
  suppressWarnings({
    m2 <- exactextractr::exact_extract(sim_features, sim_pu_sf, fun = "mean",
                                       progress = FALSE)
  })
  m2 <- as(as.matrix(m2), "dgCMatrix")
  m2 <- Matrix::t(m2)
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_lte(max(abs(m - m2)), 1e-6)
})

test_that("x=sf, y=RasterStack (sum)", {
  # create data
  data(sim_pu_sf, sim_features)
  m <- rij_matrix(sim_pu_sf, sim_features, fun = "sum")
  # calculate correct result
  suppressWarnings({
    m2 <- exactextractr::exact_extract(sim_features, sim_pu_sf, fun = "sum",
                                       progress = FALSE)
  })
  m2 <- as(as.matrix(m2), "dgCMatrix")
  m2 <- Matrix::t(m2)
  # run tests
  expect_true(inherits(m, "dgCMatrix"))
  expect_lte(max(abs(m - m2)), 1e-6)
})

test_that("x=sf, y=RasterStack (complex example, mean)", {
  skip_on_cran()
  skip_if_not_installed("prioritizrdata")
  # load data
  data(tas_pu, package = "prioritizrdata")
  data(tas_features, package = "prioritizrdata")
  tas_features <- tas_features[[c(1, 21, 52)]]
  tas_pu <- sf::st_as_sf(tas_pu)
  # run calculations
  m <- rij_matrix(tas_pu, tas_features, fun = "mean")
  # calculate correct result
  suppressWarnings({
    m2 <- exactextractr::exact_extract(tas_features, tas_pu, fun = "mean",
                                       progress = FALSE)
  })
  m2 <- as(as.matrix(m2), "dgCMatrix")
  m2 <- Matrix::t(m2)
  # calculate correct result using R
  suppressWarnings({
    m3 <- exactextractr::exact_extract(tas_features, tas_pu, fun = NULL,
                                       progress = FALSE)
  })
  m3 <- sapply(m3, function(x) {
    v <- x[, seq_len(ncol(x) - 1), drop = FALSE]
    p <- x[, ncol(x), drop = TRUE]
    colSums(sweep(v, 1, p, "*")) / sum(p)
  })
  m3 <- as(as.matrix(m3), "dgCMatrix")
  # run tests
  # note that exactextractr uses floats and not doubles, so it has reduced
  # precision than our summary Rcpp and R summary functions which uses doubles
  expect_true(inherits(m, "dgCMatrix"))
  expect_lte(max(abs(m - m2)), 1e-6)
  expect_lte(max(abs(m - m3)), 1e-10)
})

test_that("x=sf, y=RasterStack (complex example, sum)", {
  skip_on_cran()
  skip_if_not_installed("prioritizrdata")
  # load data
  data(tas_pu, package = "prioritizrdata")
  data(tas_features, package = "prioritizrdata")
  tas_features <- tas_features[[c(1, 21, 52)]]
  tas_pu <- sf::st_as_sf(tas_pu)
  # run calculations
  m <- rij_matrix(tas_pu, tas_features, fun = "sum")
  # calculate correct result using exact exactextractr
  suppressWarnings({
    m2 <- exactextractr::exact_extract(tas_features, tas_pu, fun = "sum",
                                       progress = FALSE)
  })
  m2 <- as(as.matrix(m2), "dgCMatrix")
  m2 <- Matrix::t(m2)
  # calculate correct result using R
  suppressWarnings({
    m3 <- exactextractr::exact_extract(tas_features, tas_pu, fun = NULL,
                                       progress = FALSE)
  })
  m3 <- sapply(m3, function(x) {
    v <- x[, seq_len(ncol(x) - 1), drop = FALSE]
    p <- x[, ncol(x), drop = TRUE]
    colSums(sweep(v, 1, p, "*"))
  })
  m3 <- as(as.matrix(m3), "dgCMatrix")
  # run tests
  # note that exactextractr uses floats and not doubles, so it has reduced
  # precision than our summary Rcpp and R summary functions which uses doubles
  expect_true(inherits(m, "dgCMatrix"))
  expect_lte(max(abs(m - m2)), 1e-6)
  expect_lte(max(abs(m - m3)), 1e-10)
})
