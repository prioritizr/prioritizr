context("connectivity_matrix")

test_that("Spatial,character", {
  ## load data
  data(sim_pu_polygons)
  ## make matrix
  cm <- connectivity_matrix(sim_pu_polygons, "cost")
  # preliminary calculations
  bm <- boundary_matrix(sim_pu_polygons)
  bd <- matrix_to_triplet_dataframe(bm)
  bd <- bd[bd[[1]] != bd[[2]], ]
  bd$x <- bd$x * ( (sim_pu_polygons$cost[bd$i] + sim_pu_polygons$cost[bd$j]) *
                  0.5)
  bd <- bd[which(bd$x > 0), ]
  correct_cm <- Matrix::sparseMatrix(i = bd$i, j = bd$j, x = bd$x,
                                     symmetric = TRUE,
                                     dims = rep(length(sim_pu_polygons), 2))
  # tests
  expect_equal(ncol(cm), ncol(correct_cm))
  expect_equal(nrow(cm), nrow(correct_cm))
  expect_true(all(cm == correct_cm))
  ## check that invalid inputs result in errors
  expect_error({
    data(sim_pu_polygons)
    connectivity_matrix(sim_features, "column_that_doesnt_exist")
  })
  expect_error({
    data(sim_pu_polygons)
    connectivity_matrix(sim_features, NA_character_)
  })
  expect_error({
    data(sim_pu_polygons)
    sim_pu_polygons$column <- "a"
    connectivity_matrix(sim_features, column)
  })
})

test_that("Spatial,Raster", {
  ## load data
  data(sim_pu_polygons, sim_features)
  ## make matrix
  cm <- connectivity_matrix(sim_pu_polygons, sim_features[[1]])
  ## check that matrix is correct
  # preliminary calculations
  bm <- boundary_matrix(sim_pu_polygons)
  fd <- extract(sim_features[[1]], sim_pu_polygons, fun = mean)[, 1]
  bd <- matrix_to_triplet_dataframe(bm)
  bd <- bd[bd[[1]] != bd[[2]], ]
  bd$x <- bd$x * ( (fd[bd$i] + fd[bd$j]) * 0.5)
  bd <- bd[which(bd$x > 0), ]
  correct_cm <- Matrix::sparseMatrix(i = bd$i, j = bd$j, x = bd$x,
                                     symmetric = TRUE,
                                     dims = rep(length(sim_pu_polygons), 2))
  # tests
  expect_equal(ncol(cm), ncol(correct_cm))
  expect_equal(nrow(cm), nrow(correct_cm))
  expect_true(all(cm == correct_cm))
  ## check that invalid inputs result in errors
  expect_error({
    data(sim_pu_polygons, sim_features)
    connectivity_matrix(sim_pu_polygons, sim_features)
  })
  expect_error({
    data(sim_pu_polygons, sim_pu_raster)
    sim_pu_raster@crs <- sp::CRS("+init=epsg:4326")
    connectivity_matrix(sim_pu_polygons, sim_pu_raster)
  })
})

test_that("Raster,Raster", {
  ## load data
  data(sim_pu_raster, sim_features)
  ## make matrix
  cm <- connectivity_matrix(sim_pu_raster, sim_features[[1]])
  ## check that matrix is correct
  # preliminary calculations
  bm <- boundary_matrix(sim_pu_raster)
  included <- raster::Which(!is.na(sim_pu_raster[[1]]), cells = TRUE)
  bd <- matrix_to_triplet_dataframe(bm)
  bd <- bd[bd[[1]] != bd[[2]], ]
  bd$x <- bd$x * ( (sim_features[[1]][included[bd$i]] +
                    sim_features[[1]][included[bd$j]]) * 0.5)
  bd <- bd[which(bd$x > 0), ]
  correct_cm <- Matrix::sparseMatrix(i = bd$i, j = bd$j, x = bd$x,
                                     symmetric = TRUE,
                                     dims = rep(length(included), 2))
  # tests
  expect_equal(ncol(cm), ncol(correct_cm))
  expect_equal(nrow(cm), nrow(correct_cm))
  expect_true(all(cm == correct_cm))
  ## invalid inputs
  expect_error({
    data(sim_pu_raster, sim_features)
    connectivity_matrix(sim_pu_raster, sim_features)
  })
  expect_error({
    data(sim_pu_raster, sim_features)
    connectivity_matrix(sim_features, sim_pu_raster)
  })
  expect_error({
    data(sim_pu_raster, sim_features)
    extent(sim_pu_raster) <- c(0, 0.5, 0, 0.5)
    connectivity_matrix(sim_features, sim_pu_raster)
  })
  expect_error({
    data(sim_pu_raster, sim_features)
    sim_pu_raster@crs <- sp::CRS("+init=epsg:4326")
    connectivity_matrix(sim_features, sim_pu_raster)
  })
})
