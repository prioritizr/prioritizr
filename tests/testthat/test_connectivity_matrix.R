context("connectivity_matrix")

test_that("x=sf, y=character", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  # create matrix
  cm <- connectivity_matrix(sim_pu_polygons, "cost")
  # create correct matrix
  bm <- boundary_matrix(sim_pu_polygons)
  bd <- matrix_to_triplet_dataframe(bm)
  bd <- bd[bd[[1]] != bd[[2]], ]
  bd$x <-
    bd$x * c((sim_pu_polygons$cost[bd$i] + sim_pu_polygons$cost[bd$j]) * 0.5)
  bd <- bd[which(bd$x > 0), ]
  correct_cm <- Matrix::sparseMatrix(
    i = bd$i, j = bd$j, x = bd$x, symmetric = TRUE,
    dims = rep(nrow(sim_pu_polygons), 2)
  )
  # tests
  expect_equal(ncol(cm), ncol(correct_cm))
  expect_equal(nrow(cm), nrow(correct_cm))
  expect_true(all(cm == correct_cm))
  # test for invalid inputs
  expect_error({
    sim_pu_polygons <- get_sim_pu_polygons()
    connectivity_matrix(sim_pu_polygons, "column_that_doesnt_exist")
  })
  expect_error({
    sim_pu_polygons <- get_sim_pu_polygons()
    connectivity_matrix(sim_pu_polygons, NA_character_)
  })
  expect_error({
    sim_pu_polygons <- get_sim_pu_polygons()
    sim_pu_polygons$column <- "a"
    connectivity_matrix(sim_pu_polygons, "column")
  })
})

test_that("x=sf, y=Raster", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create matrix
  cm <- connectivity_matrix(sim_pu_polygons, sim_features[[1]])
  # create correct matrix
  bm <- boundary_matrix(sim_pu_polygons)
  fd <- terra::extract(
    sim_features[[1]], sim_pu_polygons, ID = FALSE, fun = mean
  )[[1]]
  bd <- matrix_to_triplet_dataframe(bm)
  bd <- bd[bd[[1]] != bd[[2]], ]
  bd$x <- bd$x * ( (fd[bd$i] + fd[bd$j]) * 0.5)
  bd <- bd[which(bd$x > 0), ]
  correct_cm <- Matrix::sparseMatrix(
    i = bd$i, j = bd$j, x = bd$x, symmetric = TRUE,
    dims = rep(nrow(sim_pu_polygons), 2)
  )
  # tests
  expect_equal(ncol(cm), ncol(correct_cm))
  expect_equal(nrow(cm), nrow(correct_cm))
  expect_lte(max(abs(cm - correct_cm)), 1e-8)
  # tests for invalid inputs
  expect_error(
    connectivity_matrix( get_sim_pu_polygons(), get_sim_features())
  )
  expect_error(
    connectivity_matrix(
      suppressWarnings(sf::st_set_crs(get_sim_pu_polygons(), 3857)),
      get_sim_pu_raster()
    )
  )
})

test_that("x=SpatRaster, y=SpatRaster", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # make matrix
  cm <- connectivity_matrix(sim_pu_raster, sim_features[[1]])
  # create correct matrix
  bm <- boundary_matrix(sim_pu_raster)
  bd <- matrix_to_triplet_dataframe(bm)
  bd <- bd[bd[[1]] != bd[[2]], ]
  bd$x <-
    bd$x *
    c( (sim_features[[1]][bd$i][[1]] + sim_features[[1]][bd$j][[1]]) * 0.5)
  bd <- bd[which(bd$x > 0), ]
  correct_cm <- Matrix::sparseMatrix(
    i = bd$i, j = bd$j, x = bd$x,
    symmetric = TRUE, dims = rep(terra::ncell(sim_pu_raster), 2)
  )
  # tests
  expect_equal(ncol(cm), ncol(correct_cm))
  expect_equal(nrow(cm), nrow(correct_cm))
  expect_true(all(cm == correct_cm))
  # tests for invalid inputs
  expect_error(
    connectivity_matrix(get_sim_pu_raster(), get_sim_features())
  )
  expect_error(
    connectivity_matrix(get_sim_features(), get_sim_pu_raster())
  )
  expect_error(
    connectivity_matrix(
      get_sim_pu_raster(),
      {r <- get_sim_pu_raster(); terra::ext(r) <- c(0, 0.5, 0, 0.5); r}
    )
  )
  expect_error(
    connectivity_matrix(
      sim_pu_raster,
      {
        r <- get_sim_pu_raster()
        terra::crs(r) <- as.character(sf::st_crs(4326))[[2]]
        r
      }
    )
  )
})

test_that("x=Spatial, y=character", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  # make matrices
  cm1 <- connectivity_matrix(sim_pu_polygons, "cost")
  expect_warning(
    cm2 <-  connectivity_matrix(sf::as_Spatial(sim_pu_polygons), "cost"),
    "deprecated"
  )
  # tests
  expect_equal(cm1, cm2)
})

test_that("x=Spatial, y=Raster", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create matrices
  cm1 <- connectivity_matrix(sim_pu_polygons, sim_features[[1]])
  expect_warning(
    cm2 <- connectivity_matrix(
      sf::as_Spatial(sim_pu_polygons), raster::stack(sim_features[[1]])
    ),
    "deprecated"
  )
  # tests
  expect_equal(cm1, cm2)
})
