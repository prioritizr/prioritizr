test_that("numeric", {
  # create data
  pu <- data.frame(
    id = seq_len(4), cost = c(10, 2, NA, 3),
    spp1 = c(1, 0, 0, 1), spp2 = c(10, 5, 10, 6)
  )
  p <-
    problem(
      pu$cost,
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    )
  s <- matrix(c(0, 1, 0), ncol = 1)
  # create formatted solution
  x <- planning_unit_solution_format(p, s)
  # create correct result
  y <- matrix(c(0, 1, NA, 0), ncol = 1, dimnames = list(NULL, zone_names(p)))
  # tests
  expect_equal(x, y)
})

test_that("matrix", {
  # create data
  pu <- data.frame(
    id = seq_len(4),
    cost_1 = c(10, 2, NA, 3),
    cost_2 = c(3, NA, NA, 1),
    spp1 = c(1, 0, 0, 1), spp2 = c(10, 5, 10, 6)
  )
  p <-
    problem(
      as.matrix(pu[, c("cost_1", "cost_2")]),
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      list(as.matrix(t(pu[, 3:4])), as.matrix(t(pu[, 3:4])))
    )
  s <- matrix(c(0, 1, 0, 1, 0, 1), ncol = 2)
  # create formatted solution
  x <- planning_unit_solution_format(p, s)
  # create correct result
  y <- matrix(
    c(0, 1, NA, 0, 1, NA, NA, 1), ncol = 2,
    dimnames = list(NULL, zone_names(p))
  )
  # tests
  expect_equal(x, y)
})

test_that("data.frame (append = FALSE)", {
  # create data
  pu <- data.frame(
    id = seq_len(7),
    cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
    cost_2 = c(10, 10, 10, 10,  4,   1, NA),
    spp1_z1 = c(1,  2, 0, 0, 0, 0,  0),
    spp2_z1 = c(NA, 0, 1, 1, 0, 0,  0),
    spp1_z2 = c(1,  0, 0, 0, 1, 0,  0),
    spp2_z2 = c(0,  0, 0, 0, 0, 10, 0)
  )
  p <-
    problem(
      pu,
      zones(z1 = c("spp1_z1", "spp2_z1"), z2 = c("spp1_z2", "spp2_z2")),
      cost_column = c("cost_1", "cost_2")
    )
  s <- as.matrix(data.frame(
    z1 = c(1, 0, 0, 1, 0, 0),
    z2 = c(0, 1, 1, 0, 1, 0)
  ))
  # create formatted solution
  x <- planning_unit_solution_format(p, s)
  # create correct result
  y <- tibble::tibble(
    z1 = c(1, 0, NA, 1, 0, 0, NA),
    z2 = c(0, 1, 1, 0, 1, 0, NA)
  )
  # tests
  expect_equal(x, y)
})

test_that("data.frame (append = TRUE)", {
  # create data
  pu <- data.frame(
    id = seq_len(7),
    cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
    cost_2 = c(10, 10, 10, 10,  4,   1, NA),
    spp1_z1 = c(1,  2, 0, 0, 0, 0,  0),
    spp2_z1 = c(NA, 0, 1, 1, 0, 0,  0),
    spp1_z2 = c(1,  0, 0, 0, 1, 0,  0),
    spp2_z2 = c(0,  0, 0, 0, 0, 10, 0)
  )
  p <-
    problem(
      pu,
      zones(z1 = c("spp1_z1", "spp2_z1"), z2 = c("spp1_z2", "spp2_z2")),
      cost_column = c("cost_1", "cost_2")
    )
  s <- as.matrix(data.frame(
    z1 = c(1, 0, 0, 1, 0, 0),
    z2 = c(0, 1, 1, 0, 1, 0)
  ))
  # create formatted solution
  x <- planning_unit_solution_format(p, s, append = TRUE)
  # create correct result
  y <- tibble::tibble(
    z1 = c(1, 0, NA, 1, 0, 0, NA),
    z2 = c(0, 1, 1, 0, 1, 0, NA)
  )
  # tests
  expect_equal(x, tibble::as_tibble(cbind(pu, y)))
})

test_that("sf (append = FALSE)", {
  # create data
  pu <- sf::st_as_sf(tibble::tibble(
    id = seq_len(7),
    cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
    cost_2 = c(10, 10, 10, 10,  4,   1, NA),
    spp1_z1 = c(1,  2, 0, 0, 0, 0,  0),
    spp2_z1 = c(NA, 0, 1, 1, 0, 0,  0),
    spp1_z2 = c(1,  0, 0, 0, 1, 0,  0),
    spp2_z2 = c(0,  0, 0, 0, 0, 10, 0),
    geometry = sf::st_geometry(get_sim_pu_polygons())[seq_len(7)]
  ))
  p <-
    problem(
      pu,
      zones(z1 = c("spp1_z1", "spp2_z1"), z2 = c("spp1_z2", "spp2_z2")),
      cost_column = c("cost_1", "cost_2")
    )
  s <- as.matrix(data.frame(
    z1 = c(1, 0, 0, 1, 0, 0),
    z2 = c(0, 1, 1, 0, 1, 0)
  ))
  # create formatted solution
  x <- planning_unit_solution_format(p, s)
  # create correct result
  y <- sf::st_as_sf(tibble::tibble(
    z1 = c(1, 0, NA, 1, 0, 0, NA),
    z2 = c(0, 1, 1, 0, 1, 0, NA),
    geometry = pu$geometry
  ))
  # tests
  expect_equal(x, y)
})

test_that("sf (append = TRUE)", {
  # create data
  pu <- sf::st_as_sf(tibble::tibble(
    id = seq_len(7),
    cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
    cost_2 = c(10, 10, 10, 10,  4,   1, NA),
    spp1_z1 = c(1,  2, 0, 0, 0, 0,  0),
    spp2_z1 = c(NA, 0, 1, 1, 0, 0,  0),
    spp1_z2 = c(1,  0, 0, 0, 1, 0,  0),
    spp2_z2 = c(0,  0, 0, 0, 0, 10, 0),
    geometry = sf::st_geometry(get_sim_pu_polygons())[seq_len(7)]
  ))
  p <-
    problem(
      pu,
      zones(z1 = c("spp1_z1", "spp2_z1"), z2 = c("spp1_z2", "spp2_z2")),
      cost_column = c("cost_1", "cost_2")
    )
  s <- as.matrix(data.frame(
    z1 = c(1, 0, 0, 1, 0, 0),
    z2 = c(0, 1, 1, 0, 1, 0)
  ))
  # create formatted solution
  x <- planning_unit_solution_format(p, s, append = TRUE)
  # create correct result
  y <- sf::st_as_sf(
    tibble::as_tibble(
      cbind(
        sf::st_drop_geometry(pu),
        data.frame(
          z1 = c(1, 0, NA, 1, 0, 0, NA),
          z2 = c(0, 1, 1, 0, 1, 0, NA),
          geometry = pu$geometry
        )
      )
    )
  )
  # tests
  expect_equal(x, y)
})

test_that("Spatial (append = FALSE)", {
  # create data
  pu <- sf::as_Spatial(sf::st_as_sf(tibble::tibble(
    id = seq_len(7),
    cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
    cost_2 = c(10, 10, 10, 10,  4,   1, NA),
    spp1_z1 = c(1,  2, 0, 0, 0, 0,  0),
    spp2_z1 = c(NA, 0, 1, 1, 0, 0,  0),
    spp1_z2 = c(1,  0, 0, 0, 1, 0,  0),
    spp2_z2 = c(0,  0, 0, 0, 0, 10, 0),
    geometry = sf::st_geometry(get_sim_pu_polygons())[seq_len(7)]
  )))
  expect_warning(
    p <-
      problem(
        pu,
        zones(z1 = c("spp1_z1", "spp2_z1"), z2 = c("spp1_z2", "spp2_z2")),
        cost_column = c("cost_1", "cost_2")
      )
    ,
    "deprecated"
  )
  s <- as.matrix(data.frame(
    z1 = c(1, 0, 0, 1, 0, 0),
    z2 = c(0, 1, 1, 0, 1, 0)
  ))
  # create formatted solution
  x <- planning_unit_solution_format(p, s)
  # create correct result
  y <- sf::as_Spatial(sf::st_as_sf(tibble::tibble(
    z1 = c(1, 0, NA, 1, 0, 0, NA),
    z2 = c(0, 1, 1, 0, 1, 0, NA),
    geometry = sf::st_geometry(get_sim_pu_polygons())[seq_len(7)]
  )))
  attr(y@data, "row.names") <- as.character(attr(y@data, "row.names"))
  # tests
  expect_equal(x, y)
})

test_that("Spatial (append = TRUE)", {
  # create data
  pu <- sf::as_Spatial(sf::st_as_sf(tibble::tibble(
    id = seq_len(7),
    cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
    cost_2 = c(10, 10, 10, 10,  4,   1, NA),
    spp1_z1 = c(1,  2, 0, 0, 0, 0,  0),
    spp2_z1 = c(NA, 0, 1, 1, 0, 0,  0),
    spp1_z2 = c(1,  0, 0, 0, 1, 0,  0),
    spp2_z2 = c(0,  0, 0, 0, 0, 10, 0),
    geometry = sf::st_geometry(get_sim_pu_polygons())[seq_len(7)]
  )))
  expect_warning(
    p <-
      problem(
        pu,
        zones(z1 = c("spp1_z1", "spp2_z1"), z2 = c("spp1_z2", "spp2_z2")),
        cost_column = c("cost_1", "cost_2")
      )
    ,
    "deprecated"
  )
  s <- as.matrix(data.frame(
    z1 = c(1, 0, 0, 1, 0, 0),
    z2 = c(0, 1, 1, 0, 1, 0)
  ))
  # create formatted solution
  x <- planning_unit_solution_format(p, s, append = TRUE)
  # create correct result
  y <- sf::as_Spatial(sf::st_as_sf(
    tibble::as_tibble(
      cbind(
        sf::st_drop_geometry(pu),
        data.frame(
          z1 = c(1, 0, NA, 1, 0, 0, NA),
          z2 = c(0, 1, 1, 0, 1, 0, NA),
          geometry = sf::st_geometry(get_sim_pu_polygons())[seq_len(7)]
        )
      )
    )
  ))
  attr(y@data, "row.names") <- as.character(attr(y@data, "row.names"))
  # tests
  expect_equal(x, y)
})

test_that("SpatRaster", {
  # create data
  pu <- c(
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
  p <- problem(pu, zones(z1 = spp[[1:2]], z2 = spp[[3:4]]))
  s <- as.matrix(data.frame(
    z1 = c(1, 0, 0, 1, 0, 0),
    z2 = c(0, 1, 1, 0, 1, 0)
  ))
  # create formatted solution
  x <- planning_unit_solution_format(p, s)
  # create correct result
  y <- setNames(
    c(
      terra::rast(matrix(c(1, 0, NA, 1, 0, 0, NA), ncol = 7)),
      terra::rast(matrix(c(0, 1, 1, 0, 1, 0, NA), ncol = 7))
    ),
    colnames(s)
  )
  # tests
  expect_equal(terra::as.data.frame(x), terra::as.data.frame(y))
})

test_that("Raster", {
  # create data
  pu <- raster::stack(
    raster::raster(matrix(c(1,  2,  NA, 3, 100, 100, NA), ncol = 7)),
    raster::raster(matrix(c(10, 10, 10, 10,  4,   1, NA), ncol = 7))
  )
  spp <- raster::stack(
    raster::raster(matrix(c(1,  2, 0, 0, 0, 0,  0), ncol = 7)),
    raster::raster(matrix(c(NA, 0, 1, 1, 0, 0,  0), ncol = 7)),
    raster::raster(matrix(c(1,  0, 0, 0, 1, 0,  0), ncol = 7)),
    raster::raster(matrix(c(0,  0, 0, 0, 0, 10, 0), ncol = 7))
  )
  names(spp) <- make.unique(names(spp))
  expect_warning(
    p <- problem(pu, suppressWarnings(zones(z1 = spp[[1:2]], z2 = spp[[3:4]]))),
    "deprecated"
  )
  s <- as.matrix(data.frame(
    z1 = c(1, 0, 0, 1, 0, 0),
    z2 = c(0, 1, 1, 0, 1, 0)
  ))
  # create formatted solution
  x <- planning_unit_solution_format(p, s)
  # create correct result
  y <- setNames(
    raster::stack(
      raster::raster(matrix(c(1, 0, NA, 1, 0, 0, NA), ncol = 7)),
      raster::raster(matrix(c(0, 1, 1, 0, 1, 0, NA), ncol = 7))
    ),
    colnames(s)
  )
  # tests
  expect_equal(raster::as.data.frame(x), raster::as.data.frame(y))
})
