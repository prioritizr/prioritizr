context("eval_boundary_summary")

test_that("single zone (edge_factor = 1, zone matrix = 1)", {
  set.seed(500)
  # create zones data
  zm <- matrix(1, ncol = 1, nrow = 1)
  ef <- 1
  # create problem data
  pu <- sf::st_as_sf(
    tibble::tibble(
      id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
      spp1 = runif(10), spp2 = c(rpois(9, 4), NA),
      solution = c(0, NA, 1, 1, 1, 0, 0, 0, 1, 0)
    ),
    geometry =
      terra::rast(
        matrix(seq_len(10), ncol = 2, byrow = TRUE),
        extent = terra::ext(0, 2, 0, 5)
      ) %>%
      terra::as.polygons() %>%
      sf::st_as_sf() %>%
      {.[order(.[[1]]), ]} %>%
      sf::st_geometry()
  )
  # create boundary matrix
  bm <- boundary_matrix(pu)
  # create problem
  p <- problem(pu, features = c("spp1", "spp2"), cost_column = "cost")
  # calculate boundary using manually specified boundary matrix
  r1 <- eval_boundary_summary(p, pu[, "solution"], ef, zm, bm)
  # calculate boundary using automatically generated boundary matrix
  r2 <- eval_boundary_summary(p, pu[, "solution"], ef, zm)
  # correct boundary result based on matrix`
  r3 <- tibble::tibble(
    summary = "overall",
    boundary = r_boundary_given_matrix(pu$solution, ef, zm, bm)
  )
  # correct boundary result based on spatial data
  r4 <- tibble::tibble(
    summary = "overall",
    boundary = r_boundary_given_geometry(pu$solution, pu) * zm[1]
  )
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("single zone (diagonal polygons)", {
  # create data
  pu <- get_sim_pu_polygons()
  pu <- pu[c(1, 2, 11, 12, 20, 21), ]
  pu$spp1 <- 1
  pu$cost <- 1
  pu$solution_1 <- c(0, 1, 1, 0, 1, 0)
  # create boundary matrix
  bm <- boundary_matrix(pu)
  # create problem
  p <- problem(pu, features = "spp1", cost_column = "cost")
  # calculate boundary using manually specified boundary matrix
  x <- eval_boundary_summary(
    p, pu[, "solution_1"], edge_factor = 1, data = bm
  )
  # calculate boundary correctly
  y <- terra::perim(terra::vect(sf::st_union(pu[pu$solution_1 > 0.5, ])))
  # tests
  expect_equal(x$boundary, y, tolerance = 1e-8)
})

test_that("single zone (overlapping polygons)", {
  # create data
  pu <- get_sim_pu_polygons()
  pu <- pu[c(1, 2, 11, 12, 2), ]
  pu$spp1 <- 1
  pu$cost <- 1
  pu$solution_1 <- c(1, 0, 1, 1, 1)
  # create boundary matrix
  bm <- boundary_matrix(pu)
  # create problem
  p <- problem(pu, features = "spp1", cost_column = "cost")
  # calculate boundary using manually specified boundary matrix
  expect_warning(
    x <- eval_boundary_summary(
      p, pu[, "solution_1"], edge_factor = 1, data = bm
    ),
    "overlapping"
  )
  # calculate boundary correctly
  y <- terra::perim(terra::vect(sf::st_union(pu[pu$solution_1 > 0.5, ])))
  # tests
  expect_equal(x$boundary, y, tolerance = 1e-8)
})

test_that("single zone (multiple overlapping polygons)", {
  # create data
  pu <- get_sim_pu_polygons()
  pu <- pu[c(1, 2, 11, 12, 20, 21), ]
  pu <- sf::st_sf(
    geom = c(
      sf::st_geometry(pu),
      sf::st_union(pu[c(1, 2, 3), ]),
      sf::st_union(pu[c(5, 6), ])
    )
  )
  pu$spp1 <- 1
  pu$cost <- 1
  pu$solution_1 <- c(0, 0, 0, 1, 0, 0, 1, 1)
  # create boundary matrix
  bm <- boundary_matrix(pu)
  # create problem
  p <- problem(pu, features = "spp1", cost_column = "cost")
  # calculate boundary using manually specified boundary matrix
  expect_warning(
    x <- eval_boundary_summary(
      p, pu[, "solution_1"], edge_factor = 1, data = bm
    ),
    "overlapping"
  )
  # calculate boundary correctly
  y <- terra::perim(terra::vect(sf::st_union(pu[pu$solution_1 > 0.5, ])))
  # tests
  expect_equal(x$boundary, y, tolerance = 1e-8)
})

test_that("single zone (variable edge_factor, zone matrix)", {
  set.seed(500)
  # create zones data
  zm <- matrix(0.4, ncol = 1, nrow = 1)
  ef <- c(0.5)
  # create problem data
  pu <- sf::st_as_sf(
    tibble::tibble(
      id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
      spp1 = runif(10), spp2 = c(rpois(9, 4), NA),
      solution = c(0, NA, 1, 1, 1, 0, 0, 0, 1, 0)
    ),
    geometry =
      terra::rast(
        matrix(seq_len(10), ncol = 2, byrow = TRUE),
        extent = terra::ext(0, 2, 0, 5)
      ) %>%
      terra::as.polygons() %>%
      sf::st_as_sf() %>%
      {.[order(.[[1]]), ]} %>%
      sf::st_geometry()
  )
  # create boundary matrix
  bm <- boundary_matrix(pu)
  # create problem
  p <- problem(pu, features = c("spp1", "spp2"), cost_column = "cost")
  # calculate boundary using manually specified boundary matrix
  r1 <- eval_boundary_summary(p, pu[, "solution"], ef, zm, bm)
  # calculate boundary using automatically generated boundary matrix
  r2 <- eval_boundary_summary(p, pu[, "solution"], ef, zm)
  # correct boundary result based on matrix
  r3 <- tibble::tibble(
    summary = "overall",
    boundary = r_boundary_given_matrix(pu$solution, ef, zm, bm)
  )
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("multiple zones (edge_factor = 1, zone matrix = 1)", {
  set.seed(500)
  # create zones data
  zm <- matrix(1, ncol = 2, nrow = 2)
  ef <- c(1, 1)
  # create problem data
  pu <- sf::st_as_sf(
    tibble::tibble(
      id = seq_len(10),
      con = runif(10),
      cost_1 = c(NA, NA, runif(8)),
      cost_2 = c(0.3, NA, runif(8)),
      spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
      spp1_2 = runif(10), spp2_2 = runif(10),
      sol_1 = c(NA, NA, rep(c(0, 1), 4)),
      sol_2 = c(1, NA, rep(c(1, 0), 4))
    ),
    geometry =
      terra::rast(
        matrix(seq_len(10), ncol = 2, byrow = TRUE),
        extent = terra::ext(0, 2, 0, 5)
      ) %>%
      terra::as.polygons() %>%
      sf::st_as_sf() %>%
      {.[order(.[[1]]), ]} %>%
      sf::st_geometry()
  )
  # create boundary matrix
  bm <- boundary_matrix(pu)
  # create problem
  p <- problem(
    pu,
    features = zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
    cost_column = c("cost_1", "cost_2")
  )
  # calculate boundary using manually specified boundary matrix
  r1 <- eval_boundary_summary(p, pu[, c("sol_1", "sol_2")], ef, zm, bm)
  # calculate boundary using automatically generated boundary matrix
  r2 <- eval_boundary_summary(p, pu[, c("sol_1", "sol_2")], ef, zm)
  # correct boundary result based on matrix
  r3 <- tibble::tibble(
    summary = c("overall", "1", "2"),
    boundary = c(
      r_boundary_given_matrix(pu[, c("sol_1", "sol_2")], ef, zm, bm),
      r_boundary_given_matrix(pu[, "sol_1"], ef[1], diag(1), bm),
      r_boundary_given_matrix(pu[, "sol_2"], ef[2], diag(1), bm)
    )
  )
  # correct boundary result based on spatial data
  r4 <- tibble::tibble(
    summary = c("overall", "1", "2"),
    boundary = c(
      r_boundary_given_geometry(pu[, c("sol_1", "sol_2")], pu),
      r_boundary_given_geometry(pu[, "sol_1"], pu),
      r_boundary_given_geometry(pu[, "sol_2"], pu)
    )
  )
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("multiple zones (edge_factor = 1, zone matrix = identity matrix)", {
  set.seed(500)
  # create zones data
  zm <- diag(2)
  ef <- c(1, 1)
  # create problem data
  pu <- sf::st_as_sf(
    tibble::tibble(
      id = seq_len(10),
      con = runif(10),
      cost_1 = c(NA, NA, runif(8)),
      cost_2 = c(0.3, NA, runif(8)),
      spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
      spp1_2 = runif(10), spp2_2 = runif(10),
      sol_1 = c(NA, NA, rep(c(0, 1), 4)),
      sol_2 = c(1, NA, rep(c(1, 0), 4))
    ),
    geometry =
      terra::rast(
        matrix(seq_len(10), ncol = 2, byrow = TRUE),
        extent = terra::ext(0, 2, 0, 5)
      ) %>%
      terra::as.polygons() %>%
      sf::st_as_sf() %>%
      {.[order(.[[1]]), ]} %>%
      sf::st_geometry()
  )
  # create boundary matrix
  bm <- boundary_matrix(pu)
  # create problem
  p <- problem(
    pu,
    features = zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
    cost_column = c("cost_1", "cost_2")
  )
  # calculate boundary using manually specified boundary matrix
  r1 <- eval_boundary_summary(p, pu[, c("sol_1", "sol_2")], ef, zm, bm)
  # calculate boundary using automatically generated boundary matrix
  r2 <- eval_boundary_summary(p, pu[, c("sol_1", "sol_2")], ef, zm)
  # correct boundary result based on matrix
  r3 <- tibble::tibble(
    summary = c("overall", "1", "2"),
    boundary = c(
      r_boundary_given_matrix(pu[, c("sol_1", "sol_2")], ef, zm, bm),
      r_boundary_given_matrix(pu[, "sol_1"], ef[1], diag(1), bm),
      r_boundary_given_matrix(pu[, "sol_2"], ef[2], diag(1), bm)
    )
  )
  # correct boundary result based on spatial data
  r4 <- tibble::tibble(
    summary = c("overall", "1", "2"),
    boundary = c(
      sum(r_boundary_given_geometry(pu[, "sol_1"], pu),
          r_boundary_given_geometry(pu[, "sol_2"], pu)),
      r_boundary_given_geometry(pu[, "sol_1"], pu),
      r_boundary_given_geometry(pu[, "sol_2"], pu)
    )
  )
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("multiple zones (variable edge_factor, zone matrix)", {
  set.seed(500)
  # create zones data
  zm <- matrix(c(0.9, 0.2, 0.2, 0.4), ncol = 2, nrow = 2)
  ef <- c(0.5, 0.2)
  # create problem data
  pu <- sf::st_as_sf(
    tibble::tibble(
      id = seq_len(10),
      con = runif(10),
      cost_1 = c(NA, NA, runif(8)),
      cost_2 = c(0.3, NA, runif(8)),
      spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
      spp1_2 = runif(10), spp2_2 = runif(10),
      sol_1 = c(NA, NA, rep(c(0, 1), 4)),
      sol_2 = c(1, NA, rep(c(1, 0), 4))
    ),
    geometry =
      terra::rast(
        matrix(seq_len(10), ncol = 2, byrow = TRUE),
        extent = terra::ext(0, 2, 0, 5)
      ) %>%
      terra::as.polygons() %>%
      sf::st_as_sf() %>%
      {.[order(.[[1]]), ]} %>%
      sf::st_geometry()
  )
  # create boundary matrix
  bm <- boundary_matrix(pu)
  # create problem
  p <- problem(
    pu,
    features = zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
    cost_column = c("cost_1", "cost_2")
  )
  # calculate boundary using manually specified boundary matrix
  r1 <- eval_boundary_summary(p, pu[, c("sol_1", "sol_2")], ef, zm, bm)
  # calculate boundary using automatically generated boundary matrix
  r2 <- eval_boundary_summary(p, pu[, c("sol_1", "sol_2")], ef, zm)
  # correct boundary result based on matrix
  r3 <- tibble::tibble(
    summary = c("overall", "1", "2"),
    boundary = c(
      r_boundary_given_matrix(pu[, c("sol_1", "sol_2")], ef, zm, bm),
      r_boundary_given_matrix(pu[, "sol_1"], ef[1], diag(1), bm),
      r_boundary_given_matrix(pu[, "sol_2"], ef[2], diag(1), bm)
    )
  )
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})
