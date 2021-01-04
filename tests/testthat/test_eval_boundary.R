context("eval_boundary")

test_that("single zone (edge_factor = 1, zone matrix = 1)", {
  # simulate spatial data
  set.seed(500)
  pl <- raster::raster(matrix(seq_len(10), ncol = 2),
                       xmn = 0, xmx = 2, ymn = 0, ymx = 5)
  pl <- as(pl, "SpatialPolygons")
  bm <- boundary_matrix(pl)
  zm <- diag(1)
  ef <- 1
  # simulate problem data
  pu <- sp::SpatialPolygonsDataFrame(
    pl, match.ID = FALSE,
    data.frame(id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
               spp1 = runif(10), spp2 = c(rpois(9, 4), NA),
               solution = c(0, NA, 1, 1, 1, 0, 0, 0, 1, 0)))
  # create problem
  p <- problem(pu, features = c("spp1", "spp2"), cost_column = "cost")
  # calculate boundary using manually specified boundary matrix
  r1 <- eval_boundary(p, pu[, "solution"], ef, zm, bm)
  # calculate boundary using automatically generated boundary matrix
  r2 <- eval_boundary(p, pu[, "solution"], ef, zm)
  # correct boundary result based on matrix
  r3 <- tibble::tibble(
    statistic = "overall",
    boundary = r_boundary_given_matrix(pu$solution, ef, zm, bm))
  # correct boundary result based on spatial data
  r4 <- tibble::tibble(
    statistic = "overall",
    boundary = r_boundary_given_geometry(pu$solution, pl) * zm[1])
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("single zone (variable edge_factor, zone matrix)", {
  # simulate spatial data
  set.seed(500)
  pl <- raster::raster(matrix(seq_len(10), ncol = 2),
                       xmn = 0, xmx = 2, ymn = 0, ymx = 5)
  pl <- as(pl, "SpatialPolygons")
  bm <- boundary_matrix(pl)
  zm <- matrix(0.4, ncol = 1, nrow = 1)
  ef <- c(0.5)
  # simulate problem data
  pu <- sp::SpatialPolygonsDataFrame(
    pl, match.ID = FALSE,
    data.frame(id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
               spp1 = runif(10), spp2 = c(rpois(9, 4), NA),
               solution = c(0, NA, 1, 1, 1, 0, 0, 0, 1, 0)))
  # create problem
  p <- problem(pu, features = c("spp1", "spp2"), cost_column = "cost")
  # calculate boundary using manually specified boundary matrix
  r1 <- eval_boundary(p, pu[, "solution"], ef, zm, bm)
  # calculate boundary using automatically generated boundary matrix
  r2 <- eval_boundary(p, pu[, "solution"], ef, zm)
  # correct boundary result based on matrix
  r3 <- tibble::tibble(
    statistic = "overall",
    boundary = r_boundary_given_matrix(pu$solution, ef, zm, bm))
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("multiple zones (edge_factor = 1, zone matrix = 1)", {
  # simulate spatial data
  set.seed(500)
  pl <- raster::raster(matrix(seq_len(10), ncol = 2),
                       xmn = 0, xmx = 2, ymn = 0, ymx = 5)
  pl <- as(pl, "SpatialPolygons")
  bm <- boundary_matrix(pl)
  zm <- matrix(1, ncol = 2, nrow = 2)
  ef <- c(1, 1)
  # simulate problem data
  pu <- sp::SpatialPolygonsDataFrame(
    pl, match.ID = FALSE,
    data.frame(id = seq_len(10),
               cost_1 = c(NA, NA, runif(8)),
               cost_2 = c(0.3, NA, runif(8)),
               spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
               spp1_2 = runif(10), spp2_2 = runif(10),
               sol_1 = c(NA, NA, rep(c(0, 1), 4)),
               sol_2 = c(1, NA, rep(c(1, 0), 4))))
  # create problem
  p <- problem(pu,
               features = zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
               cost_column = c("cost_1", "cost_2"))
  # calculate boundary using manually specified boundary matrix
  r1 <- eval_boundary(p, pu[, c("sol_1", "sol_2")], ef, zm, bm)
  # calculate boundary using automatically generated boundary matrix
  r2 <- eval_boundary(p, pu[, c("sol_1", "sol_2")], ef, zm)
  # correct boundary result based on matrix
  r3 <- tibble::tibble(
    statistic = c("overall", "1", "2"),
    boundary = c(
      r_boundary_given_matrix(pu[, c("sol_1", "sol_2")], ef, zm, bm),
      r_boundary_given_matrix(pu[, "sol_1"], ef[1], diag(1), bm),
      r_boundary_given_matrix(pu[, "sol_2"], ef[2], diag(1), bm)))
  # correct boundary result based on spatial data
  r4 <- tibble::tibble(
    statistic = c("overall", "1", "2"),
    boundary = c(
      r_boundary_given_geometry(pu[, c("sol_1", "sol_2")], pl),
      r_boundary_given_geometry(pu[, "sol_1"], pl),
      r_boundary_given_geometry(pu[, "sol_2"], pl)))
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("multiple zones (edge_factor = 1, zone matrix = identity matrix)", {
  # simulate spatial data
  set.seed(500)
  pl <- raster::raster(matrix(seq_len(10), ncol = 2),
                       xmn = 0, xmx = 2, ymn = 0, ymx = 5)
  pl <- as(pl, "SpatialPolygons")
  bm <- boundary_matrix(pl)
  zm <- diag(2)
  ef <- c(1, 1)
  # simulate problem data
  pu <- sp::SpatialPolygonsDataFrame(
    pl, match.ID = FALSE,
    data.frame(id = seq_len(10),
               cost_1 = c(NA, NA, runif(8)),
               cost_2 = c(0.3, NA, runif(8)),
               spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
               spp1_2 = runif(10), spp2_2 = runif(10),
               sol_1 = c(NA, NA, rep(c(0, 1), 4)),
               sol_2 = c(1, NA, rep(c(1, 0), 4))))
  # create problem
  p <- problem(pu,
               features = zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
               cost_column = c("cost_1", "cost_2"))
  # calculate boundary using manually specified boundary matrix
  r1 <- eval_boundary(p, pu[, c("sol_1", "sol_2")], ef, zm, bm)
  # calculate boundary using automatically generated boundary matrix
  r2 <- eval_boundary(p, pu[, c("sol_1", "sol_2")], ef, zm)
  # correct boundary result based on matrix
  r3 <- tibble::tibble(
    statistic = c("overall", "1", "2"),
    boundary = c(
      r_boundary_given_matrix(pu[, c("sol_1", "sol_2")], ef, zm, bm),
      r_boundary_given_matrix(pu[, "sol_1"], ef[1], diag(1), bm),
      r_boundary_given_matrix(pu[, "sol_2"], ef[2], diag(1), bm)))
  # correct boundary result based on spatial data
  r4 <- tibble::tibble(
    statistic = c("overall", "1", "2"),
    boundary = c(
      sum(r_boundary_given_geometry(pu[, "sol_1"], pl),
          r_boundary_given_geometry(pu[, "sol_2"], pl)),
      r_boundary_given_geometry(pu[, "sol_1"], pl),
      r_boundary_given_geometry(pu[, "sol_2"], pl)))
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("multiple zones (variable edge_factor, zone matrix)", {
  # simulate spatial data
  set.seed(500)
  pl <- raster::raster(matrix(seq_len(10), ncol = 2),
                       xmn = 0, xmx = 2, ymn = 0, ymx = 5)
  pl <- as(pl, "SpatialPolygons")
  bm <- boundary_matrix(pl)
  zm <- matrix(c(0.9, 0.2, 0.2, 0.4), ncol = 2, nrow = 2)
  ef <- c(0.5, 0.2)
  # simulate problem data
  pu <- sp::SpatialPolygonsDataFrame(
    pl, match.ID = FALSE,
    data.frame(id = seq_len(10),
               cost_1 = c(NA, NA, runif(8)),
               cost_2 = c(0.3, NA, runif(8)),
               spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
               spp1_2 = runif(10), spp2_2 = runif(10),
               sol_1 = c(NA, NA, rep(c(0, 1), 4)),
               sol_2 = c(1, NA, rep(c(1, 0), 4))))
  # create problem
  p <- problem(pu,
               features = zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
               cost_column = c("cost_1", "cost_2"))
  # calculate boundary using manually specified boundary matrix
  r1 <- eval_boundary(p, pu[, c("sol_1", "sol_2")], ef, zm, bm)
  # calculate boundary using automatically generated boundary matrix
  r2 <- eval_boundary(p, pu[, c("sol_1", "sol_2")], ef, zm)
  # correct boundary result based on matrix
  r3 <- tibble::tibble(
    statistic = c("overall", "1", "2"),
    boundary = c(
      r_boundary_given_matrix(pu[, c("sol_1", "sol_2")], ef, zm, bm),
      r_boundary_given_matrix(pu[, "sol_1"], ef[1], diag(1), bm),
      r_boundary_given_matrix(pu[, "sol_2"], ef[2], diag(1), bm)))
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})
