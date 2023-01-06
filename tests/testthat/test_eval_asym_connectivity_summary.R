context("eval_asym_connectivity_summary")

test_that("single zone (zone matrix = 1)", {
  # simulate spatial data
  set.seed(500)
  pl <- terra::rast(matrix(seq_len(10), ncol = 2),
                       xmn = 0, xmx = 2, ymn = 0, ymx = 5)
  pl <- as(pl, "SpatialPolygons")
  zm <- diag(1)
  # simulate problem data
  pu <- sp::SpatialPolygonsDataFrame(
    pl, match.ID = FALSE,
    data.frame(id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
               spp1 = runif(10), spp2 = c(rpois(9, 4), NA),
               solution = c(0, NA, 1, 1, 1, 0, 0, 0, 1, 0)))
  # simulate connectivity matrix
  cm <- matrix(runif(nrow(pu)^2), nrow = nrow(pu), ncol = nrow(pu))
  # create problem
  p <- problem(pu, features = c("spp1", "spp2"), cost_column = "cost")
  # calculate connectivity (dgCMatrix)
  r1 <- eval_asym_connectivity_summary(p, pu[, "solution"], zm, cm)
  # calculate connectivity (matrix)
  r2 <- eval_asym_connectivity_summary(p, pu[, "solution"], zm, as.matrix(cm))
  # calculate connectivity (array)
  r3 <- eval_asym_connectivity_summary(
    p, pu[, "solution"], NULL, as_connectivity_array(zm, cm))
  # correct connectivity result
  r4 <- tibble::tibble(
    summary = "overall",
    asym_connectivity = r_asym_connectivity_given_matrix(
      pu$solution, zm, cm))
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("single zone (variable zone matrix)", {
  # simulate spatial data
  set.seed(500)
  pl <- terra::rast(matrix(seq_len(10), ncol = 2),
                       xmn = 0, xmx = 2, ymn = 0, ymx = 5)
  pl <- as(pl, "SpatialPolygons")
  zm <- matrix(0.4, ncol = 1, nrow = 1)
  # simulate problem data
  pu <- sp::SpatialPolygonsDataFrame(
    pl, match.ID = FALSE,
    data.frame(id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
               spp1 = runif(10), spp2 = c(rpois(9, 4), NA), con = runif(10),
               solution = c(0, NA, 1, 1, 1, 0, 0, 0, 1, 0)))
  # simulate connectivity matrix
  cm <- matrix(runif(nrow(pu)^2), nrow = nrow(pu), ncol = nrow(pu))
  # create problem
  p <- problem(pu, features = c("spp1", "spp2"), cost_column = "cost")
  # calculate connectivity (dgCMatrix)
  r1 <- eval_asym_connectivity_summary(p, pu[, "solution"], zm, cm)
  # calculate connectivity (matrix)
  r2 <- eval_asym_connectivity_summary(p, pu[, "solution"], zm, as.matrix(cm))
  # calculate connectivity (array)
  r3 <- eval_asym_connectivity_summary(
    p, pu[, "solution"], NULL, as_connectivity_array(zm, cm))
  # correct connectivity result
  r4 <- tibble::tibble(
    summary = "overall",
    asym_connectivity = r_asym_connectivity_given_matrix(
      pu$solution, zm, cm))
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("multiple zones (zone matrix = 1)", {
  # simulate spatial data
  set.seed(500)
  pl <- terra::rast(matrix(seq_len(10), ncol = 2),
                       xmn = 0, xmx = 2, ymn = 0, ymx = 5)
  pl <- as(pl, "SpatialPolygons")
  zm <- matrix(1, ncol = 2, nrow = 2)
  # simulate problem data
  pu <- sp::SpatialPolygonsDataFrame(
    pl, match.ID = FALSE,
    data.frame(id = seq_len(10),
               con = runif(10),
               cost_1 = c(NA, NA, runif(8)),
               cost_2 = c(0.3, NA, runif(8)),
               spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
               spp1_2 = runif(10), spp2_2 = runif(10),
               sol_1 = c(NA, NA, rep(c(0, 1), 4)),
               sol_2 = c(1, NA, rep(c(1, 0), 4))))
  # simulate connectivity matrix
  cm <- matrix(runif(nrow(pu)^2), nrow = nrow(pu), ncol = nrow(pu))
  # create problem
  p <- problem(pu,
               features = zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
               cost_column = c("cost_1", "cost_2"))
  # calculate connectivity (dgCMatrix)
  r1 <- eval_asym_connectivity_summary(p, pu[, c("sol_1", "sol_2")], zm, cm)
  # calculate connectivity (matrix)
  r2 <- eval_asym_connectivity_summary(
    p, pu[, c("sol_1", "sol_2")], zm, as.matrix(cm))
  # calculate connectivity (array)
  r3 <- eval_asym_connectivity_summary(
    p, pu[, c("sol_1", "sol_2")], NULL, as_connectivity_array(zm, cm))
  # correct connectivity result
  r4 <- tibble::tibble(
    summary = c("overall", "1", "2"),
    asym_connectivity = c(
      r_asym_connectivity_given_matrix(pu[, c("sol_1", "sol_2")], zm, cm),
      r_asym_connectivity_given_matrix(pu[, c("sol_1")], diag(1), cm),
      r_asym_connectivity_given_matrix(pu[, c("sol_2")], diag(1), cm)))
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("multiple zones (zone matrix = identity matrix)", {
  # simulate spatial data
  set.seed(500)
  pl <- terra::rast(matrix(seq_len(10), ncol = 2),
                       xmn = 0, xmx = 2, ymn = 0, ymx = 5)
  pl <- as(pl, "SpatialPolygons")
  zm <- diag(2)
  # simulate problem data
  pu <- sp::SpatialPolygonsDataFrame(
    pl, match.ID = FALSE,
    data.frame(id = seq_len(10),
               con = runif(10),
               cost_1 = c(NA, NA, runif(8)),
               cost_2 = c(0.3, NA, runif(8)),
               spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
               spp1_2 = runif(10), spp2_2 = runif(10),
               sol_1 = c(NA, NA, rep(c(0, 1), 4)),
               sol_2 = c(1, NA, rep(c(1, 0), 4))))
  # simulate connectivity matrix
  cm <- matrix(runif(nrow(pu)^2), nrow = nrow(pu), ncol = nrow(pu))
  # create problem
  p <- problem(pu,
               features = zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
               cost_column = c("cost_1", "cost_2"))
  # calculate connectivity (dgCMatrix)
  r1 <- eval_asym_connectivity_summary(p, pu[, c("sol_1", "sol_2")], zm, cm)
  # calculate connectivity (matrix)
  r2 <- eval_asym_connectivity_summary(
    p, pu[, c("sol_1", "sol_2")], zm, as.matrix(cm))
  # calculate connectivity (array)
  r3 <- eval_asym_connectivity_summary(
    p, pu[, c("sol_1", "sol_2")], NULL, as_connectivity_array(zm, cm))
  # correct connectivity result
  r4 <- tibble::tibble(
    summary = c("overall", "1", "2"),
    asym_connectivity = c(
      r_asym_connectivity_given_matrix(
        pu[, c("sol_1", "sol_2")], zm, cm),
      r_asym_connectivity_given_matrix(
        pu[, c("sol_1")], diag(1), cm),
      r_asym_connectivity_given_matrix(
        pu[, c("sol_2")], diag(1), cm)))
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("multiple zones (variable zone matrix)", {
  # simulate spatial data
  set.seed(500)
  pl <- terra::rast(matrix(seq_len(10), ncol = 2),
                       xmn = 0, xmx = 2, ymn = 0, ymx = 5)
  pl <- as(pl, "SpatialPolygons")
  zm <- matrix(c(0.9, 0.2, 0.2, 0.4), ncol = 2, nrow = 2)
  # simulate problem data
  pu <- sp::SpatialPolygonsDataFrame(
    pl, match.ID = FALSE,
    data.frame(id = seq_len(10),
               con = runif(10),
               cost_1 = c(NA, NA, runif(8)),
               cost_2 = c(0.3, NA, runif(8)),
               spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
               spp1_2 = runif(10), spp2_2 = runif(10),
               sol_1 = c(NA, NA, rep(c(0, 1), 4)),
               sol_2 = c(1, NA, rep(c(1, 0), 4))))
  # simulate connectivity matrix
  cm <- matrix(runif(nrow(pu)^2), nrow = nrow(pu), ncol = nrow(pu))
  # create problem
  p <- problem(pu,
               features = zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
               cost_column = c("cost_1", "cost_2"))
  # calculate connectivity (dgCMatrix)
  r1 <- eval_asym_connectivity_summary(p, pu[, c("sol_1", "sol_2")], zm, cm)
  # calculate connectivity (matrix)
  r2 <- eval_asym_connectivity_summary(
    p, pu[, c("sol_1", "sol_2")], zm, as.matrix(cm))
  # calculate connectivity (array)
  ## calculate metrics
  r3 <- eval_asym_connectivity_summary(
    p, pu[, c("sol_1", "sol_2")], NULL, as_connectivity_array(zm, cm))
  ## rescale metrics to account for diagonal values != 1
  r3[[2]][[2]] <- r3[[2]][[2]] * (1 / zm[1, 1])
  r3[[2]][[3]] <- r3[[2]][[3]] * (1 / zm[2, 2])
  # correct connectivity result
  r4 <- tibble::tibble(
    summary = c("overall", "1", "2"),
    asym_connectivity = c(
      r_asym_connectivity_given_matrix(pu[, c("sol_1", "sol_2")], zm, cm),
      r_asym_connectivity_given_matrix(pu[, "sol_1"], diag(1), cm),
      r_asym_connectivity_given_matrix(pu[, "sol_2"], diag(1), cm)))
  # run tests
  expect_equal(r1, r2)
  expect_equal(r1, r3)
  expect_equal(r1, r4)
  expect_equal(nrow(na.omit(r1)), nrow(r1))
})

test_that("expected warnings", {
  # simulate spatial data
  set.seed(500)
  pl <- terra::rast(matrix(seq_len(10), ncol = 2),
                       xmn = 0, xmx = 2, ymn = 0, ymx = 5)
  pl <- as(pl, "SpatialPolygons")
  zm <- diag(1)
  # simulate problem data
  pu <- sp::SpatialPolygonsDataFrame(
    pl, match.ID = FALSE,
    data.frame(id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
               spp1 = runif(10), spp2 = c(rpois(9, 4), NA),
               solution = c(0, NA, 1, 1, 1, 0, 0, 0, 1, 0)))
  # simulate connectivity matrix
  cm <- boundary_matrix(pl)
  # create problem
  p <- problem(pu, features = c("spp1", "spp2"), cost_column = "cost")
  # tests
  expect_warning(eval_asym_connectivity_summary(p, pu[, "solution"], data = cm))
})
