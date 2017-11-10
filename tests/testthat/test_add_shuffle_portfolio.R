context("add_shuffle_portfolio")

test_that("compile", {
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_shuffle_portfolio(2) %>%
       add_default_solver(gap = 0.2)
  # compile problem
  cmp <- compile(p)
  # tests
  expect_is(cmp, "OptimizationProblem")
})

test_that("solve", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  locked_in <- 2
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_locked_in_constraints(locked_in) %>%
       add_shuffle_portfolio(10, remove_duplicates = FALSE) %>%
       add_default_solver(gap = 0.2)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "RasterStack")
  expect_equal(raster::nlayers(s), 10)
  expect_equal(names(s), paste0("solution_", seq_len(raster::nlayers(s))))
  for (i in seq_len(raster::nlayers(s)))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
})

test_that("solve (no duplicates)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create data
  set.seed(500)
  cost <- raster::raster(matrix(c(1, 1, 0.5, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 2, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_shuffle_portfolio(100, remove_duplicates = TRUE) %>%
       add_default_solver(gap = 0.001)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "RasterStack")
  expect_true(raster::nlayers(s) == 2)
  expect_equal(names(s), paste0("solution_", seq_len(raster::nlayers(s))))
  expect_equal(anyDuplicated(apply(raster::as.data.frame(s), 2, paste,
                                   collapse = " ")), 0)
  for (i in seq_len(raster::nlayers(s)))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
})

test_that("solve (parallel processing)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create data
  set.seed(500)
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_shuffle_portfolio(10, threads = 2, remove_duplicates = FALSE) %>%
       add_default_solver(gap = 0.2)
  # solve problem
  s <- suppressWarnings(solve(p))
  # output checks
  expect_is(s, "RasterStack")
  expect_equal(raster::nlayers(s), 10)
  expect_equal(names(s), paste0("solution_", seq_len(raster::nlayers(s))))
  for (i in seq_len(raster::nlayers(s)))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
})

test_that("solve (SpatialPolygonsDataFrame)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create data
  data(sim_pu_polygons, sim_features)
  # create problem
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_absolute_targets(2) %>%
       add_shuffle_portfolio(10, remove_duplicates = FALSE) %>%
       add_default_solver(gap = 0.2)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "SpatialPolygonsDataFrame")
  expect_true(all(paste0("solution_", seq_len(10)) %in% names(s)))
  for (i in seq_len(10)) {
    curr_s <- s[s[[paste0("solution_", i)]] ==  1, ]
    expect_true(all(colSums(raster::extract(sim_features, curr_s,
                                            fun = "sum")) >= 2))
  }
})

test_that("solve (numeric)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create data
  data(sim_pu_polygons, sim_features)
  costs <- sim_pu_polygons$cost
  features <- data.frame(id = seq_len(nlayers(sim_features)),
                         name = names(sim_features))
  rij_mat <- rij_matrix(sim_pu_polygons, sim_features)
  # create problem
  p <- problem(costs, features, rij_matrix = rij_mat) %>%
       add_min_set_objective() %>%
       add_absolute_targets(2) %>%
       add_shuffle_portfolio(10, remove_duplicates = FALSE) %>%
       add_binary_decisions()
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "matrix")
  expect_equal(nrow(s), 10)
  expect_equal(ncol(s), length(costs))
  expect_true(all(paste0("solution_", seq_len(10)) %in% rownames(s)))
  for (i in seq_len(10)) {
    curr_s <- which(s[i, ] == 1)
    expect_true(all(rowSums(rij_mat[, curr_s, drop = FALSE]) >= 2))
  }
})
