context("add_shuffle_portfolio")

test_that("compile", {
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 2))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 2)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 2)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_shuffle_portfolio(2) %>%
       add_default_solver(gap = 0.2)
  # compile problem
  cmp <- compile(p)
})

test_that("solve", {
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 2))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 2)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 2)))
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
  # create data
  set.seed(500)
  cost <- raster::raster(matrix(c(1, 1, 0.5, NA), ncol = 2))
  features <- raster::stack(raster::raster(matrix(c(2, 2, 1, 0), ncol = 2)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 2)))
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
  # create data
  set.seed(500)
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 2))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 2)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 2)))
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
