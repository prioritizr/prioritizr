context("add_cuts_portfolio")

test_that("compile", {
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
       add_cuts_portfolio(2) %>%
       add_default_solver(gap = 0.2)
  # compile problem
  cmp <- compile(p)
})

test_that("solve (number_solutions within feasible limit)", {
  skip_on_cran()
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 2))
  features <- raster::stack(raster::raster(matrix(c(2, 2, 1, 0), ncol = 2)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 2)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_cuts_portfolio(2) %>%
       add_default_solver(gap = 0.2)
  # solve problem
  s <- solve(p)
  # output checks
  expect_is(s, "RasterStack")
  expect_equal(raster::nlayers(s), 2)
  expect_equal(names(s), c("solution_1", "solution_2"))
  for (i in seq_len(raster::nlayers(s)))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
})

test_that("solve (number_solutions outside limit)", {
  skip_on_cran()
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 2))
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 2)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 2)))
  locked_in <- 2
  locked_out <- 1
  # create problem
  p <- problem(cost, features) %>%
          add_min_set_objective() %>%
          add_absolute_targets(c(2, 10)) %>%
          add_locked_in_constraints(locked_in) %>%
          add_locked_out_constraints(locked_out) %>%
          add_cuts_portfolio(100) %>%
          add_default_solver(gap = 0.5)
  # solve problem
  s <- suppressWarnings(tryCatch(solve(p),
                warning = function(w) return(list(solve(p), w$message))))
  print("here1")
  print(s)
  print("here2")
  warn <- s[[2]]
  s <- s[[1]]
  # output checks
  expect_is(s, "RasterLayer")
  expect_true(raster::nlayers(s) == 1)
  expect_equal(names(s), "solution_1")
  for (i in seq_len(raster::nlayers(s)))
    expect_true(all(raster::cellStats(s[[i]] * features, "sum") >= c(2, 10)))
  expect_equal(warn,
               "there are only 1 feasible solutions within the optimality gap")
})
