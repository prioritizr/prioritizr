context("write_problem")

test_that("correct result (lp format)", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_locked_in_constraints(locked_in) %>%
       add_locked_out_constraints(locked_out) %>%
       add_default_solver(gap = 0, verbose = FALSE)
  # save problem
  path <- tempfile(fileext = ".lp")
  write_problem(p, path)
  # tests
  expect_true(file.exists(path))
  expect_true(any(grepl("Subject To", readLines(path), fixed = TRUE)))
})

test_that("correct result (mps format)", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(2, 10)) %>%
       add_locked_in_constraints(locked_in) %>%
       add_locked_out_constraints(locked_out) %>%
       add_default_solver(gap = 0, verbose = FALSE)
  # save problem
  path <- tempfile(fileext = ".mps")
  write_problem(p, path)
  # tests
  expect_true(file.exists(path))
  expect_true(any(grepl("ROWS", readLines(path), fixed = TRUE)))
})

test_that("invalid inputs", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # create data
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
       add_absolute_targets(c(2, 10)) %>%
       add_locked_in_constraints(locked_in) %>%
       add_locked_out_constraints(locked_out)
  # tests
  expect_error({
    p %>%
    add_min_set_objective()
    write_problem(path = tempfile(fileext = ".asdf"))
  })
  expect_error({
    p %>%
    write_problem(p, tempfile(fileext = ".lp"))
  })
})
