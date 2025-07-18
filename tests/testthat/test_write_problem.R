test_that("automatic (lp format)", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # save problem
  path <- tempfile(fileext = ".lp")
  write_problem(p, path, solver = NULL)
  # tests
  expect_true(file.exists(path))
  expect_true(any(grepl("Subject To", readLines(path), fixed = TRUE)))
})

test_that("Rsymphony (lp format)", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # save problem
  path <- tempfile(fileext = ".lp")
  write_problem(p, path, solver = "rsymphony")
  # tests
  expect_true(file.exists(path))
  expect_true(any(grepl("Subject To", readLines(path), fixed = TRUE)))
})

test_that("Rsymphony (mps format)", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # save problem
  path <- tempfile(fileext = ".mps")
  write_problem(p, path, solver = "rsymphony")
  # tests
  expect_true(file.exists(path))
  expect_true(any(grepl("ROWS", readLines(path), fixed = TRUE)))
  expect_error(
    write_problem(p, tempfile(fileext = ".mps.gz"), solver = "rsymphony")
  )
})

test_that("gurobi (lp format)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # save problem
  path <- tempfile(fileext = ".lp")
  write_problem(p, path, solver = "gurobi")
  # tests
  expect_true(file.exists(path))
  expect_true(any(grepl("Subject To", readLines(path), fixed = TRUE)))
})

test_that("gurobi (mps format)", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- c(
    terra::rast(matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # save problem
  path1 <- tempfile(fileext = ".mps")
  path2 <- tempfile(fileext = ".mps.gz")
  write_problem(p, path1, solver = "gurobi")
  write_problem(p, path2, solver = "gurobi")
  # tests
  expect_true(file.exists(path1))
  expect_true(file.exists(path2))
  expect_true(any(grepl("ROWS", readLines(path1), fixed = TRUE)))
})

test_that("invalid inputs", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # create data
  cost <- terra::rast(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- c(terra::rast(
    matrix(c(2, 1, 1, 0), ncol = 4)),
    terra::rast(matrix(c(10, 10, 10, 10), ncol = 4))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(cost, features) %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked_in) %>%
    add_locked_out_constraints(locked_out)
  # tests
  expect_tidy_error(
    p %>%
      add_min_set_objective() %>%
      write_problem(path = tempfile(fileext = ".asdf")),
    "file extension"
  )
  expect_tidy_error(
    write_problem(p, tempfile(fileext = ".lp")),
    "objective"
  )
})
