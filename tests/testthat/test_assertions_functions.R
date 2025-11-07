test_that("assert_dots_empty", {
  f <- function(x, ...) {
    assert_dots_empty()
    TRUE
  }
  expect_true(f())
  expect_true(f(x = 1))
  expect_error(f(x = 1, y = 1))
  expect_error(f(y = 1))
})

test_that("assert_required (simple examples)", {
  # define functions
  f1 <- function(y, w = 1) {assert_required(y); assert_required(w); w + y + 1}
  f2 <- function(x) {assert_required(x); x + 3}
  # tests
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f1(1 + "a")
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f1(object_not_exist)
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f2(1 + "a")
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f2(object_not_exist)
  )
})

test_that("assert_required (magrittr pipe)", {
  # define functions
  f1 <- function(y, w = 1) {assert_required(y); assert_required(w); w + y + 1}
  f2 <- function(x) {assert_required(x); x + 3}
  # tests
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f2(1 + "a") %>%
      f1() %>%
      print()
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f2(object_not_exist) %>%
      f1() %>%
      print()
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f2(2) %>%
      f1(1 + "a") %>%
      print()
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f2(2) %>%
      f1(object_not_exist) %>%
      print()
  )
})

test_that("assert_required (native pipe)", {
  skip_if(getRversion() < "4.1.0", "R version does not support native pipes")
  # define functions
  f1 <- function(y, w = 1) {assert_required(y); assert_required(w); w + y + 1}
  f2 <- function(x) {assert_required(x); x + 3}
  # tests
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f2(1 + "a") |>
      f1() |>
      print()
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f2(object_not_exist) |>
      f1() |>
      print()
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f2(2) |>
      f1(1 + "a") |>
      print()
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    f2(2) |>
      f1(object_not_exist) |>
      print()
  )
})

test_that("assert_required (prioritizr examples, magrittr pipe)", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # tests
  ## check that "caused by error" appears in msg when object not found
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(sim_pu_raster, sim_features) %>%
      add_relative_targets(object_not_exist) %>%
      add_min_set_objective() %>%
      add_binary_decisions() %>%
      print()
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(sim_pu_raster, sim_features) %>%
      add_min_shortfall_objective(budget = object_not_exist) %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      print()
  )
  ## check that "caused by `expression`" appears in msg when object not found
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(sim_pu_raster, sim_features) %>%
      add_relative_targets(1 + "a") %>%
      add_min_set_objective() %>%
      add_binary_decisions() %>%
      print()
  )
  ## check order of errors is correct
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(object_not_exist, sim_features) %>%
      add_relative_targets(1 + "a") %>%
      add_min_set_objective() %>%
      add_binary_decisions() %>%
      print()
  )
  ## check error occurs regardless of position in pipe-chain
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_binary_decisions() %>%
      add_relative_targets(1 + "a") %>%
      print()
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_binary_decisions() %>%
      add_relative_targets(object_not_exist) %>%
      print()
  )
})

test_that("assert_required (prioritizr examples, native pipe)", {
  skip_if(getRversion() < "4.1.0", "R version does not support native pipes")
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # tests
  ## check that "caused by error" appears in msg when object not found
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(sim_pu_raster, sim_features) |>
      add_relative_targets(object_not_exist) |>
      add_min_set_objective() |>
      add_binary_decisions() |>
      print()
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(sim_pu_raster, sim_features) |>
      add_min_shortfall_objective(budget = object_not_exist) |>
      add_binary_decisions() |>
      add_relative_targets(0.1) |>
      print()
  )
  ## check that "caused by `expression`" appears in msg when object not found
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(sim_pu_raster, sim_features) |>
      add_relative_targets(1 + "a") |>
      add_min_set_objective() |>
      add_binary_decisions() |>
      print()
  )
  ## check order of errors is correct
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(object_not_exist, sim_features) |>
      add_relative_targets(1 + "a") |>
      add_min_set_objective() |>
      add_binary_decisions() |>
      print()
  )
  ## check error occurs regardless of position in pipe-chain
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(sim_pu_raster, sim_features) |>
      add_min_set_objective() |>
      add_binary_decisions() |>
      add_relative_targets(1 + "a") |>
      print()
  )
  expect_snapshot(
    error = TRUE,
    transform = cli::ansi_strip,
    problem(sim_pu_raster, sim_features) |>
      add_min_set_objective() |>
      add_binary_decisions() |>
      add_relative_targets(object_not_exist) |>
      print()
  )
})

test_that("assert_valid_method_arg", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # build message
  msg <- try(
    assert_valid_method_arg(problem(sim_pu_raster, sim_features)),
    silent = TRUE
  )
  # run tests
  expect_true(grepl("add_auto_targets", msg, fixed = TRUE))
  expect_true(assert_valid_method_arg("a"))
})
