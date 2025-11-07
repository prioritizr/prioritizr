test_that("assert", {
  f <- function(x) x
  assertthat::on_failure(f) <- function(call, env) {
    "x is not TRUE"
  }
  expect_true(assert(1 == 1))
  expect_error(assert(1 == 2))
  expect_error(assert(f(1 == 2)), "not TRUE")
})


test_that("assert (nested expression)", {
  # load data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # define function
  fun <- function(x) terra::gobal(sim_features, "mean", na.rm = TRUE)[[1]] * 0.5
  # generate error message
  msg <- try(
    problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(targets = fun(sim_features)) %>%
      add_binary_decisions(),
    silent = TRUE
  )
  # tests
  expect_false(grepl("Caused by `NULL`", msg, fixed = TRUE))
  expect_true(grepl("Caused by error", msg, fixed = TRUE))
})

test_that("verify", {
  f <- function(x) x
  assertthat::on_failure(f) <- function(call, env) {
    "x is not TRUE"
  }
  expect_true(verify(1 == 1))
  expect_warning(verify(1 == 2))
  expect_warning(verify(f(1 == 2)), "not TRUE")
})

test_that("fn_current_env", {
  expect_inherits(fn_current_env(), "environment")
})

test_that("fn_caller_env", {
  expect_inherits(fn_caller_env(), "environment")
})
