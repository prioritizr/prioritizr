test_that("salt_data", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("prioritizrdata", minimum_version = "0.3.0")
  # import data
  salt_pu <- prioritizrdata::get_salt_pu()
  salt_features <- prioritizrdata::get_salt_features()
  # build and solve problem
  p <-
    problem(salt_pu, salt_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_default_solver(gap = 0.3, verbose = FALSE)
  s <- solve_fixed_seed(p)
  # tests
  suppressMessages(print(p))
  expect_inherits(s, "SpatRaster")
  expect_gte(terra::global(s, "sum", na.rm = TRUE)[[1]], 1)
  expect_true(all(Matrix::rowSums(p$data$rij_matrix[[1]]) > 0))
})

test_that("tas_data", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("prioritizrdata", minimum_version = "0.3.0")
  # import data
  tas_pu <- prioritizrdata::get_tas_pu()
  tas_features <- prioritizrdata::get_tas_features()
  # build and solve problem
  p <-
    problem(tas_pu, tas_features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_locked_in_constraints("locked_in") %>%
    add_locked_out_constraints("locked_out") %>%
    add_default_solver(gap = 0.3, verbose = FALSE)
  s <- solve_fixed_seed(p)
  # tests
  suppressMessages(print(p))
  expect_inherits(s, "sf")
  expect_true(assertthat::has_name(s, "solution_1"))
  expect_gte(sum(s$solution_1), 1)
  expect_true(all(Matrix::rowSums(p$data$rij_matrix[[1]]) > 0))
})

test_that("wa_data", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("prioritizrdata", minimum_version = "0.3.0")
  # import data
  wa_pu <- prioritizrdata::get_wa_pu()
  wa_locked_in <- prioritizrdata::get_wa_locked_in()
  wa_locked_out <- prioritizrdata::get_wa_locked_out()
  wa_features <- prioritizrdata::get_wa_features()
  # build and solve problem
  p <-
    problem(wa_pu, wa_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_locked_in_constraints(wa_locked_in) %>%
    add_locked_in_constraints(wa_locked_out) %>%
    add_default_solver(gap = 0.3, verbose = FALSE)
  s <- solve_fixed_seed(p)
  # tests
  suppressMessages(print(p))
  expect_inherits(s, "SpatRaster")
  expect_gte(terra::global(s, "sum", na.rm = TRUE)[[1]], 1)
  expect_true(all(Matrix::rowSums(p$data$rij_matrix[[1]]) > 0))
})
