context("decisions")

test_that("add_binary_decisions (compile)", {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  o <- compile(p)
  # check that decision variables are correctly applied
  n_pu <- length(raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  expect_equal(o$lb(), rep(0, n_pu))
  expect_equal(o$ub(), rep(1, n_pu))
  expect_equal(o$vtype(), rep("B", n_pu))
})

test_that("add_binary_decisions (solve)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # generate solution
  data(sim_pu_raster, sim_features)
  s <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_default_solver(time_limit = 5) %>%
    solve()
  # check that solutions have correct decisions
  expect_true(all(values(s) %in% c(0L, 1L, NA_integer_)))
})

test_that("add_proportion_decisions (compile)", {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions()
  o <- compile(p)
  # check that decision variables are correctly applied
  n_pu <- length(raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  expect_equal(o$lb(), rep(0, n_pu))
  expect_equal(o$ub(), rep(1, n_pu))
  expect_equal(o$vtype(), rep("S", n_pu))
})

test_that("add_proportion_decisions (solve)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # generate solution
  data(sim_pu_raster, sim_features)
  s <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions()  %>%
    add_default_solver(time_limit = 5) %>%
    solve()
  # check that solutions have correct decisions
  expect_true(isTRUE(all(na.omit(values(s)) <= 1)))
  expect_true(isTRUE(all(na.omit(values(s)) >= 0)))
})

test_that("add_semicontinuous_decisions (compile)", {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_semicontinuous_decisions(0.3)
  o <- compile(p)
  # check that decision variables are correctly applied
  n_pu <- length(raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  expect_equal(o$lb(), rep(0, n_pu))
  expect_equal(o$ub(), rep(0.3, n_pu))
  expect_equal(o$vtype(), rep("S", n_pu))
  # check that invalid inputs result in an error
  expect_error(p %>% add_semicontinuous_decisions(NA))
  expect_error(p %>% add_semicontinuous_decisions(Inf))
  expect_error(p %>% add_semicontinuous_decisions(c()))
  expect_error(p %>% add_semicontinuous_decisions(c(1, 3)))
})

test_that("add_semicontinuous_decisions (solve)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # generate solution
  data(sim_pu_raster, sim_features)
  s <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_semicontinuous_decisions(0.3) %>%
    add_default_solver(time_limit = 5) %>%
    solve()
  # check that solutions have correct decisions
  expect_true(isTRUE(all(round(na.omit(values(s)), 5) <= 0.3)))
  expect_true(isTRUE(all(na.omit(values(s)) >= 0)))
})
