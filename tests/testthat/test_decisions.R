context("decisions")

test_that("add_binary_decisions (compile, single zone)", {
  # generate optimization problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
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

test_that("add_binary_decisions (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # generate solution
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.05) %>%
       add_binary_decisions() %>%
       add_default_solver(gap = 0.01, verbose = FALSE)
  s1 <- solve(p)
  s2 <- solve(p)
  # check that solutions have correct decisions
  expect_true(all(raster::values(s1) %in% c(0L, 1L, NA_integer_)))
  expect_equal(raster::values(s1), raster::values(s2))
})

test_that("add_binary_decisions (compile, multiple zones)", {
  # generate optimization problem
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  p <- problem(sim_zones_pu_raster, sim_zones_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(
         matrix(1, nrow = number_of_features(sim_zones_features),
                ncol = number_of_zones(sim_zones_features))) %>%
       add_binary_decisions()
  o <- compile(p)
  # check that decision variables are correctly applied
  n_pu <- length(raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  n_zone <- number_of_zones(sim_zones_features)
  expect_equal(o$lb(), rep(0, n_pu * n_zone))
  expect_equal(o$ub(), rep(1, n_pu * n_zone))
  expect_equal(o$vtype(), rep("B", n_pu * n_zone))
})

test_that("add_binary_decisions (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # generate solution
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  s <- problem(sim_zones_pu_raster, sim_zones_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(
         matrix(1, nrow = number_of_features(sim_zones_features),
                ncol = number_of_zones(sim_zones_features))) %>%
       add_binary_decisions() %>%
       add_default_solver(time_limit = 5, verbose = FALSE) %>%
       solve()
  # check that solutions have correct decisions
  expect_true(all(c(raster::values(s)) %in% c(0L, 1L, NA_integer_)))
})

test_that("add_proportion_decisions (compile, single zone)", {
  # generate optimization problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions()
  o <- compile(p)
  # check that decision variables are correctly applied
  n_pu <- length(raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  expect_equal(o$lb(), rep(0, n_pu))
  expect_equal(o$ub(), rep(1, n_pu))
  expect_equal(o$vtype(), rep("C", n_pu))
})

test_that("add_proportion_decisions (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # generate solution
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions()  %>%
       add_default_solver(time_limit = 5, verbose = FALSE)
  s1 <- solve(p)
  s2 <- solve(p)
  # check that solutions have correct decisions
  expect_true(isTRUE(all(na.omit(values(s1)) <= 1)))
  expect_true(isTRUE(all(na.omit(values(s1)) >= 0)))
  expect_equal(raster::values(s1), raster::values(s2))
})

test_that("add_proportion_decisions (compile, multiple zones)", {
  # generate optimization problem
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  p <- problem(sim_zones_pu_raster, sim_zones_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(
         matrix(1, nrow = number_of_features(sim_zones_features),
                ncol = number_of_zones(sim_zones_features))) %>%
       add_proportion_decisions()
  o <- compile(p)
  # check that decision variables are correctly applied
  n_pu <- length(raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  n_zone <- number_of_zones(sim_zones_features)
  expect_equal(o$lb(), rep(0, n_pu * n_zone))
  expect_equal(o$ub(), rep(1, n_pu * n_zone))
  expect_equal(o$vtype(), rep("C", n_pu * n_zone))
})

test_that("add_proportion_decisions (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # generate solution
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  s <- problem(sim_zones_pu_raster, sim_zones_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(
         matrix(1, nrow = number_of_features(sim_zones_features),
                ncol = number_of_zones(sim_zones_features))) %>%
       add_proportion_decisions() %>%
       add_default_solver(time_limit = 5, verbose = FALSE) %>%
       solve()
  # check that solutions have correct decisions
  expect_true(all(round(na.omit(raster::values(s)), 5) <= 1))
  expect_true(all(round(na.omit(raster::values(s)), 5) >= 0))
})

test_that("add_semicontinuous_decisions (compile, single zone)", {
  # generate optimization problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_semicontinuous_decisions(0.3)
  o <- compile(p)
  # check that decision variables are correctly applied
  n_pu <- length(raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  expect_equal(o$lb(), rep(0, n_pu))
  expect_equal(o$ub(), rep(0.3, n_pu))
  expect_equal(o$vtype(), rep("C", n_pu))
  # check that invalid inputs result in an error
  expect_error(p %>% add_semicontinuous_decisions(NA))
  expect_error(p %>% add_semicontinuous_decisions(Inf))
  expect_error(p %>% add_semicontinuous_decisions(c()))
  expect_error(p %>% add_semicontinuous_decisions(c(1, 3)))
})

test_that("add_semicontinuous_decisions (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # generate solution
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_semicontinuous_decisions(0.3) %>%
       add_default_solver(time_limit = 5, verbose = FALSE)
  s1 <- solve(p)
  s2 <- solve(p)
  # check that solutions have correct decisions
  expect_true(isTRUE(all(round(na.omit(values(s1)), 5) <= 0.3)))
  expect_true(isTRUE(all(na.omit(values(s1)) >= 0)))
  expect_equal(raster::values(s1), raster::values(s2))
})

test_that("add_semicontinuous_decisions (compile, multiple zones)", {
  # generate optimization problem
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  p <- problem(sim_zones_pu_raster, sim_zones_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(
         matrix(1, nrow = number_of_features(sim_zones_features),
                ncol = number_of_zones(sim_zones_features))) %>%
       add_semicontinuous_decisions(0.3)
  o <- compile(p)
  # check that decision variables are correctly applied
  n_pu <- length(raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  n_zone <- number_of_zones(sim_zones_features)
  expect_equal(o$lb(), rep(0, n_pu * n_zone))
  expect_equal(o$ub(), rep(0.3, n_pu * n_zone))
  expect_equal(o$vtype(), rep("C", n_pu * n_zone))
})

test_that("add_semicontinuous_decisions (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # generate solution
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  s <- problem(sim_zones_pu_raster, sim_zones_features) %>%
       add_min_set_objective() %>%
       add_absolute_targets(
         matrix(1, nrow = number_of_features(sim_zones_features),
                ncol = number_of_zones(sim_zones_features))) %>%
       add_semicontinuous_decisions(0.3) %>%
       add_default_solver(time_limit = 5, verbose = FALSE) %>%
       solve()
  # check that solutions have correct decisions
  expect_true(all(round(na.omit(raster::values(s)), 5) <= 0.3))
  expect_true(all(round(na.omit(raster::values(s)), 5) >= 0))
})
