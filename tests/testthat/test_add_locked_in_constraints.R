context("add_locked_in_constraints")

test_that("integer (compile, single zone)", {
  # create problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(seq_len(raster::ncell(sim_pu_raster)))
  suppressWarnings(o <- compile(p))
  # check that constraints added correctly
  expect_true(isTRUE(all(o$lb()[seq_len(p$number_of_planning_units())] == 1)))
  # invalid inputs
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_error(p %>% add_locked_in_constraints(-1))
  expect_error(p %>% add_locked_in_constraints(9.6))
  expect_error(p %>% add_locked_in_constraints(raster::ncell(sim_pu_raster) +
                                               1))
})

test_that("integer (solve, single zone)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_raster, sim_features)
  suppressWarnings({
    s <- problem(sim_pu_raster, sim_features) %>%
         add_min_set_objective() %>%
         add_relative_targets(0.1) %>%
         add_binary_decisions() %>%
         add_locked_in_constraints(seq_len(raster::ncell(sim_pu_raster))) %>%
         add_default_solver(time_limit = 5) %>%
         solve()
  })
  # check that the solution obeys constraints as expected
  expect_true(all(raster::Which(is.na(s), cells = TRUE) ==
      raster::Which(is.na(sim_pu_raster), cells = TRUE)))
  expect_true(isTRUE(all(raster::values(s)[raster::Which(!is.na(s),
                                                        cells = TRUE)] == 1)))
})

test_that("matrix (compile, multiple zones)", {
  # create problem
  data(sim_pu_zones_stack, sim_features_zones)
  status <- matrix(FALSE, nrow = raster::ncell(sim_pu_zones_stack),
                   ncol = number_of_zones(sim_features_zones))
  status[, 1] <- TRUE
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(status)
  o <- compile(p)
  # check that constraints added correctly
  first_zone_ind <- seq_len(p$number_of_planning_units())
  other_zone_ind <- p$number_of_planning_units() +
                    seq_len(p$number_of_planning_units() * 2)
  expect_true(isTRUE(all(o$lb()[first_zone_ind] == 1)))
  expect_true(isTRUE(all(o$lb()[other_zone_ind] == 0)))
  # invalid inputs
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_relative_targets(targets) %>%
       add_binary_decisions()
  expect_error(p %>%
               add_locked_in_constraints({s <- status; s[1, ] <- TRUE; s}))
  expect_error(p %>% add_locked_in_constraints({s <- status; s[1, 1] <- 2; s}))
})

test_that("matrix (solve, multiple zones)", {
  # create problem
  data(sim_pu_zones_stack, sim_features_zones)
  status <- matrix(FALSE, nrow = raster::ncell(sim_pu_zones_stack),
                   ncol = number_of_zones(sim_features_zones))
  status[, 1] <- TRUE
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  s <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(status) %>%
       solve()
  # check that the solution obeys constraints as expected
  for (i in seq_len(raster::nlayers(sim_pu_zones_stack)))
    expect_true(all(raster::Which(is.na(s[[i]]), cells = TRUE) ==
                    raster::Which(is.na(sim_pu_zones_stack[[i]]),
                                  cells = TRUE)))
  for (i in seq_len(raster::nlayers(sim_pu_zones_stack)))
    expect_true(all(raster::values(s[[i]])[
      raster::Which(!is.na(s[[i]]), cells = TRUE)] == as.numeric(i == 1)))
})

test_that("character (compile, single zone)", {
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints("locked_in")
  o <- compile(p)
  # check that constraints added correctly
  expect_true(isTRUE(all(o$lb()[which(sim_pu_polygons$locked_in)] == 1)))
  # invalid inputs
  expect_error({
    sim_pu_polygons$locked_in <- as.integer(sim_pu_polygons$locked_in)
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints("locked_in")
  })
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints(NA_character_)
  })
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints("column_name_that_doesnt_exist")
  })
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints("cost")
  })
})

test_that("character (solve, single zone)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_polygons, sim_features)
  s <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints("locked_in") %>%
       add_default_solver(time_limit = 5) %>%
       solve()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1[which(sim_pu_polygons$locked_in)] == 1))
})

test_that("character (compile, multiple zones)", {
  # create problem
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  sim_pu_zones_polygons$locked_1 <- TRUE
  sim_pu_zones_polygons$locked_2 <- FALSE
  sim_pu_zones_polygons$locked_3 <- FALSE
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(c("locked_1", "locked_2", "locked_3"))
  o <- compile(p)
  # check that constraints added correctly
  first_zone_ind <- seq_len(p$number_of_planning_units())
  other_zone_ind <- p$number_of_planning_units() +
                    seq_len(p$number_of_planning_units() * 2)
  expect_true(isTRUE(all(o$lb()[first_zone_ind] == 1)))
  expect_true(isTRUE(all(o$lb()[other_zone_ind] == 0)))
  # invalid inputs
  expect_error({
    data(sim_pu_zones_polygons, sim_features_zones)
    sim_pu_zones_polygons$locked_1 <- TRUE
    sim_pu_zones_polygons$locked_2 <- c(TRUE, rep(FALSE,
                                        length(sim_pu_zones_polygons) - 1))
    sim_pu_zones_polygons$locked_3 <- FALSE
    p <- problem(sim_pu_zones_polygons, sim_features_zones,
                 c("cost_1", "cost_2", "cost_3")) %>%
         add_min_set_objective() %>%
         add_relative_targets(targets) %>%
         add_binary_decisions() %>%
         add_locked_in_constraints(c("locked_1", "locked_2", "locked_3"))
  })
  expect_error({
    data(sim_pu_zones_polygons, sim_features_zones)
    sim_pu_zones_polygons$locked_1 <- 1
    sim_pu_zones_polygons$locked_2 <- FALSE
    sim_pu_zones_polygons$locked_3 <- FALSE
    p <- problem(sim_pu_zones_polygons, sim_features_zones,
                 c("cost_1", "cost_2", "cost_3")) %>%
         add_min_set_objective() %>%
         add_relative_targets(targets) %>%
         add_binary_decisions() %>%
         add_locked_in_constraints(c("locked_1", "locked_2", "locked_3"))
  })
})

test_that("character (solve, multiple zones)", {
  # create problem
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  sim_pu_zones_polygons$locked_1 <- TRUE
  sim_pu_zones_polygons$locked_2 <- FALSE
  sim_pu_zones_polygons$locked_3 <- FALSE
  s <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(c("locked_1", "locked_2", "locked_3")) %>%
       solve()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1_zone_1 == 1))
  expect_true(all(s$solution_1_zone_2 == 0))
  expect_true(all(s$solution_1_zone_3 == 0))
})

test_that("character (solve, single zone, proportion decisions)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_polygons, sim_features)
  s <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions() %>%
       add_locked_in_constraints("locked_in") %>%
       add_default_solver(time_limit = 5) %>%
       solve()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1[which(sim_pu_polygons$locked_in)] == 1))
})

test_that("character (solve, multiple zones, proportion decisions)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  sim_pu_zones_polygons$locked_1 <- TRUE
  sim_pu_zones_polygons$locked_2 <- FALSE
  sim_pu_zones_polygons$locked_3 <- FALSE
  s <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(targets) %>%
       add_proportion_decisions() %>%
       add_locked_in_constraints(c("locked_1", "locked_2", "locked_3")) %>%
       solve()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1_zone_1 == 1))
  expect_true(all(s$solution_1_zone_2 == 0))
  expect_true(all(s$solution_1_zone_3 == 0))
})

test_that("raster (compile, single zone)", {
  # create problem
  data(sim_pu_raster, sim_locked_in_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(sim_locked_in_raster)
  o <- compile(p)
  # check that constraints added correctly
  locked_in_cells <- raster::Which(sim_locked_in_raster & !is.na(sim_pu_raster),
                                   cells = TRUE)
  locked_in_indices <- match(locked_in_cells,
    raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  expect_true(isTRUE(all(o$lb()[locked_in_indices] == 1)))
  expect_true(isTRUE(all(o$lb()[-locked_in_indices] == 0)))
  # check that invalid inputs throw errors
  expect_error({
    data(sim_locked_in_raster)
    extent(sim_locked_in_raster) <- c(0, 20, 0, 20)
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints(sim_locked_in_raster)
  })
  expect_error({
    data(sim_locked_in_raster)
    sim_locked_in_raster@crs <- sp::CRS("+init=epsg:4326")
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints(sim_locked_in_raster)
  })
  expect_error({
    data(sim_locked_in_raster)
    suppressWarnings(sim_locked_in_raster <- raster::setValues(
      sim_locked_in_raster, NA))
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints(sim_locked_in_raster)
  })
  expect_error({
    data(sim_locked_in_raster)
    sim_locked_in_raster <- raster::setValues(sim_locked_in_raster, 0)
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints(sim_locked_in_raster)
  })
})

test_that("raster (solve, single zone)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_raster, sim_locked_in_raster, sim_features)
  s <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(sim_locked_in_raster) %>%
       add_default_solver(time_limit = 5) %>%
       solve()
  # check that the solution obeys constraints
  locked_in_cells <- raster::Which(sim_locked_in_raster & !is.na(sim_pu_raster),
                                   cells = TRUE)
  expect_true(all(s[locked_in_cells] == 1))
})

test_that("raster (compile, multiple zones)", {
  # create problem
  data(sim_pu_zones_stack, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  status <- sim_pu_zones_stack
  status[[1]][raster::Which(!is.na(status[[1]]))] <- 1
  status[[2]][raster::Which(!is.na(status[[2]]))] <- 0
  status[[3]][raster::Which(!is.na(status[[3]]))] <- 0
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(status)
  o <- compile(p)
  # check that constraints added correctly
  first_zone_ind <- seq_len(p$number_of_planning_units())
  other_zone_ind <- p$number_of_planning_units() +
                    seq_len(p$number_of_planning_units() * 2)
  expect_true(isTRUE(all(o$lb()[first_zone_ind] == 1)))
  expect_true(isTRUE(all(o$lb()[other_zone_ind] == 0)))
  # invalid inputs
  expect_error({
    data(sim_pu_zones_stack, sim_features_zones)
    status <- sim_pu_zones_stack
    status[[1]][raster::Which(!is.na(status[[1]]))] <- 1
    status[[2]][raster::Which(!is.na(status[[2]]))] <- 0
    status[[2]][1] <- 1
    status[[3]][raster::Which(!is.na(status[[3]]))] <- 0
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(status)
  })
})

test_that("raster (solve, multiple zones)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_zones_stack, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  status <- sim_pu_zones_stack
  status[[1]][raster::Which(!is.na(status[[1]]))] <- 1
  status[[2]][raster::Which(!is.na(status[[2]]))] <- 0
  status[[3]][raster::Which(!is.na(status[[3]]))] <- 0
  s <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(status) %>%
       solve()
  # check that the solution obeys constraints as expected
  for (i in seq_len(raster::nlayers(sim_pu_zones_stack)))
    expect_true(all(raster::Which(is.na(s[[i]]), cells = TRUE) ==
                    raster::Which(is.na(sim_pu_zones_stack[[i]]),
                                  cells = TRUE)))
  for (i in seq_len(raster::nlayers(sim_pu_zones_stack)))
    expect_true(all(raster::values(s[[i]])[
      raster::Which(!is.na(s[[i]]), cells = TRUE)] == as.numeric(i == 1)))
})

test_that("spatial (compile, single zone)", {
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(sim_pu_polygons[sim_pu_polygons$locked_in, ])
  o <- compile(p)
  # check that constraints added correctly
  locked_in_units <- which(sim_pu_polygons$locked_in)
  expect_true(isTRUE(all(o$lb()[locked_in_units] == 1)))
  expect_true(isTRUE(all(o$lb()[-locked_in_units] == 0)))
  # check that invalid inputs throw errors
  expect_error({
    data(sim_pu_polygons, sim_features)
    problem(sim_pu_polygons[1:10, ], sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(sim_pu_polygons[50:55, ])
  })
  expect_error({
    data(sim_pu_polygons, sim_features)
    problem(sim_pu_polygons[1:10, ], sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints(sim_pu_polygons[0, ])
  })
  expect_error({
    data(sim_pu_polygons, sim_features)
    sim_pu_polygons2 <- sim_pu_polygons[1:10, ]
    sim_pu_polygons2@proj4string <- sp::CRS("+init=epsg:4326")
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints(sim_pu_polygons2)
  })
})

test_that("spatial (solve, single zone)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_polygons, sim_features)
  s <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(sim_pu_polygons[
         sim_pu_polygons$locked_in, ]) %>%
       add_default_solver(time_limit = 5) %>%
       solve()
  locked_in_units <- which(sim_pu_polygons$locked_in)
  expect_true(all(s$solution_1[locked_in_units] == 1))
})

test_that("spatial (compile, multiple zones)", {
  expect_error({
    data(sim_pu_zones_stack, sim_features_zones, sim_pu_polygons)
    targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                      ncol = number_of_zones(sim_features_zones))
    targets[] <- 0
    targets[, 1] <- 1
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(sim_pu_polygons[1:5, ]) %>%
    solve()
  })
})
