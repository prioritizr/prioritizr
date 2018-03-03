context("add_locked_out_constraints")

test_that("integer (compile, single zone)", {
  # create problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(1:20)
  o <- compile(p)
  # check that constraints added correctly
  locked_out_cells <- 1:20
  locked_out_indices <- match(locked_out_cells,
    raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  locked_out_indices <- locked_out_indices[!is.na(locked_out_indices)]
  expect_true(isTRUE(all(o$ub()[locked_out_indices] == 0)))
  expect_true(isTRUE(all(o$ub()[-locked_out_indices] == 1)))
  # check that the solution obeys constraints as expected
  # invalid inputs
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_error(p %>% add_locked_out_constraints(-1))
  expect_error(p %>% add_locked_out_constraints(9.6))
  expect_error(p %>%
                add_locked_out_constraints(raster::ncell(sim_pu_raster) + 1))
})

test_that("integer (solve, single zone)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(1:20) %>%
       add_default_solver(time_limit = 5)
  # check that solutions match expectations
  s <- solve(p)
  locked_out_cells <- 1:20
  locked_out_units <- locked_out_cells[locked_out_cells %in%
    raster::Which(!is.na(s), cells = TRUE)]
  expect_true(all(s[locked_out_units] == 0))
})

test_that("integer (compile, multiple zones)", {
  # create problem
  data(sim_pu_zones_stack, sim_features_zones)
  status <- matrix(FALSE, nrow = raster::ncell(sim_pu_zones_stack),
                   ncol = n_zone(sim_features_zones))
  locked_out_ind <- Which(!is.na(sim_pu_zones_stack[[1]]), cells = TRUE)[1:20]
  status[locked_out_ind, 1] <- TRUE
  targets <- matrix(FALSE, nrow = n_feature(sim_features_zones),
                    ncol = n_zone(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(status)
  o <- compile(p)
  # check that constraints added correctly
  other_ind <- base::setdiff(seq_len(p$number_of_planning_units() *
                                     p$number_of_zones()), 1:20)
  expect_true(isTRUE(all(o$ub()[1:20] == 0)))
  expect_true(isTRUE(all(o$ub()[other_ind] == 1)))
  # invalid inputs
  expect_error({
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_binary_decisions()
    add_locked_in_constraints({s <- status; s[1, 1] <- 2; s})
  })
})

test_that("integer (solve, multiple zones)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create and solve problem
  data(sim_pu_zones_stack, sim_features_zones)
  status <- matrix(FALSE, nrow = raster::ncell(sim_pu_zones_stack),
                   ncol = n_zone(sim_features_zones))
  locked_out_ind <- Which(!is.na(sim_pu_zones_stack[[1]]), cells = TRUE)[1:20]
  status[locked_out_ind, 1] <- TRUE
  targets <- matrix(FALSE, nrow = n_feature(sim_features_zones),
                    ncol = n_zone(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  s <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(status) %>%
       solve()
  # check that solutions match expectations
  expect_true(all(s[[1]][locked_out_ind] == 0))
})

test_that("character (compile, single zone)", {
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints("locked_out")
  o <- compile(p)
  # check that constraints added correctly
  expect_true(isTRUE(all(o$ub()[which(sim_pu_polygons$locked_out)] == 0)))
  # invalid inputs
  expect_error({
    sim_pu_polygons$locked_out <- as.integer(sim_pu_polygons$locked_out)
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints("locked_out")
  })
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(NA_character_)
  })
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints("column_name_that_doesnt_exist")
  })
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints("cost")
  })
})

test_that("character (solve, single zone)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints("locked_out") %>%
       add_default_solver(time_limit = 5)
  # check that the solution obeys constraints as expected
  s <- solve(p)
  expect_true(all(s$solution_1[which(sim_pu_polygons$locked_out)] == 0))
})

test_that("character (solve, proportion decisions, single zone", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_polygons, sim_features)
  s <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions() %>%
       add_locked_out_constraints("locked_out") %>%
       add_default_solver(time_limit = 5) %>%
       solve()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1[which(sim_pu_polygons$locked_out)] == 0))
})

test_that("character (compile, multiple zones)", {
  # create problem
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = n_feature(sim_features_zones),
                    ncol = n_zone(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  sim_pu_zones_polygons$locked_1 <- FALSE
  sim_pu_zones_polygons$locked_2 <- FALSE
  sim_pu_zones_polygons$locked_3 <- FALSE
  sim_pu_zones_polygons$locked_1[1:20] <- TRUE
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_relative_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(c("locked_1", "locked_2", "locked_3"))
  o <- compile(p)
  # check that constraints added correctly
  locked_ind <- seq_len(20)
  other_ind <- seq(21, p$number_of_planning_units() * p$number_of_zones())
  expect_true(isTRUE(all(o$ub()[locked_ind] == 0)))
  expect_true(isTRUE(all(o$ub()[other_ind] == 1)))
  # invalid inputs
  expect_error({
    data(sim_pu_zones_polygons, sim_features_zones)
    sim_pu_zones_polygons$locked_1 <- FALSE
    sim_pu_zones_polygons$locked_2 <- FALSE
    sim_pu_zones_polygons$locked_3 <- FALSE
    sim_pu_zones_polygons$locked_1[1] <- 2
    problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(c("locked_1", "locked_2", "locked_3"))
  })
})

test_that("character (solve, multiple zones)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create and solve problem
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = n_feature(sim_features_zones),
                    ncol = n_zone(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  sim_pu_zones_polygons$locked_1 <- FALSE
  sim_pu_zones_polygons$locked_2 <- FALSE
  sim_pu_zones_polygons$locked_3 <- FALSE
  sim_pu_zones_polygons$locked_1[1:20] <- TRUE
  s <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(c("locked_1", "locked_2", "locked_3")) %>%
       solve()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1_zone_1[sim_pu_zones_polygons$locked_1] == 0))
})

test_that("character (solve, proportion decisions, multiple zones)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create and solve problem
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = n_feature(sim_features_zones),
                    ncol = n_zone(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  sim_pu_zones_polygons$locked_1 <- FALSE
  sim_pu_zones_polygons$locked_2 <- FALSE
  sim_pu_zones_polygons$locked_3 <- FALSE
  sim_pu_zones_polygons$locked_1[1:20] <- TRUE
  s <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_proportion_decisions() %>%
       add_locked_out_constraints(c("locked_1", "locked_2", "locked_3")) %>%
       solve()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1_zone_1[sim_pu_zones_polygons$locked_1] == 0))
})

test_that("raster (compile, single zone)", {
  # create problem
  data(sim_pu_raster, sim_locked_out_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(sim_locked_out_raster)
  o <- compile(p)
  # check that constraints added correctly
  locked_out_cells <- raster::Which(sim_locked_out_raster &
    !is.na(sim_pu_raster), cells = TRUE)
  locked_out_indices <- match(locked_out_cells,
    raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  expect_true(isTRUE(all(o$ub()[locked_out_indices] == 0)))
  expect_true(isTRUE(all(o$ub()[-locked_out_indices] == 1)))
  # check that invalid inputs throw errors
  expect_error({
    data(sim_locked_out_raster)
    extent(sim_locked_out_raster) <- c(0, 20, 0, 20)
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(sim_locked_out_raster)
  })
  expect_error({
    data(sim_locked_out_raster)
    sim_locked_out_raster@crs <- sp::CRS("+init=epsg:4326")
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(sim_locked_out_raster)
  })
  expect_error({
    data(sim_locked_out_raster)
    suppressWarnings(sim_locked_out_raster <- raster::setValues(
      sim_locked_out_raster, NA))
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(sim_locked_out_raster)
  })
  expect_error({
    data(sim_locked_out_raster)
    sim_locked_out_raster <- raster::setValues(sim_locked_out_raster, 0)
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(sim_locked_out_raster)
  })
})

test_that("raster (solve, single zone)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_raster, sim_locked_out_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(sim_locked_out_raster) %>%
       add_default_solver(time_limit = 5)
  # check that the solution obeys constraints
  s <- solve(p)
  locked_out_cells <- raster::Which(sim_locked_out_raster &
                                    !is.na(sim_pu_raster), cells = TRUE)
  expect_true(all(s[locked_out_cells] == 0))
})

test_that("raster (compile, multiple zones)", {
  # create problem
  data(sim_pu_zones_stack, sim_features_zones)
  targets <- matrix(FALSE, nrow = n_feature(sim_features_zones),
                    ncol = n_zone(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  status <- sim_pu_zones_stack
  status[[1]][raster::Which(!is.na(status[[1]]))] <- 0
  status[[1]][raster::Which(!is.na(status[[1]]), cells = TRUE)[1:20]] <- 1
  status[[2]][raster::Which(!is.na(status[[2]]))] <- 0
  status[[3]][raster::Which(!is.na(status[[3]]))] <- 0
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(status)
  o <- compile(p)
  # check that constraints added correctly
  locked_ind <- seq_len(20)
  other_ind <- seq(21, p$number_of_planning_units() * p$number_of_zones())
  expect_true(isTRUE(all(o$ub()[locked_ind] == 0)))
  expect_true(isTRUE(all(o$ub()[other_ind] == 1)))
  # invalid inputs
  expect_error({
    data(sim_pu_zones_stack, sim_features_zones)
    status <- sim_pu_zones_stack
    status[[1]][raster::Which(!is.na(status[[1]]))] <- 0
    status[[1]][raster::Which(!is.na(status[[1]]), cells = TRUE)[1:20]] <- 1
    status[[2]][raster::Which(!is.na(status[[2]]))] <- 0
    status[[2]][raster::Which(!is.na(status[[2]]), cells = TRUE)[1]] <- 1
    status[[2]][1] <- 1
    status[[3]][raster::Which(!is.na(status[[3]]))] <- 0
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(status)
  })
})

test_that("raster (solve, multiple zones)", {
  # create problem
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  data(sim_pu_zones_stack, sim_features_zones)
  targets <- matrix(FALSE, nrow = n_feature(sim_features_zones),
                    ncol = n_zone(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  status <- sim_pu_zones_stack
  status[[1]][raster::Which(!is.na(status[[1]]))] <- 0
  status[[1]][raster::Which(!is.na(status[[1]]), cells = TRUE)[1:20]] <- 1
  status[[2]][raster::Which(!is.na(status[[2]]))] <- 0
  status[[3]][raster::Which(!is.na(status[[3]]))] <- 0
  s <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(status) %>%
       solve()
  # check that the solution obeys constraints
  locked_out_cells <- raster::Which(status[[1]] == 1, cells = TRUE)
  expect_true(all(s[[1]][locked_out_cells] == 0))
})

test_that("spatial (compile, single zone)", {
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(sim_pu_polygons[sim_pu_polygons$locked_out, ])
  o <- compile(p)
  # check that constraints added correctly
  locked_out_units <- which(sim_pu_polygons$locked_out)
  expect_true(isTRUE(all(o$ub()[locked_out_units] == 0)))
  expect_true(isTRUE(all(o$ub()[-locked_out_units] == 1)))
  # check that invalid inputs throw errors
  expect_error({
    data(sim_pu_polygons, sim_features)
    problem(sim_pu_polygons[1:10, ], sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(sim_pu_polygons[50:55, ])
  })
  expect_error({
    data(sim_pu_polygons, sim_features)
    problem(sim_pu_polygons[1:10, ], sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(sim_pu_polygons[0, ])
  })
  expect_error({
    data(sim_pu_polygons, sim_features)
    sim_pu_polygons2 <- sim_pu_polygons[1:10, ]
    sim_pu_polygons2@proj4string <- sp::CRS("+init=epsg:4326")
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(sim_pu_polygons2)
  })
})

test_that("spatial (solve, single zone)", {
  skip_on_cran()
  skip_if_not(default_solver_name() != "lpsymphony")
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_pu_polygons, sim_features)
  locked_ply <- sim_pu_polygons[sim_pu_polygons$locked_out, ]
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(locked_ply) %>%
       add_default_solver(time_limit = 5)
  # check that the solution obeys constraints
  s <- solve(p)
  locked_out_units <- which(sim_pu_polygons$locked_out)
  expect_true(all(s$solution_1[locked_out_units] == 0))
})

test_that("spatial (compile, multiple zones)", {
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = n_feature(sim_features_zones),
                    ncol = n_zone(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  expect_error({
    problem(sim_pu_zones_polygons, sim_features_zones,
                 c("cost_1", "cost_2", "cost_3")) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(sim_pu_zones_polygons[seq_len(20), ]) %>%
    solve()
  })
})
