context("add_locked_out_constraints")

test_that("logical (compile, single zone)", {
  # create problem
  data(sim_pu_raster, sim_features)
  locked_out <- c(rep(TRUE, 20), rep(FALSE, raster::ncell(sim_pu_raster) - 20))
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(locked_out)
  suppressWarnings(o <- compile(p))
  # check that constraints added correctly
  locked_out_cells <- which(locked_out)
  locked_out_indices <- match(locked_out_cells,
    raster::Which(!is.na(sim_pu_raster), cells = TRUE))
  locked_out_indices <- locked_out_indices[!is.na(locked_out_indices)]
  expect_true(isTRUE(all(o$ub()[locked_out_indices] == 0)))
  expect_true(isTRUE(all(o$ub()[-locked_out_indices] == 1)))
  # invalid inputs
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions()
  expect_error(p %>% add_locked_out_constraints(c(TRUE)))
  expect_error(p %>% add_locked_out_constraints(
    c(TRUE, NA_logical, rep(FALSE, raster::ncell(sim_pu_raster) - 2))))
})

test_that("logical (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  data(sim_pu_raster, sim_features)
  locked_out <- c(rep(TRUE, 20), rep(FALSE, raster::ncell(sim_pu_raster) - 20))
  suppressWarnings({
    s <- problem(sim_pu_raster, sim_features) %>%
         add_min_set_objective() %>%
         add_relative_targets(0.1) %>%
         add_binary_decisions() %>%
         add_locked_out_constraints(locked_out) %>%
         add_default_solver(time_limit = 5, verbose = FALSE) %>%
         solve()
  })
  # check that solutions match expectations
  locked_out_cells <- which(locked_out)
  locked_out_units <- locked_out_cells[locked_out_cells %in%
    raster::Which(!is.na(s), cells = TRUE)]
  expect_true(all(s[locked_out_units] == 0))
})

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
  skip_if_no_fast_solvers_installed()
  # create problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(1:20) %>%
       add_default_solver(time_limit = 5, verbose = FALSE)
  # check that solutions match expectations
  s1 <- solve(p)
  s2 <- solve(p)
  locked_out_cells <- 1:20
  locked_out_units <- locked_out_cells[locked_out_cells %in%
    raster::Which(!is.na(s1), cells = TRUE)]
  expect_true(all(s1[locked_out_units] == 0))
  expect_equal(raster::values(s1), raster::values(s2))
})

test_that("integer (compile, multiple zones)", {
  # create problem
  data(sim_pu_zones_stack, sim_features_zones)
  status <- matrix(FALSE, nrow = raster::ncell(sim_pu_zones_stack),
                   ncol = number_of_zones(sim_features_zones))
  locked_out_ind <- Which(!is.na(sim_pu_zones_stack[[1]]), cells = TRUE)[1:20]
  status[locked_out_ind, 1] <- TRUE
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
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
  skip_if_no_fast_solvers_installed()
  # create and solve problem
  data(sim_pu_zones_stack, sim_features_zones)
  status <- matrix(FALSE, nrow = raster::ncell(sim_pu_zones_stack),
                   ncol = number_of_zones(sim_features_zones))
  locked_out_ind <- Which(!is.na(sim_pu_zones_stack[[1]]), cells = TRUE)[1:20]
  status[locked_out_ind, 1] <- TRUE
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  s <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(status) %>%
       add_default_solver(time_limit = 5, verbose = FALSE) %>%
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
  skip_if_no_fast_solvers_installed()
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints("locked_out") %>%
       add_default_solver(time_limit = 5, verbose = FALSE)
  # check that the solution obeys constraints as expected
  s <- solve(p)
  expect_true(all(s$solution_1[which(sim_pu_polygons$locked_out)] == 0))
})

test_that("character (solve, proportion decisions, single zone", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  data(sim_pu_polygons, sim_features)
  s <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions() %>%
       add_locked_out_constraints("locked_out") %>%
       add_default_solver(time_limit = 5, verbose = FALSE) %>%
       solve()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1[which(sim_pu_polygons$locked_out)] == 0))
})

test_that("character (compile, multiple zones)", {
  # create problem
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
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
  skip_if_no_fast_solvers_installed()
  # create and solve problem
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
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
       add_default_solver(time_limit = 5, verbose = FALSE) %>%
       solve()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1_zone_1[sim_pu_zones_polygons$locked_1] == 0))
})

test_that("character (solve, proportion decisions, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create and solve problem
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
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
       add_default_solver(verbose = FALSE) %>%
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
    sim_locked_out_raster@crs <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
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
  skip_if_no_fast_solvers_installed()
  # create problem
  data(sim_pu_raster, sim_locked_out_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(sim_locked_out_raster) %>%
       add_default_solver(time_limit = 5, verbose = FALSE)
  # check that the solution obeys constraints
  s <- solve(p)
  locked_out_cells <- raster::Which(sim_locked_out_raster &
                                    !is.na(sim_pu_raster), cells = TRUE)
  expect_true(all(s[locked_out_cells] == 0))
})

test_that("raster (compile, multiple zones)", {
  # create problem
  data(sim_pu_zones_stack, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
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
})

test_that("raster (solve, multiple zones)", {
  # create problem
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # make and solve problem
  data(sim_pu_zones_stack, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
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
       add_default_solver(time_limit = 5, verbose = FALSE) %>%
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
    sim_pu_polygons2@proj4string <-
      sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(sim_pu_polygons2)
  })
})

test_that("spatial (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  data(sim_pu_polygons, sim_features)
  locked_ply <- sim_pu_polygons[sim_pu_polygons$locked_out, ]
  p <- problem(sim_pu_polygons, sim_features, "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(locked_ply) %>%
       add_default_solver(time_limit = 5, verbose = FALSE)
  # check that the solution obeys constraints
  s <- solve(p)
  locked_out_units <- which(sim_pu_polygons$locked_out)
  expect_true(all(s$solution_1[locked_out_units] == 0))
})

test_that("spatial (compile, multiple zones, expect error)", {
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  expect_error({
    problem(sim_pu_zones_polygons, sim_features_zones,
                 c("cost_1", "cost_2", "cost_3")) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(sim_pu_zones_polygons[seq_len(20), ]) %>%
    add_default_solver(verbose = FALSE) %>%
    solve()
  })
})

test_that("sf (compile, sf identical to Spatial, single zone)", {
  # create problem
  data(sim_pu_polygons, sim_features)
  sim_sf <- sf::st_as_sf(sim_pu_polygons)
  # make problems
  p1 <- problem(sim_pu_polygons, sim_features, "cost") %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions() %>%
        add_locked_out_constraints(sim_pu_polygons[1:5, ])
  p2 <- problem(sim_pu_polygons, sim_features, "cost") %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions() %>%
        add_locked_out_constraints(sim_sf[1:5, ])
  p3 <- problem(sim_sf, sim_features, "cost") %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions() %>%
        add_locked_out_constraints(sim_sf[1:5, ])
  # compile problems
  o1 <- as.list(compile(p1))
  o2 <- as.list(compile(p2))
  o3 <- as.list(compile(p3))
  # tests
  expect_equal(o1, o2)
  expect_equal(o1, o3)
})

test_that("character (compile, sf identical to Spatial, multiple zones)", {
  # create problem
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  sim_pu_zones_polygons$locked_1 <- TRUE
  sim_pu_zones_polygons$locked_2 <- FALSE
  sim_pu_zones_polygons$locked_3 <- FALSE
  sim_sf <- sf::st_as_sf(sim_pu_zones_polygons)
  # make problems
  p1 <- problem(sim_pu_zones_polygons, sim_features_zones,
                c("cost_1", "cost_2", "cost_3")) %>%
        add_min_set_objective() %>%
        add_relative_targets(targets) %>%
        add_binary_decisions() %>%
        add_locked_out_constraints(c("locked_1", "locked_2", "locked_3"))
  p2 <- problem(sim_sf, sim_features_zones,
                c("cost_1", "cost_2", "cost_3")) %>%
        add_min_set_objective() %>%
        add_relative_targets(targets) %>%
        add_binary_decisions() %>%
        add_locked_out_constraints(c("locked_1", "locked_2", "locked_3"))
  # compile problems
  o1 <- as.list(compile(p1))
  o2 <- as.list(compile(p2))
  # tests
  expect_equal(o1, o2)
})
