context("add_manual_locked_constraints")

test_that("data.frame (compile, single zone)", {
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions() %>%
       add_manual_locked_constraints(data.frame(pu = seq_len(5),
                                         status = rep(0.3, 10)))
  suppressWarnings(o <- compile(p))
  # check that constraints added correctly
  locked_pos <- seq_len(5)
  other_pos <- setdiff(seq_len(nrow(sim_pu_polygons)), locked_pos)
  expect_true(isTRUE(all(o$lb()[locked_pos] == 0.3)))
  expect_true(isTRUE(all(o$ub()[locked_pos] == 0.3)))
  expect_true(isTRUE(all(o$lb()[other_pos] == 0)))
  expect_true(isTRUE(all(o$ub()[other_pos] == 1)))
})

test_that("data.frame (solve, single zone)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(any_solvers_installed())
  # create and solve problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.1) %>%
       add_proportion_decisions() %>%
       add_manual_locked_constraints(data.frame(pu = seq_len(5),
                                                status = rep(0.3, 10))) %>%
       add_default_solver(verbose = FALSE)
  s1 <- solve(p)
  s2 <- solve(p)
  # check that the solution obeys constraints as expected
  expect_true(all(s1$solution_1[seq_len(5)] == 0.3))
  expect_equal(s1$solution_1, s2$solution_1)
})

test_that("data.frame (compile, multiple zones (factor))", {
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
       add_absolute_targets(targets) %>%
       add_proportion_decisions() %>%
       add_manual_locked_constraints(data.frame(pu = c(seq_len(5), 20),
                                                zone = c(rep("zone_1", 5),
                                                         "zone_2"),
                                                status = 0.3,
                                                stringsAsFactors = TRUE))
  suppressWarnings(o <- compile(p))
  # check that constraints added correctly
  locked_pos <- c(seq_len(5), nrow(sim_pu_zones_polygons) + 20)
  other_pos <- setdiff(seq_len(p$number_of_planning_units() *
                               p$number_of_zones()), locked_pos)
  expect_true(isTRUE(all(o$lb()[locked_pos] == 0.3)))
  expect_true(isTRUE(all(o$ub()[locked_pos] == 0.3)))
  expect_true(isTRUE(all(o$lb()[other_pos] == 0)))
  expect_true(isTRUE(all(o$ub()[other_pos] == 1)))
})

test_that("data.frame (compile, multiple zones (character))", {
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
       add_absolute_targets(targets) %>%
       add_proportion_decisions() %>%
       add_manual_locked_constraints(data.frame(pu = c(seq_len(5), 20),
                                                zone = c(rep("zone_1", 5),
                                                         "zone_2"),
                                                status = 0.3,
                                                stringsAsFactors = TRUE))
  suppressWarnings(o <- compile(p))
  # check that constraints added correctly
  locked_pos <- c(seq_len(5), nrow(sim_pu_zones_polygons) + 20)
  other_pos <- setdiff(seq_len(p$number_of_planning_units() *
                               p$number_of_zones()), locked_pos)
  expect_true(isTRUE(all(o$lb()[locked_pos] == 0.3)))
  expect_true(isTRUE(all(o$ub()[locked_pos] == 0.3)))
  expect_true(isTRUE(all(o$lb()[other_pos] == 0)))
  expect_true(isTRUE(all(o$ub()[other_pos] == 1)))
})

test_that("data.frame (solve, multiple zones)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(any_solvers_installed())
  # create and solve problem
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  s <- problem(sim_pu_zones_polygons, sim_features_zones,
               c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_proportion_decisions() %>%
       add_manual_locked_constraints(data.frame(pu = c(seq_len(5), 20),
                                                zone = c(rep("zone_1", 5),
                                                             "zone_2"),
                                                status = 0.3)) %>%
       add_default_solver(verbose = FALSE) %>%
       solve()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1_zone_1[seq_len(5)] == 0.3))
  expect_true(all(s$solution_1_zone_2[20] == 0.3))
})

test_that("invalid inputs (single zone)", {
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
       add_min_set_objective() %>%
       add_absolute_targets(1) %>%
       add_proportion_decisions()
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = integer(0),
                                                status = integer(0)))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = "a", status = 1))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = 1000, status = 0.2))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = -5, status = 0.2))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = NA_integer_, status = 0.2))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = 1, status = NA_real_))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = 1, zone = "zone_10",
                                                status = 1))
  })
})

test_that("invalid inputs (multiple zones)", {
  data(sim_pu_zones_polygons, sim_features_zones)
  targets <- matrix(FALSE, nrow = number_of_features(sim_features_zones),
                    ncol = number_of_zones(sim_features_zones))
  targets[] <- 0
  targets[, 1] <- 1
  p <- problem(sim_pu_zones_polygons, sim_features_zones,
               cost_column = c("cost_1", "cost_2", "cost_3")) %>%
       add_min_set_objective() %>%
       add_absolute_targets(targets) %>%
       add_proportion_decisions()
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = integer(0),
                                                zone = character(0),
                                                status = integer(0)))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = "a", zone = "zone_1",
                                                status = 1))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = 1000, zone = "zone_1",
                                                status = 0.2))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = -5, zone = "zone_1",
                                                status = 0.2))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = NA_integer_,
                                                zone = "zone_1",
                                                status = 0.2))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = 1, zone = "zone_10",
                                                status = 0.2))
  })
  expect_error({
    add_manual_locked_constraints(p, data.frame(pu = 1, zone = NA_character_,
                                                status = 0.2))
  })
})
