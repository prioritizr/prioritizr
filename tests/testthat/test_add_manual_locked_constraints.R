test_that("data.frame (compile, single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions() %>%
    add_manual_locked_constraints(
      data.frame(pu = seq_len(5), status = rep(0.3, 10))
    )
  o <- compile(p)
  suppressMessages(print(p))
  suppressMessages(summary(p))
  # calculations for tests
  locked_pos <- seq_len(5)
  other_pos <- seq(6, nrow(sim_pu_polygons))
  # tests
  expect_true(all(o$lb()[locked_pos] == 0.3))
  expect_true(all(o$ub()[locked_pos] == 0.3))
  expect_true(all(o$lb()[other_pos] == 0))
  expect_true(all(o$ub()[other_pos] == 1))
})

test_that("data.frame (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create and solve problem
  p <-
    problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions() %>%
    add_manual_locked_constraints(
      data.frame(pu = seq_len(5), status = rep(0.3, 10))
    ) %>%
    add_default_solver(verbose = FALSE)
  s1 <- solve_fixed_seed(p)
  s2 <- solve_fixed_seed(p)
  # tests
  expect_true(all(s1$solution_1[seq_len(5)] == 0.3))
  expect_equal(s1$solution_1, s2$solution_1)
})

test_that("data.frame (compile, multiple zones (factor))", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create targets data
  targets <- matrix(
    0, nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  # create locked data
  sim_zones_pu_polygons$locked_1 <- TRUE
  sim_zones_pu_polygons$locked_2 <- FALSE
  sim_zones_pu_polygons$locked_3 <- FALSE
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_proportion_decisions() %>%
    add_manual_locked_constraints(
      data.frame(
        pu = c(seq_len(5), 20),
        zone = c(rep("zone_1", 5), "zone_2"),
        status = 0.3,
        stringsAsFactors = TRUE
      )
    )
  o <- compile(p)
  # calculations for tests
  locked_pos <- c(seq_len(5), nrow(sim_zones_pu_polygons) + 20)
  other_pos <- setdiff(
    seq_len(p$number_of_planning_units() * p$number_of_zones()), locked_pos
  )
  # tests
  expect_true(all(o$lb()[locked_pos] == 0.3))
  expect_true(all(o$ub()[locked_pos] == 0.3))
  expect_true(all(o$lb()[other_pos] == 0))
  expect_true(all(o$ub()[other_pos] == 1))
})

test_that("data.frame (compile, multiple zones (character))", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create targets data
  targets <- matrix(
    0, nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  # create locked data
  sim_zones_pu_polygons$locked_1 <- TRUE
  sim_zones_pu_polygons$locked_2 <- FALSE
  sim_zones_pu_polygons$locked_3 <- FALSE
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_proportion_decisions() %>%
    add_manual_locked_constraints(
      data.frame(
        pu = c(seq_len(5), 20),
        zone = c(rep("zone_1", 5), "zone_2"),
        status = 0.3,
        stringsAsFactors = TRUE
      )
    )
  o <- compile(p)
  # calculations for tests
  locked_pos <- c(seq_len(5), nrow(sim_zones_pu_polygons) + 20)
  other_pos <- setdiff(
    seq_len(p$number_of_planning_units() * p$number_of_zones()), locked_pos
  )
  # tests
  expect_true(all(o$lb()[locked_pos] == 0.3))
  expect_true(all(o$ub()[locked_pos] == 0.3))
  expect_true(all(o$lb()[other_pos] == 0))
  expect_true(all(o$ub()[other_pos] == 1))
})

test_that("data.frame (data.frame pu data, compile, single zone)", {
  # import data
  sim_pu_data <- get_sim_pu_polygons()
  sim_pu_data <- sf::st_drop_geometry(sim_pu_data)[1:5, , drop = FALSE]
  sim_pu_data$cost <- c(1, NA, 3, 4, 8)
  sim_pu_data$spp_1 <- runif(5)
  sim_pu_data$spp_2 <- runif(5)
  sim_pu_data$spp_3 <- runif(5)
  # create problem
  p <-
    problem(sim_pu_data, c("spp_1", "spp_2", "spp_3"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions() %>%
    add_manual_locked_constraints(
      data.frame(pu = c(1, 3, 5), status = rep(0.3, 3))
    )
  o <- compile(p)
  # calculations for tests
  locked_pos <- c(1, 2, 4)
  other_pos <- c(3)
  # tests
  expect_true(all(o$lb()[locked_pos] == 0.3))
  expect_true(all(o$ub()[locked_pos] == 0.3))
  expect_true(all(o$lb()[other_pos] == 0))
  expect_true(all(o$ub()[other_pos] == 1))
})

test_that("data.frame (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create targets data
  targets <- matrix(
    0, nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[] <- 0
  targets[, 1] <- 1
  # create and solve problem
  s <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_proportion_decisions() %>%
    add_manual_locked_constraints(
      data.frame(
        pu = c(seq_len(5), 20),
        zone = c(rep("zone_1", 5), "zone_2"),
        status = 0.3)
      ) %>%
    add_default_solver(verbose = FALSE) %>%
    solve_fixed_seed()
  # tests
  expect_true(all(s$solution_1_zone_1[seq_len(5)] == 0.3))
  expect_true(all(s$solution_1_zone_2[20] == 0.3))
})

test_that("invalid inputs (single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(1) %>%
    add_proportion_decisions()
  # tests
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = integer(0), status = integer(0))
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = "a", status = 1)
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = 1000, status = 0.2)
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = -5, status = 0.2)
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = NA_integer_, status = 0.2)
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = 1, status = NA_real_)
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = 1, zone = "zone_10", status = 1)
    )
  })
})

test_that("invalid inputs (multiple zones)", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create targets data
  targets <- matrix(
    0, nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[] <- 0
  targets[, 1] <- 1
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      cost_column = c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_proportion_decisions()
  # tests
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(
        pu = integer(0),
        zone = character(0),
        status = integer(0)
      )
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = "a", zone = "zone_1", status = 1)
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = 1000, zone = "zone_1", status = 0.2)
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = -5, zone = "zone_1", status = 0.2)
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = NA_integer_, zone = "zone_1", status = 0.2)
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = 1, zone = "zone_10", status = 0.2)
    )
  })
  expect_tidy_error({
    add_manual_locked_constraints(
      p,
      data.frame(pu = 1, zone = NA_character_, status = 0.2)
    )
  })
})
