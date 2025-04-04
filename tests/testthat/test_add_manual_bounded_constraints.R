test_that("sf (compile, single zone)", {
  # import problem
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions() %>%
    add_manual_bounded_constraints(
      data.frame(pu = seq_len(5), lower = rep(0.3, 10), upper = rep(0.35, 10))
    )
  o <- compile(p)
  suppressMessages(print(p))
  suppressMessages(summary(p))
  # calculations for tests
  locked_pos <- seq_len(5)
  other_pos <- seq(6, nrow(sim_pu_polygons))
  # tests
  expect_true(all(o$lb()[locked_pos] == 0.3))
  expect_true(all(o$ub()[locked_pos] == 0.35))
  expect_true(all(o$lb()[other_pos] == 0))
  expect_true(all(o$ub()[other_pos] == 1))
})

test_that("raster (solve, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions() %>%
    add_manual_bounded_constraints(
      data.frame(pu = seq_len(5), lower = rep(0.3, 10), upper = rep(0.35, 10))
    )
  o <- compile(p)
  # calculations for tests
  locked_pos <- seq_len(5)
  other_pos <- seq(
    6,
    terra::global(!is.na(sim_pu_raster), "sum", na.rm = TRUE)[[1]]
  )
  # tests
  expect_true(all(o$lb()[locked_pos] == 0.3))
  expect_true(all(o$ub()[locked_pos] == 0.35))
  expect_true(all(o$lb()[other_pos] == 0))
  expect_true(all(o$ub()[other_pos] == 1))
})

test_that("sf (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions() %>%
    add_manual_bounded_constraints(
     data.frame(pu = seq_len(5), lower = rep(0.3, 10), upper = rep(0.35, 10))
    ) %>%
    add_default_solver(verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # tests
  expect_true(all(s1$solution_1[seq_len(5)] >= 0.3))
  expect_true(all(s1$solution_1[seq_len(5)] <= 0.35))
  expect_equal(s1$solution_1, s2$solution_1)
})

test_that("data.frame (compile, single zone)", {
  # import data
  sim_pu_data <- get_sim_pu_polygons()
  sim_pu_data <- sf::st_drop_geometry(sim_pu_data)[1:5, , drop = FALSE]
  sim_pu_data$id <- c(1, 3, 90, 5, 2)
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
    add_manual_bounded_constraints(
      data.frame(
        pu = c(1, 3, 2),
        lower = c(0.1, 0.2, 0.3),
        upper = c(0.5, 0.6, 0.7)
      )
    )
  o <- compile(p)
  # tests
  expect_equal(o$lb(), c(0.1, 0, 0, 0.3))
  expect_equal(o$ub(), c(0.5, 1, 1, 0.7))
})

test_that("data.frame (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_data <- get_sim_pu_polygons()
  sim_pu_data <- sf::st_drop_geometry(sim_pu_data)[1:5, , drop = FALSE]
  sim_pu_data$id <- c(1, 3, 90, 5, 2)
  sim_pu_data$cost <- c(1, NA, 3, 4, 8)
  sim_pu_data$spp_1 <- runif(5)
  sim_pu_data$spp_2 <- runif(5)
  sim_pu_data$spp_3 <- runif(5)
  # create problem
  s <-
    problem(sim_pu_data, c("spp_1", "spp_2", "spp_3"), cost_column = "cost") %>%
    add_max_utility_objective(budget = 1e5) %>%
    add_proportion_decisions() %>%
    add_manual_bounded_constraints(
      data.frame(
        pu = c(1, 3, 5, 2),
        lower = c(0.1, 0.2, 0.3, 0.4),
        upper = c(0.15, 0.25, 0.35, 0.45)
      )
    ) %>%
    add_default_solver(verbose = FALSE) %>%
    solve(run_checks = FALSE)
  # tests
  expect_equal(s$solution_1, c(0.15, NA, 1, 0.35, 0.45), tolerance = 1e-6)
})

test_that("sf (compile, multiple zones)", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_max_utility_objective(budget = 1e5) %>%
    add_proportion_decisions() %>%
    add_manual_bounded_constraints(
      data.frame(
        pu = c(seq_len(5), 20),
        zone = c(rep("zone_1", 5), "zone_2"),
        lower = c(rep(0.3, 5), 0.7),
        upper = c(rep(0.4, 5), 0.8),
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
  expect_equal(o$lb()[locked_pos], c(rep(0.3, 5), 0.7))
  expect_equal(o$ub()[locked_pos], c(rep(0.4, 5), 0.8))
  expect_true(all(o$lb()[other_pos] == 0))
  expect_true(all(o$ub()[other_pos] == 1))
})

test_that("sf (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create and solve problem
  s <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_max_utility_objective(budget = 1e5) %>%
    add_proportion_decisions() %>%
    add_manual_bounded_constraints(
      data.frame(
        pu = c(seq_len(5), 20),
        zone = c(rep("zone_1", 5), "zone_2"),
        lower = c(rep(0.3, 5), 0.7),
        upper = c(rep(0.35, 5), 0.75),
        stringsAsFactors = TRUE
      )
    ) %>%
    add_default_solver(verbose = FALSE) %>%
    solve(run_checks = FALSE)
  # tests
  expect_true(all(s$solution_1_zone_1[seq_len(5)] >= 0.3))
  expect_true(all(s$solution_1_zone_1[seq_len(5)] <= 0.35))
  expect_true(all(s$solution_1_zone_2[20] >= 0.7))
  expect_true(all(s$solution_1_zone_2[20] <= 0.75))
})

test_that("data.frame (compile, multiple zones)", {
  # import data
  sim_zones_pu_data <- get_sim_zones_pu_polygons()
  sim_zones_pu_data <- sf::st_drop_geometry(sim_zones_pu_data[1:5, ])
  sim_zones_pu_data$id <- c(2, 4, 90, 1, 10)
  sim_zones_pu_data$cost_1[4] <- NA_real_
  sim_zones_pu_data$cost_1[2] <- NA_real_
  sim_zones_pu_data$cost_2[2] <- NA_real_
  sim_zones_pu_data$cost_3[2] <- NA_real_
  sim_zones_pu_data$spp_1_zone_1 <- 1
  sim_zones_pu_data$spp_1_zone_2 <- 2
  sim_zones_pu_data$spp_1_zone_3 <- 3
  sim_zones_features <- get_sim_zones_features()
  # create problem
  p <-
    problem(
      sim_zones_pu_data,
      cost_column = c("cost_1", "cost_2", "cost_3"),
      zones(
        zone_1 = "spp_1_zone_1",
        zone_2 = "spp_1_zone_2",
        zone_3 = "spp_1_zone_3"
      )
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(1, ncol = 3)) %>%
    add_proportion_decisions() %>%
    add_manual_bounded_constraints(
      data.frame(
        pu = c(10, 4, 1),
        zone = c("zone_1", "zone_2", "zone_3"),
        lower = c(0.1, 0.2, 0.3),
        upper = c(0.15, 0.25, 0.35),
        stringsAsFactors = TRUE
      )
    )
  o <- compile(p)
  # tests
  expect_equal(
    o$lb(),
    c(
      0, 0, 0, 0.1, # zone 1
      0, 0, 0, 0,   # zone 2
      0, 0, 0.3, 0  # zone 3
    )
  )
  expect_equal(
    o$ub(),
    c(
      1, 1, 0, 0.15, # zone 1
      1, 1, 1, 1,   # zone 2
      1, 1, 0.35, 1  # zone 3
    )
  )
})

test_that("data.frame (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_zones_pu_data <- get_sim_zones_pu_polygons()
  sim_zones_pu_data <- sf::st_drop_geometry(sim_zones_pu_data[1:5, ])
  sim_zones_pu_data$id <- c(2, 4, 90, 1, 10)
  sim_zones_pu_data$cost_1[4] <- NA_real_
  sim_zones_pu_data$cost_1[2] <- NA_real_
  sim_zones_pu_data$cost_2[2] <- NA_real_
  sim_zones_pu_data$cost_3[2] <- NA_real_
  sim_zones_pu_data$spp_1_zone_1 <- 1
  sim_zones_pu_data$spp_1_zone_2 <- 2
  sim_zones_pu_data$spp_1_zone_3 <- 3
  sim_zones_features <- get_sim_zones_features()
  # create and solve problem
  s <-
    problem(
      sim_zones_pu_data,
      cost_column = c("cost_1", "cost_2", "cost_3"),
      zones(
        zone_1 = "spp_1_zone_1",
        zone_2 = "spp_1_zone_2",
        zone_3 = "spp_1_zone_3"
      )
    ) %>%
    add_max_utility_objective(budget = 1e5) %>%
    add_proportion_decisions() %>%
    add_manual_bounded_constraints(
      data.frame(
        pu = c(10, 4, 1),
        zone = c("zone_1", "zone_2", "zone_3"),
        lower = c(0.1, 0.2, 0.3),
        upper = c(0.15, 0.25, 0.35),
        stringsAsFactors = TRUE
      )
    ) %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    solve(run_checks = FALSE)
  # tests
  expect_equal(s$solution_1_zone_1, c(0, NA, 0, NA, 0.1), tolerance = 1e-5)
  expect_equal(s$solution_1_zone_2, c(0, NA, 0, 0.65, 0), tolerance = 1e-5)
  expect_equal(s$solution_1_zone_3, c(1, NA, 1, 0.35, 0.9), tolerance = 1e-5)
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
    add_manual_bounded_constraints(
      p,
      data.frame(pu = integer(0), lower = integer(0), upper = integer(0))
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = "a", lower = 1, upper = 1)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = 1000, lower = 0.2, upper = 1)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = -5, lower = 0.2, upper = 1)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = NA_integer_, lower = 0.2, upper = 1)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = 1, lower = NA_real_, upper = 1)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = 1, lower = 0, upper = NA_real_)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = 1, lower = 0.7, upper = 0)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = 1, zone = "zone_10", lower = 0, upper = 1)
    )
  })
})

test_that("invalid inputs (multiple zones)", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # crate targets data
  targets <- matrix(0, nrow = number_of_features(sim_zones_features),
                    ncol = number_of_zones(sim_zones_features))
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
    add_manual_bounded_constraints(
      p,
      data.frame(pu = integer(0), lower = integer(0), upper = integer(0))
    )
  })
  expect_tidy_error({
     add_manual_bounded_constraints(
       p,
       data.frame(pu = "a", lower = 1, upper = 1)
     )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = 1000, lower = 0.2, upper = 1)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = -5, lower = 0.2, upper = 1)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = NA_integer_, lower = 0.2, upper = 1)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = 1, lower = NA_real_, upper = 1)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = 1, lower = 0, upper = NA_real_)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = 1, lower = 0.7, upper = 0)
    )
  })
  expect_tidy_error({
    add_manual_bounded_constraints(
      p,
      data.frame(pu = 1, zone = "zone_10", lower = 0, upper = 1)
    )
  })
})
