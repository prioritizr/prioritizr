context("add_manual_bounded_constraints")

test_that("compile (sf, single zone)", {
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
  # calculations for tests
  locked_pos <- seq_len(5)
  other_pos <- seq(6, nrow(sim_pu_polygons))
  # tests
  expect_true(all(o$lb()[locked_pos] == 0.3))
  expect_true(all(o$ub()[locked_pos] == 0.35))
  expect_true(all(o$lb()[other_pos] == 0))
  expect_true(all(o$ub()[other_pos] == 1))
})

test_that("compile (raster, single zone)", {
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

test_that("solve (single zone)", {
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

test_that("compile (multiple zones)", {
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
    add_manual_bounded_constraints(
      data.frame(
        pu = c(seq_len(5), 20),
        zone = c(rep("zone_1", 5), "zone_2"),
        lower = 0.3,
        upper = 0.35
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
  expect_true(all(o$ub()[locked_pos] == 0.35))
  expect_true(all(o$lb()[other_pos] == 0))
  expect_true(all(o$ub()[other_pos] == 1))
})

test_that("solve (multiple zones)", {
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
    add_manual_bounded_constraints(
      data.frame(
        pu = c(seq_len(5), 20),
        zone = c(rep("zone_1", 5), "zone_2"),
        lower = 0.3,
        upper = 0.35
      )
    ) %>%
    add_default_solver(verbose = FALSE) %>%
    solve()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1_zone_1[seq_len(5)] >= 0.3))
  expect_true(all(s$solution_1_zone_1[seq_len(5)] <= 0.35))
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
