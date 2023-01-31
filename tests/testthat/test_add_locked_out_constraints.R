context("add_locked_out_constraints")

test_that("logical (compile, single zone)", {
  # import problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create data
  locked_out <- c(rep(TRUE, 20), rep(FALSE, terra::ncell(sim_pu_raster) - 20))
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(locked_out)
  o <- compile(p)
  # calculations for tests
  locked_out_cells <- which(locked_out)
  locked_out_indices <- match(
    locked_out_cells,
    terra::cells(is.na(sim_pu_raster)[[1]], 0)
  )
  locked_out_indices <- locked_out_indices[!is.na(locked_out_indices)]
  # tests
  expect_true(all(o$ub()[locked_out_indices] == 0))
  expect_true(all(o$ub()[-locked_out_indices] == 1))
  expect_tidy_error(p %>% add_locked_out_constraints(c(TRUE)))
  # tests for invalid inputs
  expect_tidy_error(
    p %>%
    add_locked_out_constraints(
      c(TRUE, NA_logical, rep(FALSE, terra::ncell(sim_pu_raster) - 2))
    )
  )
})

test_that("logical (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create locked out data
  locked_out <- c(rep(TRUE, 20), rep(FALSE, terra::ncell(sim_pu_raster) - 20))
  # create and solve problem
  s <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(locked_out) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve()
  # calculations for tests
  locked_out_cells <- which(locked_out)
  locked_out_units <- locked_out_cells[
    locked_out_cells %in% terra::cells(is.na(s), 0)[[1]]
  ]
  # tests
  expect_true(all(s[locked_out_units] == 0))
})

test_that("integer (compile, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(1:20)
  o <- compile(p)
  # calculations for tests
  locked_out_cells <- seq_len(20)
  locked_out_indices <- match(
    locked_out_cells,
    terra::cells(is.na(sim_pu_raster), 0)[[1]]
  )
  locked_out_indices <- locked_out_indices[!is.na(locked_out_indices)]
  # tests
  expect_true(all(o$ub()[locked_out_indices] == 0))
  expect_true(all(o$ub()[-locked_out_indices] == 1))
  expect_tidy_error(p %>% add_locked_out_constraints(-1))
  expect_tidy_error(p %>% add_locked_out_constraints(9.6))
  # tests for invalid inputs
  expect_tidy_error(
    p %>% add_locked_out_constraints(terra::ncell(sim_pu_raster) + 1)
  )
})

test_that("integer (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.05) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(1:20) %>%
    add_default_solver(gap = 0.01, verbose = FALSE)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # calculations for tests
  locked_out_cells <- seq_len(20)
  locked_out_units <- locked_out_cells[
    locked_out_cells %in% terra::cells(is.na(s1), 0)[[1]]
  ]
  # tests
  expect_true(all(as.matrix(s1[locked_out_units]) == 0))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("integer (compile, multiple zones)", {
  # load data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create locked data
  status <- matrix(
    FALSE, nrow = terra::ncell(sim_zones_pu_raster),
    ncol = number_of_zones(sim_zones_features)
  )
  locked_out_ind <- terra::cells(is.na(sim_zones_pu_raster[[1]]), 0)[[1]][1:20]
  status[locked_out_ind, 1] <- TRUE
  # create targets data
  targets <- matrix(
    0, nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(status)
  o <- compile(p)
  # calculations for tests
  other_ind <- seq(21, p$number_of_planning_units() * p$number_of_zones())
  # tests
  expect_true(all(o$ub()[1:20] == 0))
  expect_true(all(o$ub()[other_ind] == 1))
  expect_tidy_error({
    p %>% add_locked_in_constraints({s <- status; s[1, 1] <- 2; s})
  })
})

test_that("integer (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create locked data
  status <- matrix(
    FALSE, nrow = terra::ncell(sim_zones_pu_raster),
    ncol = number_of_zones(sim_zones_features)
  )
  locked_out_ind <- terra::cells(is.na(sim_zones_pu_raster[[1]]), 0)[[1]][1:20]
  status[locked_out_ind, 1] <- TRUE
  # create targets data
  targets <- matrix(
    0, nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  # create and solve problem
  s <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(status) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve()
  # tests
  expect_true(all(as.matrix(s[[1]][locked_out_ind]) == 0))
})

test_that("character (compile, single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints("locked_out")
  o <- compile(p)
  # tests
  expect_true(all(o$ub()[which(sim_pu_polygons$locked_out)] == 0))
  # tests for invalid inputs
  expect_tidy_error({
    sim_pu_polygons$locked_out <- as.integer(sim_pu_polygons$locked_out)
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints("locked_out")
  })
  expect_tidy_error({
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(NA_character_)
  })
  expect_tidy_error({
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints("column_name_that_doesnt_exist")
  })
  expect_tidy_error({
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
  # iomport data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  s <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints("locked_out") %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve()
  # tests
  expect_true(all(s$solution_1[which(sim_pu_polygons$locked_out)] == 0))
})

test_that("character (solve, proportion decisions, single zone", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create and solve problem
  s <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions() %>%
    add_locked_out_constraints("locked_out") %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve()
  # tests
  expect_true(all(s$solution_1[which(sim_pu_polygons$locked_out)] == 0))
})

test_that("character (compile, multiple zones)", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create locked data
  sim_zones_pu_polygons$locked_1 <- FALSE
  sim_zones_pu_polygons$locked_2 <- FALSE
  sim_zones_pu_polygons$locked_3 <- FALSE
  sim_zones_pu_polygons$locked_1[1:20] <- TRUE
  # create targets data
  targets <- matrix(
    0, nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(c("locked_1", "locked_2", "locked_3"))
  o <- compile(p)
  # calculations for tests
  locked_ind <- seq_len(20)
  other_ind <- seq(21, p$number_of_planning_units() * p$number_of_zones())
  # tests
  expect_true(all(o$ub()[locked_ind] == 0))
  expect_true(all(o$ub()[other_ind] == 1))
  # tests for invalid inputs
  expect_tidy_error({
    sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
    sim_zones_features <- get_sim_zones_features()
    sim_zones_pu_polygons$locked_1 <- FALSE
    sim_zones_pu_polygons$locked_2 <- FALSE
    sim_zones_pu_polygons$locked_3 <- FALSE
    sim_zones_pu_polygons$locked_1[1] <- 2
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(c("locked_1", "locked_2", "locked_3"))
  })
})

test_that("character (solve, multiple zones)", {
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
  # create locked data
  sim_zones_pu_polygons$locked_1 <- FALSE
  sim_zones_pu_polygons$locked_2 <- FALSE
  sim_zones_pu_polygons$locked_3 <- FALSE
  sim_zones_pu_polygons$locked_1[1:20] <- TRUE
  # create and solve problem
  s <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(c("locked_1", "locked_2", "locked_3")) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve()
  # tests
  expect_true(all(s$solution_1_zone_1[sim_zones_pu_polygons$locked_1] == 0))
})

test_that("character (solve, proportion decisions, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create targets data
  targets <- matrix(0, nrow = number_of_features(sim_zones_features),
                    ncol = number_of_zones(sim_zones_features))
  targets[, 1] <- 1
  # create locked data
  sim_zones_pu_polygons$locked_1 <- FALSE
  sim_zones_pu_polygons$locked_2 <- FALSE
  sim_zones_pu_polygons$locked_3 <- FALSE
  sim_zones_pu_polygons$locked_1[1:20] <- TRUE
  # create and solve problem
  s <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_proportion_decisions() %>%
    add_locked_out_constraints(c("locked_1", "locked_2", "locked_3")) %>%
    add_default_solver(verbose = FALSE) %>%
    solve()
  # tests
  expect_true(all(s$solution_1_zone_1[sim_zones_pu_polygons$locked_1] == 0))
})

test_that("raster (compile, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_locked_out_raster <- get_sim_locked_out_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(sim_locked_out_raster)
  o <- compile(p)
  # calculations for constraints
  locked_out_cells <- terra::cells(
    sim_locked_out_raster & !is.na(sim_pu_raster), 1
  )[[1]]
  locked_out_indices <- match(
    locked_out_cells,
    terra::cells(is.na(sim_pu_raster), 0)[[1]]
  )
  # tests
  expect_true(all(o$ub()[locked_out_indices] == 0))
  expect_true(all(o$ub()[-locked_out_indices] == 1))
  # tests for invalid inputs
  expect_tidy_error({
    d <- get_sim_locked_out_raster()
    terra::ext(d) <- c(0, 20, 0, 20)
    p %>% add_locked_out_constraints(d)
  })
  expect_tidy_error({
    d <- get_sim_locked_out_raster()
    terra::crs(d) <- as.character(
      sf::st_crs("+proj=longlat +datum=WGS84 +no_defs")
    )[[2]]
    p %>% add_locked_out_constraints(d)
  })
  expect_tidy_error({
    p  %>%
    add_locked_out_constraints(
      terra::setValues(get_sim_locked_out_raster(), NA)
    )
  })
  expect_tidy_error({
    p  %>%
    add_locked_out_constraints(
      terra::setValues(get_sim_locked_out_raster(), NA)
    )
  })
})

test_that("raster (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_locked_out_raster <- get_sim_locked_out_raster()
  sim_features <- get_sim_features()
  # create and solve problem
  s <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(sim_locked_out_raster) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve()
  # calculations for tests
  locked_out_cells <- terra::cells(
    sim_locked_out_raster & !is.na(sim_pu_raster), 1
  )[[1]]
  # tests
  expect_true(all(s[locked_out_cells] == 0))
})

test_that("raster (compile, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # calculate targets data
  targets <- matrix(
    0, nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  # calculate locked data
  status <- sim_zones_pu_raster
  status[[1]][!is.na(status[[1]])] <- 0
  status[[1]][terra::cells(is.na(status[[1]]), 0)[[1]][1:20]] <- 1
  status[[2]][!is.na(status[[2]])] <- 0
  status[[3]][!is.na(status[[3]])] <- 0
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(status)
  o <- compile(p)
  # calculations for tests
  locked_ind <- seq_len(20)
  other_ind <- seq(21, p$number_of_planning_units() * p$number_of_zones())
  # tests
  expect_true(all(o$ub()[locked_ind] == 0))
  expect_true(all(o$ub()[other_ind] == 1))
})

test_that("raster (solve, multiple zones)", {
  # create problem
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create targets data
  targets <- matrix(
    0, nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  # create locked data
  status <- sim_zones_pu_raster
  status[[1]][!is.na(status[[1]])] <- 0
  status[[1]][terra::cells(is.na(status[[1]]), 0)[[1]][1:20]] <- 1
  status[[2]][!is.na(status[[2]])] <- 0
  status[[3]][!is.na(status[[3]])] <- 0
  # create and solve problems
  s <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(status) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve()
  # calculations for tests
  locked_out_cells <- terra::cells(status[[1]] == 1, 1)[[1]]
  # tests
  expect_true(all(s[[1]][locked_out_cells] == 0))
})

test_that("spatial (compile, single zone)", {
  # import data
  sim_pu_polygons <- sf::as_Spatial(get_sim_pu_polygons())
  sim_features <- raster::stack(get_sim_features())
  # create problem
  expect_warning(
    p <-
      problem(sim_pu_polygons, sim_features, "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_locked_out_constraints(sim_pu_polygons[sim_pu_polygons$locked_out, ]),
    "deprecated"
  )
  o <- compile(p)
  # calculations for tests
  locked_out_units <- which(sim_pu_polygons$locked_out)
  # tests
  expect_true(all(o$ub()[locked_out_units] == 0))
  expect_true(all(o$ub()[-locked_out_units] == 1))
  # tests for invalid inputs
  expect_tidy_error({
    sim_pu_polygons <- sf::as_Spatial(get_sim_pu_polygons())
    sim_features <- raster::stack(get_sim_features())
    problem(sim_pu_polygons[1:10, ], sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(sim_pu_polygons[50:55, ])
  })
  expect_tidy_error({
    sim_pu_polygons <- sf::as_Spatial(get_sim_pu_polygons())
    sim_features <- raster::stack(get_sim_features())
    problem(sim_pu_polygons[1:10, ], sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_out_constraints(sim_pu_polygons[0, ])
  })
  expect_tidy_error({
    sim_pu_polygons <-sf::as_Spatial(get_sim_pu_polygons())
    sim_features <- raster::stack(get_sim_features())
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
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create locked data
  locked_ply <- sim_pu_polygons[sim_pu_polygons$locked_out, ]
  # create and solve problem
  s <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(locked_ply) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve()
  # calculations for tests
  locked_out_units <- which(sim_pu_polygons$locked_out)
  # tests
  expect_true(all(s$solution_1[locked_out_units] == 0))
})

test_that("spatial (compile, multiple zones, expect error)", {
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create targets data
  targets <- matrix(
    0, nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  # tests
  expect_tidy_error({
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_out_constraints(sim_zones_pu_polygons[seq_len(20), ]) %>%
    add_default_solver(verbose = FALSE) %>%
    solve()
  })
})
