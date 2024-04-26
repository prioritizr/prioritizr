test_that("integer (compile, single zone)", {
  # create problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(seq_len(terra::ncell(sim_pu_raster)))
  suppressWarnings(o <- compile(p))
  # check that constraints added correctly
  expect_true(isTRUE(all(o$lb()[seq_len(p$number_of_planning_units())] == 1)))
  # invalid inputs
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  expect_tidy_error(add_locked_in_constraints(p, -1))
  expect_tidy_error(add_locked_in_constraints(p, 9.6))
  expect_tidy_error(add_locked_in_constraints(terra::ncell(sim_pu_raster) + 1))
})

test_that("integer (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  suppressWarnings({
    p <-
      problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_locked_in_constraints(seq_len(terra::ncell(sim_pu_raster))) %>%
      add_default_solver(time_limit = 5, verbose = FALSE)
    expect_warning(s1 <- solve_fixed_seed(p, force = TRUE))
    expect_warning(s2 <- solve_fixed_seed(p, force = TRUE))
  })
  # check that the solution obeys constraints as expected
  expect_equal(
    terra::cells(is.na(s1), 0)[[1]],
    terra::cells(is.na(sim_pu_raster), 0)[[1]]
  )
  expect_true(isTRUE(all(s1[!is.na(s1)][, 1] == 1)))
  expect_equal(terra::values(s1), terra::values(s2))
})

test_that("logical (compile, single zone)", {
  # create problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(rep(TRUE, terra::ncell(sim_pu_raster)))
  suppressWarnings(o <- compile(p))
  # check that constraints added correctly
  expect_true(isTRUE(all(o$lb()[seq_len(p$number_of_planning_units())] == 1)))
  # invalid inputs
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  expect_tidy_error(add_locked_in_constraints(p, c(TRUE)))
  expect_tidy_error(
    add_locked_in_constraints(
      p,
      c(TRUE, NA, rep(FALSE, terra::ncell(sim_pu_raster) - 2))
    )
  )
})

test_that("logical (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  suppressWarnings({
    s <-
      problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_locked_in_constraints(rep(TRUE, terra::ncell(sim_pu_raster))) %>%
      add_default_solver(time_limit = 5, verbose = FALSE) %>%
      solve_fixed_seed(force = TRUE)
  })
  # check that the solution obeys constraints as expected
  expect_equal(
    terra::cells(is.na(s), 0)[[1]],
    terra::cells(is.na(sim_pu_raster), 0)[[1]]
  )
  expect_true(isTRUE(all(s[!is.na(s)][, 1] == 1)))
})

test_that("matrix (compile, multiple zones)", {
  # create problem
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  status <- matrix(
    FALSE,
    nrow = terra::ncell(sim_zones_pu_raster),
    ncol = number_of_zones(sim_zones_features)
  )
  status[, 1] <- TRUE
  targets <- matrix(
    0,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(status)
  o <- compile(p)
  # check that constraints added correctly
  first_zone_ind <- seq_len(p$number_of_planning_units())
  other_zone_ind <-
    p$number_of_planning_units() +
    seq_len(p$number_of_planning_units() * 2)
  expect_true(isTRUE(all(o$lb()[first_zone_ind] == 1)))
  expect_true(isTRUE(all(o$lb()[other_zone_ind] == 0)))
  # invalid inputs
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_binary_decisions()
  expect_tidy_error(
    add_locked_in_constraints(p, {s <- status; s[1, ] <- TRUE; s})
  )
  expect_tidy_error(
    add_locked_in_constraints(p, {s <- status; s[1, 1] <- 2; s})
  )
})

test_that("matrix (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  status <- matrix(
    FALSE,
    nrow = terra::ncell(sim_zones_pu_raster),
    ncol = number_of_zones(sim_zones_features)
  )
  status[, 1] <- TRUE
  targets <- matrix(
    0,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[] <- 0
  targets[, 1] <- 1
  s <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(status) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve_fixed_seed()
  # check that the solution obeys constraints as expected
  for (i in seq_len(terra::nlyr(sim_zones_pu_raster)))
    expect_equal(
      terra::cells(is.na(s[[i]]), 0)[[1]],
      terra::cells(is.na(sim_zones_pu_raster[[i]]), 0)[[1]]
    )
  for (i in seq_len(terra::nlyr(sim_zones_pu_raster)))
    expect_true(
      all(s[[i]][!is.na(s[[i]])] == as.numeric(i == 1))
    )
})

test_that("character (compile, sf, single zone)", {
  # create problem
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints("locked_in")
  o <- compile(p)
  # check that constraints added correctly
  expect_true(isTRUE(all(o$lb()[which(sim_pu_polygons$locked_in)] == 1)))
  # invalid inputs
  expect_tidy_error({
    sim_pu_polygons$locked_in <- as.integer(sim_pu_polygons$locked_in)
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints("locked_in")
  })
  expect_tidy_error({
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints(NA_character_)
  })
  expect_tidy_error({
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints("column_name_that_doesnt_exist")
  })
  expect_tidy_error({
    problem(sim_pu_polygons, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints("cost")
  })
})

test_that("character (solve, sf, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  s <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints("locked_in") %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve_fixed_seed()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1[which(sim_pu_polygons$locked_in)] == 1))
})

test_that("character (compile, sf, multiple zones)", {
  # create problem
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  targets <- matrix(
    0,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  sim_zones_pu_polygons$locked_1 <- TRUE
  sim_zones_pu_polygons$locked_2 <- FALSE
  sim_zones_pu_polygons$locked_3 <- FALSE
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(c("locked_1", "locked_2", "locked_3"))
  o <- compile(p)
  # check that constraints added correctly
  first_zone_ind <- seq_len(p$number_of_planning_units())
  other_zone_ind <-
    p$number_of_planning_units() +
    seq_len(p$number_of_planning_units() * 2)
  expect_true(isTRUE(all(o$lb()[first_zone_ind] == 1)))
  expect_true(isTRUE(all(o$lb()[other_zone_ind] == 0)))
  # invalid inputs
  expect_tidy_error({
    sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
    sim_zones_features <- get_sim_zones_features()
    sim_zones_pu_polygons$locked_1 <- TRUE
    sim_zones_pu_polygons$locked_2 <- c(
      TRUE, rep(FALSE, nrow(sim_zones_pu_polygons) - 1)
    )
    sim_zones_pu_polygons$locked_3 <- FALSE
    p <-
      problem(
        sim_zones_pu_polygons, sim_zones_features,
         c("cost_1", "cost_2", "cost_3")
      ) %>%
      add_min_set_objective() %>%
      add_relative_targets(targets) %>%
      add_binary_decisions() %>%
      add_locked_in_constraints(c("locked_1", "locked_2", "locked_3"))
  })
  expect_tidy_error({
    sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
    sim_zones_features <- get_sim_zones_features()
    sim_zones_pu_polygons$locked_1 <- 1
    sim_zones_pu_polygons$locked_2 <- FALSE
    sim_zones_pu_polygons$locked_3 <- FALSE
    p <-
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

test_that("character (solve, sf, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  targets <- matrix(
    0,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  sim_zones_pu_polygons$locked_1 <- TRUE
  sim_zones_pu_polygons$locked_2 <- FALSE
  sim_zones_pu_polygons$locked_3 <- FALSE
  s <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(c("locked_1", "locked_2", "locked_3")) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve_fixed_seed()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1_zone_1 == 1))
  expect_true(all(s$solution_1_zone_2 == 0))
  expect_true(all(s$solution_1_zone_3 == 0))
})

test_that("character (solve, single zone, proportion decisions)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  s <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_proportion_decisions() %>%
    add_locked_in_constraints("locked_in") %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve_fixed_seed()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1[which(sim_pu_polygons$locked_in)] == 1))
})

test_that("character (solve, multiple zones, proportion decisions)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  targets <- matrix(FALSE, nrow = number_of_features(sim_zones_features),
                    ncol = number_of_zones(sim_zones_features))
  targets[] <- 0
  targets[, 1] <- 1
  sim_zones_pu_polygons$locked_1 <- TRUE
  sim_zones_pu_polygons$locked_2 <- FALSE
  sim_zones_pu_polygons$locked_3 <- FALSE
  s <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_proportion_decisions() %>%
    add_locked_in_constraints(c("locked_1", "locked_2", "locked_3")) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve_fixed_seed()
  # check that the solution obeys constraints as expected
  expect_true(all(s$solution_1_zone_1 == 1))
  expect_true(all(s$solution_1_zone_2 == 0))
  expect_true(all(s$solution_1_zone_3 == 0))
})

test_that("raster (compile, single zone)", {
  # create problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_locked_in_raster <- get_sim_locked_in_raster()
  sim_features <- get_sim_features()
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(sim_locked_in_raster)
  o <- compile(p)
  # check that constraints added correctly
  locked_in_cells <-
    terra::cells(sim_locked_in_raster & !is.na(sim_pu_raster), 1)[[1]]
  locked_in_indices <- match(
    locked_in_cells, terra::cells(is.na(sim_pu_raster), 0)[[1]]
  )
  expect_true(isTRUE(all(o$lb()[locked_in_indices] == 1)))
  expect_true(isTRUE(all(o$lb()[-locked_in_indices] == 0)))
  # check that invalid inputs throw errors
  expect_tidy_error({
    sim_locked_in_raster <- get_sim_locked_in_raster()
    terra::ext(sim_locked_in_raster) <- c(0, 20, 0, 20)
    problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions()  %>%
      add_locked_in_constraints(sim_locked_in_raster)
  })
  expect_tidy_error({
    sim_locked_in_raster <- get_sim_locked_in_raster()
    terra::crs(sim_locked_in_raster) <- "+proj=longlat +datum=WGS84 +no_defs"
    problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions()  %>%
      add_locked_in_constraints(sim_locked_in_raster)
  })
  expect_tidy_error({
    sim_locked_in_raster <- get_sim_locked_in_raster()
    sim_locked_in_raster <- terra::setValues(sim_locked_in_raster, NA)
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints(sim_locked_in_raster)
  })
  expect_tidy_error({
    sim_locked_in_raster <- get_sim_locked_in_raster()
    sim_locked_in_raster <- terra::setValues(sim_locked_in_raster, 0)
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()  %>%
    add_locked_in_constraints(sim_locked_in_raster)
  })
})

test_that("raster (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_locked_in_raster <- get_sim_locked_in_raster()
  sim_features <- get_sim_features()
  s <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(sim_locked_in_raster) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve_fixed_seed()
  # check that the solution obeys constraints
  locked_in_cells <-
    terra::cells(sim_locked_in_raster & !is.na(sim_pu_raster), 1)[[1]]
  expect_true(all(s[locked_in_cells][, 1] == 1))
})

test_that("raster (compile, multiple zones)", {
  # create problem
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  targets <- matrix(
    0,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  status <- sim_zones_pu_raster
  status[[1]][!is.na(status[[1]])] <- 1
  status[[2]][!is.na(status[[2]])] <- 0
  status[[3]][!is.na(status[[3]])] <- 0
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(status)
  o <- compile(p)
  # check that constraints added correctly
  first_zone_ind <- seq_len(p$number_of_planning_units())
  other_zone_ind <-
    p$number_of_planning_units() +
    seq_len(p$number_of_planning_units() * 2)
  expect_true(isTRUE(all(o$lb()[first_zone_ind] == 1)))
  expect_true(isTRUE(all(o$lb()[other_zone_ind] == 0)))
  # invalid inputs
  expect_tidy_error({
    sim_zones_pu_raster <- get_sim_zones_pu_raster()
    sim_zones_features <- get_sim_zones_features()
    status <- sim_zones_pu_raster
    status[[1]][!is.na(status[[1]])] <- 1
    status[[2]][!is.na(status[[2]])] <- 0
    status[[2]][1] <- 1
    status[[3]][!is.na(status[[3]])] <- 0
    problem(sim_zones_pu_raster, sim_zones_features) %>%
      add_min_set_objective() %>%
      add_absolute_targets(targets) %>%
      add_binary_decisions() %>%
      add_locked_in_constraints(status)
  })
})

test_that("raster (solve, multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  targets <- matrix(
    0,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  status <- sim_zones_pu_raster
  status[[1]][!is.na(status[[1]])] <- 1
  status[[2]][!is.na(status[[2]])] <- 0
  status[[3]][!is.na(status[[3]])] <- 0
  s <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(status) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve_fixed_seed()
  # check that the solution obeys constraints as expected
  for (i in seq_len(terra::nlyr(sim_zones_pu_raster)))
    expect_equal(
      terra::cells(is.na(s[[i]]), 0)[[1]],
      terra::cells(is.na(sim_zones_pu_raster[[i]]), 0)[[1]]
    )
  for (i in seq_len(terra::nlyr(sim_zones_pu_raster)))
    expect_true(
      all(s[[i]][!is.na(s[[i]])][, 1] == as.numeric(i == 1))
    )
})

test_that("sf (compile, single zone)", {
  # create problem
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
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
  expect_tidy_error({
    sim_pu_polygons <- get_sim_pu_polygons()
    sim_features <- get_sim_features()
    problem(sim_pu_polygons[1:10, ], sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_locked_in_constraints(sim_pu_polygons[50:55, ])
  })
  expect_tidy_error({
    sim_pu_polygons <- get_sim_pu_polygons()
    sim_features <- get_sim_features()
    problem(sim_pu_polygons[1:10, ], sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions()  %>%
      add_locked_in_constraints(sim_pu_polygons[0, ])
  })
  expect_tidy_error({
    sim_pu_polygons <- get_sim_pu_polygons()
    sim_features <- get_sim_features()
    sim_pu_polygons2 <- sim_pu_polygons[1:10, ]
    suppressWarnings(sf::st_crs(sim_pu_polygons2) <- sf::st_crs(3857))
    problem(sim_pu_polygons, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions()  %>%
      add_locked_in_constraints(sim_pu_polygons2)
  })
})

test_that("sf (solve, single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create problem
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  s <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(
      sim_pu_polygons[sim_pu_polygons$locked_in, ]
    ) %>%
    add_default_solver(time_limit = 5, verbose = FALSE) %>%
    solve_fixed_seed()
  locked_in_units <- which(sim_pu_polygons$locked_in)
  expect_true(all(s$solution_1[locked_in_units] == 1))
})

test_that("sf (compile, multiple zones)", {
  expect_tidy_error({
    sim_zones_pu_raster <- get_sim_zones_pu_raster()
    sim_zones_features <- get_sim_zones_features()
    sim_pu_polygons <- get_sim_pu_polygons()
    targets <- matrix(
      0,
      nrow = number_of_features(sim_zones_features),
      ncol = number_of_zones(sim_zones_features)
    )
    targets[, 1] <- 1
    problem(sim_zones_pu_raster, sim_zones_features) %>%
      add_min_set_objective() %>%
      add_absolute_targets(targets) %>%
      add_binary_decisions() %>%
      add_locked_in_constraints(sim_pu_polygons[1:5, ]) %>%
      add_default_solver(time_limit = 5, verbose = FALSE) %>%
      solve_fixed_seed()
  })
})

test_that("deprecated Spatial (compile, single zone)", {
  # create problem
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # make problems
  p1 <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(sim_pu_polygons[1:5, ])
  expect_warning(
    p2 <-
      problem(sim_pu_polygons, sim_features, "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_locked_in_constraints(sf::as_Spatial(sim_pu_polygons[1:5, ]))
  )
  # compile problems
  o1 <- as.list(compile(p1))
  o2 <- as.list(compile(p2))
  # tests
  expect_equal(o1, o2)
})

test_that(
  "character (compile, deprecated Spatial, multiple zones)", {
  # create problem
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  targets <- matrix(
    0,
    nrow = number_of_features(sim_zones_features),
    ncol = number_of_zones(sim_zones_features)
  )
  targets[, 1] <- 1
  sim_zones_pu_polygons$locked_1 <- TRUE
  sim_zones_pu_polygons$locked_2 <- FALSE
  sim_zones_pu_polygons$locked_3 <- FALSE
  sim_spatial <- sf::as_Spatial(sim_zones_pu_polygons)
  # make problems
  p1 <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(c("locked_1", "locked_2", "locked_3"))
  expect_warning(
    expect_warning(
      p2 <-
        problem(
          sim_spatial, as.ZonesRaster(sim_zones_features),
          c("cost_1", "cost_2", "cost_3")
        ) %>%
        add_min_set_objective() %>%
        add_relative_targets(targets) %>%
        add_binary_decisions() %>%
        add_locked_in_constraints(c("locked_1", "locked_2", "locked_3")),
      "deprecated"
    ),
    "deprecated"
  )
  # compile problems
  o1 <- as.list(compile(p1))
  o2 <- as.list(compile(p2))
  # tests
  expect_equal(o1, o2)
})

test_that("raster (compile, deprecated Raster, single zone)", {
  # create problem
  sim_pu_raster <- get_sim_pu_raster()
  sim_locked_in_raster <- get_sim_locked_in_raster()
  sim_features <- get_sim_features()
  # make problems
  p1 <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(sim_locked_in_raster)
  expect_warning(
    p2 <-
      problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions() %>%
      add_locked_in_constraints(raster::raster(sim_locked_in_raster))
  )
  # compile problems
  o1 <- as.list(compile(p1))
  o2 <- as.list(compile(p2))
  # tests
  expect_equal(o1, o2)
})

test_that("invalid inputs", {
  # create problem
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  sim_spatial <- sf::st_as_sf(sim_pu_polygons)
  sim_spatial$locked_in <- FALSE
  # make problems
  p <- problem(sim_spatial, sim_features, "cost")
  # tests
  expect_tidy_error(add_locked_in_constraints(p, "locked_in"), "locked")
})
