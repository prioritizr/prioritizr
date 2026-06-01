test_that("single zone (SpatRaster planning units)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  names(sim_zones_pu_raster) <- rep("zone_1", 3)
  sim_features1 <- get_sim_features()
  sim_features2 <- sim_features1 * 2
  names(sim_features2) <- letters[seq_len(terra::nlyr(sim_features2))]
  # create multi problem
  mp <-
    multi_problem(
      obj1 =
        problem(sim_zones_pu_raster[[1]], sim_features1) %>%
        add_min_set_objective() %>%
        add_absolute_targets(seq_along(terra::nlyr(sim_features1))) %>%
        add_neighbor_constraints(2) %>%
        add_neighbor_constraints(2) %>%
        add_binary_decisions(),
      obj2 =
        problem(sim_zones_pu_raster[[2]], sim_features2) %>%
        add_min_set_objective() %>%
        add_boundary_penalties(1) %>%
        add_boundary_penalties(2) %>%
        add_boundary_penalties(3) %>%
        add_absolute_targets(rev(seq_along(terra::nlyr(sim_features2)))) %>%
        add_binary_decisions()
    ) %>%
    add_wtd_sum_approach(weights = c(1, 1))
  # verify that object can be printed
  suppressMessages(print(mp))
  suppressMessages(summary(mp))
  suppressMessages(mp)
  suppressMessages(mp$print())
  suppressMessages(mp$show())
  suppressMessages(mp$repr())
  # test for problem-specific info
  expect_true(length(mp$problems) == 2)
  expect_equal(mp$problem_names(), c("obj1", "obj2"))
  # test for logical fields
  expect_true(mp$is_ids_equivalent_to_indices())
  # test for character fields
  expect_equal(mp$planning_unit_class(), "SpatRaster")
  # test for integer fields
  expect_equal(
    number_of_planning_units(mp),
    length(terra::cells(is.na(sim_zones_pu_raster), 0)[[1]])
  )
  expect_equal(mp$number_of_total_units(), terra::ncell(sim_zones_pu_raster))
  expect_equal(
    mp$planning_unit_indices(),
    terra::cells(is.na(sim_zones_pu_raster), 0)[[1]]
  )
  expect_equal(
    number_of_features(mp),
    terra::nlyr(c(sim_features1, sim_features2))
  )
  expect_equal(
    feature_names(mp),
    c(
      setNames(names(sim_features1), rep("obj1", terra::nlyr(sim_features1))),
      setNames(names(sim_features2), rep("obj2", terra::nlyr(sim_features2)))
    )
  )
  # tests for methods
  expect_equal(
    mp$convert_total_unit_ids_to_indices(1:10),
    mp$problems[[1]]$convert_total_unit_ids_to_indices(1:10)
  )
  expect_equal(
    mp$planning_unit_indices_with_finite_costs(),
    mp$problems[[1]]$planning_unit_indices_with_finite_costs()
  )
  expect_error(mp$total_unit_ids())
})

test_that("single zone (numeric planning units)", {
  # create data
  pu <- data.frame(
    id = seq_len(10), cost = c(0.2, NA, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA),
    spp3 = runif(10), spp4 = c(rpois(9, 4), NA)
  )
  # create problem
  mp <-
    multi_problem(
      obj1 =
        problem(
          pu$cost,
          data.frame(id = seq_len(3), name = c("spp1", "spp2", "spp3")),
          as.matrix(t(pu[, 3:5]))
        ) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decisions(),
      obj2 =
        problem(
          pu$cost,
          data.frame(id = 1L, name = "spp4"),
          as.matrix(t(pu[, 6, drop = FALSE]))
        ) %>%
        add_min_set_objective() %>%
        add_relative_targets(0.3) %>%
        add_binary_decisions()
    )
  # verify that object can be printed
  suppressMessages(print(mp))
  suppressMessages(summary(mp))
  suppressMessages(mp)
  suppressMessages(mp$print())
  suppressMessages(mp$show())
  suppressMessages(mp$repr())
  # test for problem-specific info
  expect_true(length(mp$problems) == 2)
  expect_equal(mp$problem_names(), c("obj1", "obj2"))
  # test for logical fields
  expect_true(mp$is_ids_equivalent_to_indices())
  # test for character fields
  expect_equal(mp$planning_unit_class(), "matrix")
  # test for integer fields
  expect_equal(
    number_of_planning_units(mp),
    sum(!is.na(pu$cost))
  )
  expect_equal(mp$number_of_total_units(), nrow(pu))
  expect_equal(
    mp$planning_unit_indices(),
    which(!is.na(pu$cost))
  )
  expect_equal(
    number_of_features(mp),
    4L
  )
  expect_equal(
    feature_names(mp),
    c(
      setNames(c("spp1", "spp2", "spp3"), rep("obj1", 3)),
      setNames("spp4", "obj2")
    )
  )
  # tests for methods
  expect_equal(
    mp$convert_total_unit_ids_to_indices(1:10),
    mp$problems[[1]]$convert_total_unit_ids_to_indices(1:10)
  )
  expect_equal(
    mp$planning_unit_indices_with_finite_costs(),
    mp$problems[[1]]$planning_unit_indices_with_finite_costs()
  )
  expect_error(mp$total_unit_ids())
})

test_that("multiple zones", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_features <- get_sim_features()
  # set budgets
  budgets <- 0.2 * c(
    terra::global(sim_zones_pu_raster[[1]], sum, na.rm = TRUE)[[1]],
    terra::global(sim_zones_pu_raster[[2]], sum, na.rm = TRUE)[[1]]
  )
  # build problem 1
  p1 <-
    problem(
      c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
      zones(z1 = sim_features, z2 = sim_features)
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(
      matrix(
        seq_len(terra::nlyr(sim_features)),
        nrow = terra::nlyr(sim_features),
        ncol = 2
      )
    ) %>%
    add_binary_decisions()
  # build problem 2
  p2 <-
    problem(
      c(sim_zones_pu_raster[[1]], sim_zones_pu_raster[[2]]),
      zones(z1 = sim_features, z2 = sim_features)
    ) %>%
    add_min_shortfall_objective(budget = budgets) %>%
    add_absolute_targets(
      matrix(
        rev(seq_len(terra::nlyr(sim_features))),
        nrow = terra::nlyr(sim_features),
        ncol = 2
      )
    ) %>%
    add_binary_decisions()
  # create multi problem
  mp <- multi_problem(obj1 = p1, obj2 = p2)
  # verify that object can be printed
  suppressMessages(print(mp))
  suppressMessages(summary(mp))
  suppressMessages(mp)
  suppressMessages(mp$print())
  suppressMessages(mp$show())
  suppressMessages(mp$repr())
  # test for problem-specific info
  expect_true(length(mp$problems) == 2)
  expect_equal(mp$problem_names(), c("obj1", "obj2"))
  expect_true(mp$number_of_zones() == 2)
  expect_equal(mp$zone_names(), c("z1", "z2"))
  # test for logical fields
  expect_true(mp$is_ids_equivalent_to_indices())
  # test for character fields
  expect_equal(mp$planning_unit_class(), "SpatRaster")
  # test for integer fields
  expect_equal(
    mp$number_of_planning_units(),
    length(terra::cells(is.na(sim_zones_pu_raster), 0)[[1]])
  )
  expect_equal(mp$number_of_total_units(), terra::ncell(sim_zones_pu_raster))
  expect_equal(
    mp$planning_unit_indices(),
    terra::cells(is.na(sim_zones_pu_raster), 0)[[1]]
  )
  expect_error(mp$total_unit_ids())
})

test_that("warnings", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define base problem
  p <-
    problem(sim_pu_raster, sim_features[[1:3]]) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  # problem with portfolio
  expect_warning(
    multi_problem(p %>% add_cuts_portfolio(5), p),
    regexp = "portfolios"
  )
  # problem with non-default solver
  expect_warning(
    multi_problem(p %>% add_compile_solver(), p),
    regexp = "solver"
  )
  # overwriting approach
  expect_warning(
    multi_problem(p, p) %>%
      add_hier_approach(rel_tol = 0.5) %>%
      add_hier_approach(rel_tol = 0.5),
    regexp = "approach"
  )
  # overwriting solver
  expect_warning(
    multi_problem(p, p) %>%
      add_compile_solver() %>%
      add_compile_solver(),
    regexp = "solver"
  )
})

test_that("invalid inputs", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # single problem (invalid: only one problem provided)
  expect_tidy_error(
    multi_problem(
      problem(sim_zones_pu_raster[[1]], sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
        add_binary_decisions()
    ),
    "at least two"
  )
  # mismatched planning units (raster mismatch)
  expect_tidy_error(
    {
      c1 <- sim_pu_raster
      c2 <- sim_pu_raster
      c2[1:10] <- NA
      multi_problem(
        obj1 =
          problem(c1, sim_features) %>%
          add_min_set_objective() %>%
          add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
          add_binary_decisions(),
        obj2 =
          problem(c2, sim_features) %>%
          add_min_set_objective() %>%
          add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
          add_binary_decisions()
      )
    },
    "planning units"
  )
  # mismatched planning unit types (raster vs polygons)
  expect_tidy_error(
    {
      multi_problem(
        obj1 =
          problem(sim_pu_raster, sim_features) %>%
          add_min_set_objective() %>%
          add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
          add_binary_decisions(),
        obj2 =
          problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
          add_min_set_objective() %>%
          add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
          add_binary_decisions()
      )
    },
    "planning unit"
  )
  # mismatched number of zones
  expect_tidy_error(
    multi_problem(
      obj1 =
        problem(sim_pu_raster, sim_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(seq_along(terra::nlyr(sim_features))) %>%
        add_binary_decisions(),
      obj2 =
        problem(sim_zones_pu_raster, sim_zones_features) %>%
        add_min_set_objective() %>%
        add_absolute_targets(
          matrix(
            1,
            ncol = number_of_zones(sim_zones_features),
            nrow = number_of_features(sim_zones_features)
          )
        ) %>%
        add_binary_decisions()
    ),
    "same number of zones"
  )
  # mismatched zone names
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  p1 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
    add_binary_decisions()
  attr(sim_zones_features, "zone_names") <- c("zone1", "zone2", "zone3")
  p2 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
    add_binary_decisions()
  expect_error(
    multi_problem(p1, p2),
    regexp = "zone names"
  )
  # mismatched decision types
  p1 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
    add_proportion_decisions()
  expect_error(
    multi_problem(p1, p2),
    regexp = "decision types"
  )
})
