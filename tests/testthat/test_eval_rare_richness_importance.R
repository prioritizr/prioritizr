test_that("numeric", {
  # create data
  pu <- data.frame(
    id = seq_len(4), cost = c(10, 2, NA, 3),
    spp1 = c(0, 0, 0, 1), spp2 = c(10, 5, 10, 6)
  )
  # create problem
  p <-
    problem(
      pu$cost,
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- c(0, 1, NA, 1)
  # calculate replacement costs
  r <- eval_rare_richness_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- c(0, ((5 / 10) / 31), NA, ((6 / 10) / 31) + (1 / 1))
  # run tests
  expect_equal(r, r2)
})

test_that("numeric (feature with zero abundance in all planning units)", {
  # create data
  pu <- data.frame(
    id = seq_len(4), cost = c(10, 2, NA, 3),
    spp1 = c(0, 0, 0, 1), spp2 = c(10, 5, 10, 6), spp3 = c(0, 0, 0, 0)
  )
  # create problem
  p <-
    problem(
      pu$cost,
      data.frame(id = seq_len(3), name = c("spp1", "spp2", "spp3")),
      as.matrix(t(pu[, 3:5]))
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10, 0)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- c(0, 1, NA, 1)
  # calculate replacement costs
  r <- eval_rare_richness_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- c(0, ((5 / 10) / 31), NA, ((6 / 10) / 31) + (1 / 1))
  # run tests
  expect_equal(r, r2)
})

test_that("matrix (single zone)", {
  # create data
  pu <- data.frame(
    id = seq_len(4), cost = c(10, 2, NA, 3),
    spp1 = c(0, 0, 0, 1), spp2 = c(10, 5, 10, 6))
  # create problem
  p <-
    problem(
      matrix(pu$cost, ncol = 1),
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- matrix(c(0, 1, NA, 1), ncol = 1)
  # calculate replacement costs
  r <- eval_rare_richness_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- matrix(c(0, ((5 / 10) / 31), NA, ((6 / 10) / 31) + (1 / 1)), ncol = 1)
  colnames(r2) <- "rwr"
  # run tests
  expect_equal(r, r2)
})

test_that("matrix (single zone, rescale = TRUE)", {
  # create data
  pu <- data.frame(
    id = seq_len(4),
    cost = c(10, 2, NA, 3),
    spp1 = c(0, 0, 0, 1),
    spp2 = c(10, 5, 10, 6))
  # create problem
  p <-
    problem(
      matrix(pu$cost, ncol = 1),
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- matrix(c(0, 1, NA, 1), ncol = 1)
  # calculate replacement costs
  r <- eval_rare_richness_importance(p, s, rescale = TRUE)
  # create correct result
  r2 <- matrix(c(0, 0.01, NA, 1), ncol = 1)
  colnames(r2) <- "rwr"
  # run tests
  expect_equal(r, r2)
})

test_that("data.frame (single zone)", {
  # create data
  pu <-
    data.frame(
      id = seq_len(4), cost = c(10, 2, NA, 3),
      spp1 = c(0, 0, 0, 1), spp2 = c(10, 5, 10, 6)
    )
  # create problem
  p <-
    problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- tibble::tibble(solution = c(0, 1, NA, 1))
  # calculate replacement costs
  r <- eval_rare_richness_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- tibble::tibble(
    rwr = c(0, ((5 / 10) / 31), NA, ((6 / 10) / 31) + (1 / 1))
  )
  # run tests
  expect_equal(r, r2)
})

test_that("sf (single zone)", {
  # create data
  pu <- get_sim_pu_polygons()[1:4, ]
  pu$id <- seq_len(4)
  pu$cost <- c(10, 2, NA, 3)
  pu$spp1 <- c(0, 0, 0, 1)
  pu$spp2 <- c(10, 5, 10, 6)
  # create problem
  p <-
    problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  pu$solution <- c(0, 1, NA, 1)
  # calculate replacement costs
  r <- eval_rare_richness_importance(p, pu[, "solution"], rescale = FALSE)
  # create correct result
  pu$rwr <- c(0, ((5 / 10) / 31), NA, ((6 / 10) / 31) + (1 / 1))
  # run tests
  expect_equal(r, pu[, "rwr"], ignore_attr = TRUE)
})

test_that("Spatial (single zone)", {
  # create data
  pu <- get_sim_pu_polygons()[1:4, ]
  pu$id <- seq_len(4)
  pu$cost <- c(10, 2, NA, 3)
  pu$spp1 <- c(0, 0, 0, 1)
  pu$spp2 <- c(10, 5, 10, 6)
  # create problems
  p1 <-
    problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  expect_warning(
    p2 <-
      problem(sf::as_Spatial(pu), c("spp1", "spp2"), cost_column = "cost") %>%
      add_min_set_objective() %>%
      add_absolute_targets(c(1, 10)) %>%
      add_binary_decisions() %>%
      add_default_solver(gap = 0, verbose = FALSE),
    "deprecated"
  )
  # create a solution
  pu$solution <- c(0, 1, NA, 1)
  # calculate replacement costs
  r1 <- eval_rare_richness_importance(p1, pu[, "solution"], rescale = FALSE)
  expect_warning(
    r2 <- eval_rare_richness_importance(
      p2, sf::as_Spatial(pu[, "solution"]), rescale = FALSE
    ),
    "deprecated"
  )
  # run tests
  expect_equal(
    tibble::tibble(sf::st_drop_geometry(r1)),
    tibble::tibble(r2@data),
    ignore_attr = TRUE
  )
})

test_that("SpatRaster (single zone)", {
  # create data
  pu <- terra::rast(matrix(c(10, 2, NA, 3), nrow = 1))
  features <- c(
    terra::rast(matrix(c(0, 0, 0, 1), nrow = 1)),
    terra::rast(matrix(c(10, 5, 10, 6), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  # create problem
  p <-
    problem(pu, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- terra::rast(matrix(c(0, 1, NA, 1), nrow = 1))
  # calculate replacement costs
  r <- eval_rare_richness_importance(p, s, rescale = FALSE)
  # create correct result
  r2 <- terra::rast(
    matrix(c(0, ((5 / 10) / 31), NA, ((6 / 10) / 31) + (1 / 1)), nrow = 1)
  )
  names(r2) <- "rwr"
  # run tests
  expect_equal(terra::values(r), terra::values(r2))
})

test_that("Raster (single zone)", {
  # create data
  pu <- terra::rast(matrix(c(10, 2, NA, 3), nrow = 1))
  features <- c(
    terra::rast(matrix(c(0, 0, 0, 1), nrow = 1)),
    terra::rast(matrix(c(10, 5, 10, 6), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  # create a solution
  s <- terra::rast(matrix(c(0, 1, NA, 1), nrow = 1))
  # create problems
  p1 <-
    problem(pu, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  expect_warning(
    p2 <-
      problem(raster::raster(pu), raster::stack(features)) %>%
      add_min_set_objective() %>%
      add_absolute_targets(c(1, 10)) %>%
      add_binary_decisions() %>%
      add_default_solver(gap = 0, verbose = FALSE),
    "deprecated"
  )
  # calculate replacement costs
  r1 <- eval_rare_richness_importance(p1, s, rescale = FALSE)
  expect_warning(
    r2 <- eval_rare_richness_importance(p2, raster::raster(s), rescale = FALSE),
    "deprecated"
  )
  # run tests
  expect_equal(terra::values(r1), terra::values(terra::rast(r2)))
})

test_that("invalid inputs", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create data
  pu <- data.frame(
    id = seq_len(10), cost = c(0.2, NA, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA),
    solution = rep(c(0, 1), 5)
  )
  # tests
  expect_tidy_error(
    {
      # create problem
      x <- problem(
        pu$cost,
        data.frame(id = seq_len(2), name = c("spp1", "spp2")),
        as.matrix(t(pu[, 3:4]))
      )
      # create a solution
      y <- round(pu$cost)
      # calculate representation
      r <- eval_rare_richness_importance(x, y, rescale = "a")
    },
    "flag"
  )
  expect_tidy_error(
    {
      # create problem
      x <- problem(
        pu$cost,
        data.frame(id = seq_len(2), name = c("spp1", "spp2")),
        as.matrix(t(pu[, 3:4]))
      )
      # create a solution
      y <- rep(c(0, 1), 5)
      # calculate representation
      r <- eval_rare_richness_importance(x, y)
    },
    "missing"
  )
  expect_tidy_error(
    {
      # create problem
      x <- problem(
        matrix(pu$cost, ncol = 1),
        data.frame(id = seq_len(2), name = c("spp1", "spp2")),
        as.matrix(t(pu[, 3:4]))
      )
      # create a solution
      y <- matrix(rep(c(0, 1), 5), ncol = 1)
      # calculate representation
      r <- eval_rare_richness_importance(x, y)
    },
    "missing"
  )
  expect_tidy_error(
    {
      # create problem
      x <- problem(pu, c("spp1", "spp2"), cost_column = "cost")
      # create a solution
      y <- data.frame(solution = rep(c(0, 1), 5))
      # calculate representation
      r <- eval_rare_richness_importance(x, y)
    },
    "missing"
  )
  expect_tidy_error(
    {
      # create problem
      x <- problem(pu, c("spp1", "spp2"), "cost")
      # create a solution
      y <- pu[, "solution", drop = FALSE]
      # calculate representation
      r <- eval_rare_richness_importance(x, y)
    },
    "missing"

  )
  expect_tidy_error(
    {
      # create problem
      x <- problem(sim_pu_raster, sim_features)
      # create a solution
      y <- terra::setValues(
        sim_pu_raster,
        rep(c(0, 1), terra::ncell(sim_pu_raster) / 2)
      )
      # calculate representation
      r <- eval_rare_richness_importance(x, y)
    },
    "missing"
  )
  expect_tidy_error(
    {
      # create data
      pu <- data.frame(
        id = seq_len(8),
        cost_1 = c(1,  2,  NA, 3, 100, 100, NA, 100),
        cost_2 = c(10, 10, 10, 10,  4,   1, NA, 10),
        spp1_1 = c(1,  2, 0, 0, 0, 0,  0, 0),
        spp2_1 = c(NA, 0, 1, 1, 0, 0,  0, 0),
        spp1_2 = c(1,  0, 0, 0, 1, 0,  0, 1),
        spp2_2 = c(0,  0, 0, 0, 0, 10, 0, 0)
      )
      targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
      # create problem
      p <-
        problem(
          as.matrix(pu[, c(2, 3)]),
          data.frame(id = seq_len(2), name = c("spp1", "spp2")),
          list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))
        ) %>%
        add_min_set_objective() %>%
        add_absolute_targets(targets) %>%
        add_binary_decisions() %>%
        add_default_solver(gap = 0, verbose = FALSE)
      # create a solution
      s <- matrix(
        c(1, 0, NA, 1, 0, 0, NA, 0, 0, 0, 0, 0, 1, 0, NA, 0),
        ncol = 2
      )
      # calculate replacement costs
      eval_rare_richness_importance(p, s, rescale = FALSE)
    },
    "single zone"
  )
})
