test_that("numeric", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(
    id = seq_len(4), cost = c(10, 2, NA, 3),
    spp1 = c(1, 0, 0, 1), spp2 = c(10, 5, 10, 6)
  )
  budgets <- c(2.5, 5)
  # create problem
  p <-
    problem(
      pu$cost,
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- c(0, 1, NA, 1)
  # calculate scores
  r1 <- eval_rank_importance(p, s, budgets = budgets)
  r2 <- eval_rank_importance(p, s, n = 2)
  # create correct total scores
  r3 <- c(0, 1, NA_real_, 0.5)
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(1.5, 0.5)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "numeric")
  expect_inherits(r2, "numeric")
  expect_equal(r1, r3, ignore_attr = TRUE)
  expect_equal(r2, r3, ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("matrix (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(
    id = seq_len(4), cost = c(10, 2, NA, 3),
    spp1 = c(1, 0, 0, 1), spp2 = c(10, 5, 10, 6)
  )
  budgets <- c(2.5, 5)
  # create problem
  p <-
    problem(
      matrix(pu$cost, ncol = 1),
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- c(0, 1, NA, 1)
  # calculate scores
  r1 <- eval_rank_importance(p, s, budgets = budgets)
  r2 <- eval_rank_importance(p, s, n = 2)
  # create correct total scores
  r3 <- matrix(c(0, 1, NA_real_, 0.5), ncol = 1)
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(1.5, 0.5)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "numeric")
  expect_inherits(r2, "numeric")
  expect_equal(r1, r3, ignore_attr = TRUE)
  expect_equal(r2, r3, ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("matrix (multiple zones, by_zone = FALSE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
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
  budgets <- matrix(c(4, 8), ncol = 1)
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
  s <- matrix(c(1, 0, NA, 1, 0, 0, NA, 0, 0, 0, 0, 0, 1, 0, NA, 0), ncol = 2)
  # calculate scores
  r1 <- eval_rank_importance(p, s, budgets = budgets)
  r2 <- eval_rank_importance(p, s, n = 2, by_zone = FALSE)
  # create correct total scores
  r3 <- matrix(c(1, 0, NA, 1, 0, 0, NA, 0, 0, 0, 0, 0, 0.5, 0, NA, 0), ncol = 2)
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(1.0, 0)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "matrix")
  expect_inherits(r2, "matrix")
  expect_equal(r1, r3, ignore_attr = TRUE)
  expect_equal(r2, r3, ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("matrix (multiple zones, by_zone = TRUE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
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
  budgets <- matrix(c(2, 4, 2, 4), ncol = 2)
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
  s <- matrix(c(1, 0, NA, 1, 0, 0, NA, 0, 0, 0, 0, 0, 1, 0, NA, 0), ncol = 2)
  # calculate scores
  r1 <- eval_rank_importance(p, s, budgets = budgets)
  r2 <- eval_rank_importance(p, s, n = 2, by_zone = TRUE)
  # create correct total scores
  r3 <- matrix(
    c(1, 0, NA, 0.5, 0, 0, NA, 0, 0, 0, 0, 0, 0.5, 0, NA, 0),
    ncol = 2
  )
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(2, 0)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "matrix")
  expect_inherits(r2, "matrix")
  expect_equal(r1, r3, ignore_attr = TRUE)
  expect_equal(r2, r3, ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("data.frame (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- data.frame(
    id = seq_len(4), cost = c(10, 2, NA, 3),
    spp1 = c(1, 0, 0, 1), spp2 = c(10, 5, 10, 6)
  )
  budgets <- c(2.5, 5)
  # create problem
  p <-
    problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- tibble::tibble(solution = c(0, 1, NA, 1))
  # calculate scores
  r1 <- eval_rank_importance(p, s, budgets = budgets)
  r2 <- eval_rank_importance(p, s, n = 2)
  # create correct total scores
  r3 <- tibble::tibble(rs = c(0, 1, NA_real_, 0.5))
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(1.5, 0.5)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "data.frame")
  expect_inherits(r2, "data.frame")
  expect_equal(r1, r3, ignore_attr = TRUE)
  expect_equal(r2, r3, ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("data.frame (multiple zones, by_zone = FALSE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
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
  budgets <- matrix(c(4, 8), ncol = 1)
  # create problem
  p <-
    problem(
      pu,
      zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
      c("cost_1", "cost_2")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- data.frame(
    cost_1 = c(1, 0, NA, 1, 0, 0, NA, 0),
    cost_2 = c(0, 0, 0, 0, 1, 0, NA, 0)
  )
  # calculate scores
  r1 <- eval_rank_importance(p, s, budgets = budgets)
  r2 <- eval_rank_importance(p, s, n = 2, by_zone = FALSE)
  # create correct total scores
  r3 <- tibble::tibble(
    rc_1 = c(1, 0, NA, 1, 0, 0, NA, 0),
    rc_2 = c(0, 0, 0, 0, 0.5, 0, NA, 0)
  )
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(1.0, 0)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "data.frame")
  expect_inherits(r2, "data.frame")
  expect_equal(r1, r3, ignore_attr = TRUE)
  expect_equal(r2, r3, ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("data.frame (multiple zones, by_zone = TRUE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
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
  budgets <- matrix(c(2, 4, 2, 4), ncol = 2)
  # create problem
  p <-
    problem(
      pu,
      zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
      c("cost_1", "cost_2")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- data.frame(
    cost_1 = c(1, 0, NA, 1, 0, 0, NA, 0),
    cost_2 = c(0, 0, 0, 0, 1, 0, NA, 0)
  )  # calculate scores
  r1 <- eval_rank_importance(p, s, budgets = budgets)
  r2 <- eval_rank_importance(p, s, n = 2, by_zone = TRUE)
  # create correct total scores
  r3 <- tibble::tibble(
    rc_1 = c(1, 0, NA, 0.5, 0, 0, NA, 0),
    rc_2 = c(0, 0, 0, 0, 0.5, 0, NA, 0)
  )
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(2, 0)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "data.frame")
  expect_inherits(r2, "data.frame")
  expect_equal(r1, r3, ignore_attr = TRUE)
  expect_equal(r2, r3, ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("sf (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- get_sim_pu_polygons()[1:4, ]
  pu$id <- seq_len(4)
  pu$cost <- c(10, 2, NA, 3)
  pu$spp1 <- c(1, 0, 0, 1)
  pu$spp2 <- c(10, 5, 10, 6)
  pu$solution <- c(0, 1, NA, 1)
  pu$geometry <- sf::st_geometry(pu)
  pu <- sf::st_set_geometry(pu, "geometry")
  budgets <- c(2.5, 5)
  # create problem
  p <-
    problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # calculate ranks
  r1 <- eval_rank_importance(p, pu[, "solution"], n = 2)
  r2 <- eval_rank_importance(
    p, pu[, "solution"], budgets = budgets
  )
  # create correct result
  pu$rs <- c(0, 1, NA_real_, 0.5)
  r3 <- pu[, "rs"]
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(1.5, 0.5)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "sf")
  expect_inherits(r2, "sf")
  expect_equal(r1, r3, ignore_attr = TRUE)
  expect_equal(r2, r3, ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("sf (multiple zones, by_zone = FALSE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- sf::st_as_sf(tibble::tibble(
    id = seq_len(8),
    cost_1 = c(1,  2,  NA, 3, 100, 100, NA, 100),
    cost_2 = c(10, 10, 10, 10,  4,   1, NA, 10),
    spp1_1 = c(1,  2, 0, 0, 0, 0,  0, 0),
    spp2_1 = c(NA, 0, 1, 1, 0, 0,  0, 0),
    spp1_2 = c(1,  0, 0, 0, 1, 0,  0, 1),
    spp2_2 = c(0,  0, 0, 0, 0, 10, 0, 0),
    geometry = sf::st_geometry(get_sim_pu_polygons()[seq_len(8), ])
  ))
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  budgets <- matrix(c(4, 8), ncol = 1)
  # create problem
  p <-
    problem(
      pu,
      zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
      c("cost_1", "cost_2")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- sf::st_as_sf(tibble::tibble(
    cost_1 = c(1, 0, NA, 1, 0, 0, NA, 0),
    cost_2 = c(0, 0, 0, 0, 1, 0, NA, 0),
    geometry = sf::st_geometry(pu)
  ))
  # calculate scores
  r1 <- eval_rank_importance(p, s, budgets = budgets)
  r2 <- eval_rank_importance(p, s, n = 2, by_zone = FALSE)
  # create correct total scores
  r3 <-  sf::st_as_sf(tibble::tibble(
    rc_1 = c(1, 0, NA, 1, 0, 0, NA, 0),
    rc_2 = c(0, 0, 0, 0, 0.5, 0, NA, 0),
    geometry = sf::st_geometry(pu)
  ))
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(1.0, 0)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "data.frame")
  expect_inherits(r2, "data.frame")
  expect_equal(r1, r3, ignore_attr = TRUE)
  expect_equal(r2, r3, ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("sf (multiple zones, by_zone = TRUE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- sf::st_as_sf(tibble::tibble(
    id = seq_len(8),
    cost_1 = c(1,  2,  NA, 3, 100, 100, NA, 100),
    cost_2 = c(10, 10, 10, 10,  4,   1, NA, 10),
    spp1_1 = c(1,  2, 0, 0, 0, 0,  0, 0),
    spp2_1 = c(NA, 0, 1, 1, 0, 0,  0, 0),
    spp1_2 = c(1,  0, 0, 0, 1, 0,  0, 1),
    spp2_2 = c(0,  0, 0, 0, 0, 10, 0, 0),
    geometry = sf::st_geometry(get_sim_pu_polygons()[seq_len(8), ])
  ))
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  budgets <- matrix(c(2, 4, 2, 4), ncol = 2)
  # create problem
  p <-
    problem(
      pu,
      zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
      c("cost_1", "cost_2")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- sf::st_as_sf(tibble::tibble(
    cost_1 = c(1, 0, NA, 1, 0, 0, NA, 0),
    cost_2 = c(0, 0, 0, 0, 1, 0, NA, 0),
    geometry = sf::st_geometry(pu)
  ))
  # calculate scores
  r1 <- eval_rank_importance(p, s, budgets = budgets)
  r2 <- eval_rank_importance(p, s, n = 2, by_zone = TRUE)
  # create correct total scores
  r3 <- sf::st_as_sf(tibble::tibble(
    rs_1 = c(1, 0, NA, 0.5, 0, 0, NA, 0),
    rs_2 = c(0, 0, 0, 0, 0.5, 0, NA, 0),
    geometry = sf::st_geometry(pu)
  ))
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(2, 0)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "sf")
  expect_inherits(r2, "sf")
  expect_equal(r1, r3, ignore_attr = TRUE)
  expect_equal(r2, r3, ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("SpatRaster (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- terra::rast(matrix(c(10, 2, NA, 3), nrow = 1))
  features <- c(
    terra::rast(matrix(c(1, 0, 0, 1), nrow = 1)),
    terra::rast(matrix(c(10, 5, 10, 6), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  budgets <- c(2.5, 5)
  # create problem
  p <-
    problem(pu, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- terra::rast(matrix(c(0, 1, NA, 1), nrow = 1))
  # calculate ranks
  r1 <- eval_rank_importance(p, s, n = 2)
  r2 <- eval_rank_importance(p, s, budgets = budgets)
  # create correct result
  r3 <- terra::rast(matrix(c(0, 1, NA, 0.5), nrow = 1))
  names(r3) <- "rs"
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(1.5, 0.5)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "SpatRaster")
  expect_inherits(r2, "SpatRaster")
  expect_equal(terra::values(r1), terra::values(r3), ignore_attr = TRUE)
  expect_equal(terra::values(r2), terra::values(r3), ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("SpatRaster (multiple zones, by_zone = FALSE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- c(
    terra::rast(matrix(c(1,  2,  NA, 3, 100, 100, NA, 100), nrow = 1)),
    terra::rast(matrix(c(10, 10, 10, 10,  4,   1, NA, 10), nrow = 1))
  )
  names(pu) <- make.unique(names(pu))
  features <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0, 0), nrow = 1)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0, 0), nrow = 1)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0, 1), nrow = 1)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0, 0), nrow = 1))
  )
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  budgets <- matrix(c(4, 8), ncol = 1)
  # create problem
  p <-
    problem(pu, zones(features[[c(1, 2)]], features[[c(3, 4)]])) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- c(
    terra::rast(matrix(c(1, 0, NA, 1, 0, 0, NA, 0), nrow = 1)),
    terra::rast(matrix(c(0, 0, 0, 0, 1, 0, NA, 0), nrow = 1))
  )
  # calculate ranks
  r1 <- eval_rank_importance(p, s, budgets = budgets)
  r2 <- eval_rank_importance(p, s, n = 2, by_zone = FALSE)
  # create correct result
  r3 <- c(
    terra::rast(matrix(c(1, 0, NA, 1, 0, 0, NA, 0), nrow = 1)),
    terra::rast(matrix(c(0, 0, 0,  0, 0.5, 0, NA, 0), nrow = 1))
  )
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(1.0, 0)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "SpatRaster")
  expect_inherits(r2, "SpatRaster")
  expect_equal(terra::values(r1), terra::values(r3), ignore_attr = TRUE)
  expect_equal(terra::values(r2), terra::values(r3), ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("SpatRaster (multiple zones, by_zone = TRUE)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- c(
    terra::rast(matrix(c(1,  2,  NA, 3, 100, 100, NA, 100), nrow = 1)),
    terra::rast(matrix(c(10, 10, 10, 10,  4,   1, NA, 10), nrow = 1))
  )
  names(pu) <- make.unique(names(pu))
  features <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0, 0), nrow = 1)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0, 0), nrow = 1)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0, 1), nrow = 1)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0, 0), nrow = 1))
  )
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  budgets <- matrix(c(2, 4, 2, 4), ncol = 2)
  # create problem
  p <-
    problem(pu, zones(features[[c(1, 2)]], features[[c(3, 4)]])) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- c(
    terra::rast(matrix(c(1, 0, NA, 1, 0, 0, NA, 0), nrow = 1)),
    terra::rast(matrix(c(0, 0, 0, 0, 1, 0, NA, 0), nrow = 1))
  )
  # calculate ranks
  r1 <- eval_rank_importance(p, s, budgets = budgets)
  r2 <- eval_rank_importance(p, s, n = 2, by_zone = TRUE)
  # create correct result
  r3 <- c(
    terra::rast(matrix(c(1, 0, NA, 0.5, 0, 0, NA, 0), nrow = 1)),
    terra::rast(matrix(c(0, 0, 0,  0, 0.5, 0, NA, 0), nrow = 1))
  )
  attr(r3, "budgets") <- budgets
  attr(r3, "status") <- c("OPTIMAL", "OPTIMAL")
  attr(r3, "objective") <- c(2, 0)
  attr(r3, "runtime") <- c(1, 1)
  attr(r3, "gap") <- c(0, 0)
  # run tests
  ## objects
  expect_inherits(r1, "SpatRaster")
  expect_inherits(r2, "SpatRaster")
  expect_equal(terra::values(r1), terra::values(r3), ignore_attr = TRUE)
  expect_equal(terra::values(r2), terra::values(r3), ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("Spatial (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- get_sim_pu_polygons()[1:4, ]
  pu$id <- seq_len(4)
  pu$cost <- c(10, 2, NA, 3)
  pu$spp1 <- c(1, 0, 0, 1)
  pu$spp2 <- c(10, 5, 10, 6)
  pu$solution <- c(0, 1, NA, 1)
  pu$geometry <- sf::st_geometry(pu)
  pu <- sf::st_set_geometry(pu, "geometry")
  budgets <- c(2.5, 5)
  # create problems
  expect_warning(
    p1 <-
      problem(sf::as_Spatial(pu), c("spp1", "spp2"), cost_column = "cost") %>%
      add_min_set_objective() %>%
      add_absolute_targets(c(2, 10)) %>%
      add_binary_decisions() %>%
      add_default_solver(gap = 0, verbose = FALSE),
    "deprecated"
  )
  p2 <-
    problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # calculate ranks
  expect_warning(
    r1 <- eval_rank_importance(
      p1, sf::as_Spatial(pu[, "solution"]), n = 2
    ),
    "deprecated"
  )
  expect_warning(
    r2 <- eval_rank_importance(
      p1, sf::as_Spatial(pu[, "solution"]), budgets = budgets
    ),
    "deprecated"
  )
  # correct result
  r3 <- eval_rank_importance(
    p2, pu[, "solution"], budgets = budgets
  )
  # tests
  expect_inherits(r1, "Spatial")
  expect_inherits(r2, "Spatial")
  expect_equal(sf::st_as_sf(r1), r3, ignore_attr = TRUE)
  expect_equal(sf::st_as_sf(r2), r3, ignore_attr = TRUE)
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("Spatial (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- sf::st_as_sf(tibble::tibble(
    id = seq_len(8),
    cost_1 = c(1,  2,  NA, 3, 100, 100, NA, 100),
    cost_2 = c(10, 10, 10, 10,  4,   1, NA, 10),
    spp1_1 = c(1,  2, 0, 0, 0, 0,  0, 0),
    spp2_1 = c(NA, 0, 1, 1, 0, 0,  0, 0),
    spp1_2 = c(1,  0, 0, 0, 1, 0,  0, 1),
    spp2_2 = c(0,  0, 0, 0, 0, 10, 0, 0),
    geometry = sf::st_geometry(get_sim_pu_polygons()[seq_len(8), ])
  ))
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  budgets <- matrix(c(4, 8), ncol = 1)
  # create problems
  expect_warning(
    p1 <-
      problem(
        sf::as_Spatial(pu),
        zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
        c("cost_1", "cost_2")
      ) %>%
      add_min_set_objective() %>%
      add_absolute_targets(targets) %>%
      add_binary_decisions() %>%
      add_default_solver(gap = 0, verbose = FALSE),
    "deprecated"
  )
  p2 <-
    problem(
      pu,
      zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
      c("cost_1", "cost_2")
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- sf::st_as_sf(tibble::tibble(
    cost_1 = c(1, 0, NA, 1, 0, 0, NA, 0),
    cost_2 = c(0, 0, 0, 0, 1, 0, NA, 0),
    geometry = sf::st_geometry(pu)
  ))
  # calculate correct result
  expect_warning(
    r1 <- eval_rank_importance(
      p1, sf::as_Spatial(s), n = 2,
      by_zone = FALSE
    ),
    "deprecated"
  )
  expect_warning(
    r2 <- eval_rank_importance(
      p1, sf::as_Spatial(s), budgets = budgets
    ),
    "deprecated"
  )
  # correct result
  r3 <- eval_rank_importance(
    p2, s, budgets = budgets
  )
  # tests
  expect_inherits(r1, "Spatial")
  expect_inherits(r2, "Spatial")
  expect_equal(sf::st_as_sf(r1), r3, ignore_attr = TRUE)
  expect_equal(sf::st_as_sf(r2), r3, ignore_attr = TRUE)
  ## attributes
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("raster (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- terra::rast(matrix(c(10, 2, NA, 3), nrow = 1))
  features <- c(
    terra::rast(matrix(c(1, 0, 0, 1), nrow = 1)),
    terra::rast(matrix(c(10, 5, 10, 6), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  budgets <- c(2.5, 5)
  # create problem
  expect_warning(
    p1 <-
      problem(raster::raster(pu), raster::stack(features)) %>%
      add_min_set_objective() %>%
      add_absolute_targets(c(2, 10)) %>%
      add_binary_decisions() %>%
      add_default_solver(gap = 0, verbose = FALSE),
    "deprecated"
  )
  p2 <-
    problem(pu, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- terra::rast(matrix(c(0, 1, NA, 1), nrow = 1))
  # calculate ranks
  expect_warning(
    r1 <- eval_rank_importance(
      p1, raster::raster(s), n = 2
    ),
    "deprecated"
  )
  expect_warning(
    r2 <- eval_rank_importance(
      p1, raster::raster(s), budgets = budgets
    ),
    "deprecated"
  )
  # create correct result
  r3 <- eval_rank_importance(p2, s, budgets = budgets)
  # run tests
  ## objects
  expect_inherits(r1, "Raster")
  expect_inherits(r2, "Raster")
  expect_equal(
    terra::values(terra::rast(r1)),
    terra::values(r3),
    ignore_attr = TRUE
  )
  expect_equal(
    terra::values(terra::rast(r2)),
    terra::values(r3),
    ignore_attr = TRUE
  )
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("Raster (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- c(
    terra::rast(matrix(c(1,  2,  NA, 3, 100, 100, NA, 100), nrow = 1)),
    terra::rast(matrix(c(10, 10, 10, 10,  4,   1, NA, 10), nrow = 1))
  )
  names(pu) <- make.unique(names(pu))
  features <- c(
    terra::rast(matrix(c(1,  2, 0, 0, 0, 0,  0, 0), nrow = 1)),
    terra::rast(matrix(c(NA, 0, 1, 1, 0, 0,  0, 0), nrow = 1)),
    terra::rast(matrix(c(1,  0, 0, 0, 1, 0,  0, 1), nrow = 1)),
    terra::rast(matrix(c(0,  0, 0, 0, 0, 10, 0, 0), nrow = 1))
  )
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  budgets <- matrix(c(4, 8), ncol = 1)
  # create problems
  expect_warning(
    p1 <-
      problem(
        raster::stack(pu),
        suppressWarnings(zones(
          raster::stack(features[[c(1, 2)]]),
          raster::stack(features[[c(3, 4)]])
        ))
      ) %>%
      add_min_set_objective() %>%
      add_absolute_targets(targets) %>%
      add_binary_decisions() %>%
      add_default_solver(gap = 0, verbose = FALSE),
      "deprecated"
  )
  p2 <-
    problem(pu, zones(features[[c(1, 2)]], features[[c(3, 4)]])) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- c(
    terra::rast(matrix(c(1, 0, NA, 1, 0, 0, NA, 0), nrow = 1)),
    terra::rast(matrix(c(0, 0, 0, 0, 1, 0, NA, 0), nrow = 1))
  )
  # calculate ranks
  expect_warning(
    r1 <- eval_rank_importance(
      p1, raster::stack(s), budgets = budgets
    ),
    "deprecated"
  )
  expect_warning(
    r2 <- eval_rank_importance(
      p1, raster::stack(s), n = 2, by_zone = FALSE
    ),
    "deprecated"
  )
  # create correct result
  r3 <- eval_rank_importance(p2, s, budgets = budgets)
  # run tests
  ## objects
  expect_inherits(r1, "Raster")
  expect_inherits(r2, "Raster")
  expect_equal(
    terra::values(terra::rast(r1)),
    terra::values(r3),
    ignore_attr = TRUE
  )
  expect_equal(
    terra::values(terra::rast(r2)),
    terra::values(r3),
    ignore_attr = TRUE
  )
  ## attributes
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r3)[c("budgets", "objective")]
  )
  expect_equal(
    attributes(r1)[c("budgets", "objective")],
    attributes(r2)[c("budgets", "objective")]
  )
  expect_inherits(attr(r1, "runtime"), "numeric")
  expect_inherits(attr(r2, "runtime"), "numeric")
  expect_inherits(attr(r1, "gap"), "numeric")
  expect_inherits(attr(r2, "gap"), "numeric")
  expect_inherits(attr(r1, "status"), "character")
  expect_inherits(attr(r2, "status"), "character")
  expect_length(attr(r1, "runtime"), 2)
  expect_length(attr(r2, "runtime"), 2)
  expect_length(attr(r1, "gap"), 2)
  expect_length(attr(r2, "gap"), 2)
  expect_length(attr(r1, "status"), 2)
  expect_length(attr(r2, "status"), 2)
})

test_that("custom objective", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_phylogeny <- get_sim_phylogeny()
  # set targets
  ## note that we set targets such that we are likely to have some
  ## features that can have their targets entirely met when
  ## setting a budget that is 50% the cost of the min set solution
  targ <- c(rep(1, terra::nlyr(sim_features) - 1), 4)
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # create a solution
  s <- solve(p)
  # calculate ranks
  r1 <- eval_rank_importance(
    p, s, n = 2,
    objective = "add_max_phylo_div_objective",
    extra_args = list(tree = sim_phylogeny)
  )
  # tests
  expect_inherits(r1, "SpatRaster")
  expect_true(terra::global(r1 == 0, "sum", na.rm = TRUE)[[1]] > 0)
  expect_true(terra::global(r1 == 0.5, "sum", na.rm = TRUE)[[1]] > 0)
  expect_true(terra::global(r1 == 1, "sum", na.rm = TRUE)[[1]] > 0)
})

test_that("default budget-limited objective", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- terra::rast(matrix(c(10, 2, NA, 3), nrow = 1))
  features <- c(
    terra::rast(matrix(c(1, 0, 0, 1), nrow = 1)),
    terra::rast(matrix(c(10, 5, 10, 6), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  budgets <- c(2.5, 5)
  # create problem
  p <-
    problem(pu, features) %>%
    add_min_shortfall_objective(budget = max(budgets)) %>%
    add_absolute_targets(c(2, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- terra::rast(matrix(c(0, 1, NA, 1), nrow = 1))
  # calculate ranks
  r1 <- eval_rank_importance(p, s, n = 2)
  r2 <- eval_rank_importance(p, s, budgets = budgets)
  # create correct result
  r3 <- terra::rast(matrix(c(0, 1, NA, 0.5), nrow = 1))
  # run tests
  ## objects
  expect_inherits(r1, "SpatRaster")
  expect_inherits(r2, "SpatRaster")
  expect_equal(terra::values(r1), terra::values(r3), ignore_attr = TRUE)
  expect_equal(terra::values(r2), terra::values(r3), ignore_attr = TRUE)
  ## attributes
  expect_equal(attr(r1, "budgets"), budgets)
  expect_equal(attr(r2, "budgets"), budgets)
})

test_that("default budget-limited and explicit objectives give same result", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- terra::rast(matrix(c(10, 2, NA, 3), nrow = 1))
  features <- c(
    terra::rast(matrix(c(1, 0, 0, 1), nrow = 1)),
    terra::rast(matrix(c(10, 5, 10, 6), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  budgets <- c(2.5, 5)
  # create problem
  p <-
    problem(pu, features) %>%
    add_min_shortfall_objective(budget = max(budgets)) %>%
    add_absolute_targets(c(2, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- terra::rast(matrix(c(0, 1, NA, 1), nrow = 1))
  # calculate ranks
  r1 <- eval_rank_importance(p, s, n = 2)
  r2 <- eval_rank_importance(p, s, budgets = budgets)
  r3 <- eval_rank_importance(
    p, s, budgets = budgets,
    objective = "add_min_shortfall_objective"
  )
  r4 <- eval_rank_importance(
    p, s, n = 2,
    objective = "add_min_shortfall_objective"
  )
  # create correct result
  r5 <- terra::rast(matrix(c(0, 1, NA, 0.5), nrow = 1))
  # run tests
  ## objects
  expect_inherits(r1, "SpatRaster")
  expect_inherits(r2, "SpatRaster")
  expect_inherits(r3, "SpatRaster")
  expect_inherits(r4, "SpatRaster")
  expect_equal(terra::values(r1), terra::values(r5), ignore_attr = TRUE)
  expect_equal(terra::values(r2), terra::values(r5), ignore_attr = TRUE)
  expect_equal(terra::values(r3), terra::values(r5), ignore_attr = TRUE)
  expect_equal(terra::values(r4), terra::values(r5), ignore_attr = TRUE)
  ## attributes
  expect_equal(attr(r1, "budgets"), budgets)
  expect_equal(attr(r2, "budgets"), budgets)
  expect_equal(attr(r3, "budgets"), budgets)
  expect_equal(attr(r4, "budgets"), budgets)
})

test_that("locked in constraints", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- terra::rast(matrix(c(10, 2, NA, 3, 1), nrow = 1))
  locked <- terra::rast(matrix(c(0, 0, NA, 0, 1), nrow = 1))
  features <- c(
    terra::rast(matrix(c(1, 0, 0, 1, 0), nrow = 1)),
    terra::rast(matrix(c(10, 5, 10, 6, 0), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  budgets <- c(3.5, 6)
  # create problem
  p <-
    problem(pu, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_in_constraints(locked) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- terra::rast(matrix(c(0, 1, NA, 1, 1), nrow = 1))
  # calculate ranks
  r1 <- eval_rank_importance(p, s, n = 2)
  r2 <- eval_rank_importance(p, s, budgets = budgets)
  # create correct result
  r3 <- terra::rast(matrix(c(0, 1, NA, 0.5, 1), nrow = 1))
  names(r3) <- "rs"
  # run tests
  ## objects
  expect_inherits(r1, "SpatRaster")
  expect_inherits(r2, "SpatRaster")
  expect_inherits(r3, "SpatRaster")
  expect_equal(terra::values(r1), terra::values(r3), ignore_attr = TRUE)
  expect_equal(terra::values(r2), terra::values(r3), ignore_attr = TRUE)
  ## attributes
  expect_equal(attr(r1, "budgets"), budgets)
  expect_equal(attr(r2, "budgets"), budgets)
})

test_that("locked out constraints", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  # create data
  pu <- terra::rast(matrix(c(10, 2, NA, 3, 1), nrow = 1))
  locked <- terra::rast(matrix(c(0, 0, NA, 0, 1), nrow = 1))
  features <- c(
    terra::rast(matrix(c(1, 0, 0, 1, 100), nrow = 1)),
    terra::rast(matrix(c(10, 5, 10, 6, 100), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  budgets <- c(2.5, 5)
  # create problem
  p <-
    problem(pu, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_locked_out_constraints(locked) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- terra::rast(matrix(c(0, 1, NA, 1, 0), nrow = 1))
  # calculate ranks
  r1 <- eval_rank_importance(p, s, n = 2)
  r2 <- eval_rank_importance(p, s, budgets = budgets)
  # create correct result
  r3 <- terra::rast(matrix(c(0, 1, NA, 0.5, 0), nrow = 1))
  # run tests
  ## objects
  expect_inherits(r1, "SpatRaster")
  expect_inherits(r2, "SpatRaster")
  expect_inherits(r3, "SpatRaster")
  expect_equal(terra::values(r1), terra::values(r3), ignore_attr = TRUE)
  expect_equal(terra::values(r2), terra::values(r3), ignore_attr = TRUE)
  ## attributes
  expect_equal(attr(r1, "budgets"), budgets)
  expect_equal(attr(r2, "budgets"), budgets)
})

test_that("invalid inputs", {
  # import data
  pu <- terra::rast(matrix(c(10, 2, NA, 3), nrow = 1))
  features <- c(
    terra::rast(matrix(c(1, 0, 0, 1), nrow = 1)),
    terra::rast(matrix(c(10, 5, 10, 6), nrow = 1))
  )
  names(features) <- make.unique(names(features))
  budgets <- c(2.5, 5)
  # create problem
  p <-
    problem(pu, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(2, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- terra::rast(matrix(c(0, 1, NA, 1), nrow = 1))
  # not specifying n explicitly
  expect_tidy_error(
    eval_rank_importance(p, s, 1),
    "explicitly named"
  )
  # not specifying n or budgets
  expect_tidy_error(
    eval_rank_importance(p, s),
    "Exactly one of"
  )
  # specifying both n and budgets
  expect_tidy_error(
    eval_rank_importance(p, s, n = 2, budgets = budgets),
    "not both"
  )
  # specifying both n < 2
  expect_tidy_error(
    eval_rank_importance(p, s, n = 1),
    "greater than or equal"
  )
  # specifying invalid objective
  expect_tidy_error(
    eval_rank_importance(p, s, n = 2, objective = "add_min_set_objective"),
    "budget"
  )
  expect_tidy_error(
    eval_rank_importance(p, s, n = 2, objective = "asdf"),
    "name of an objective function"
  )
  # specifying extra_args with unused parameter
  expect_tidy_error(
    eval_rank_importance(p, s, n = 2, extra_args = list(1)),
    "explicitly named"
  )
  expect_tidy_error(
    eval_rank_importance(p, s, n = 2, extra_args = list(asdf = 1)),
    "must be `NULL`"
  )
  expect_tidy_error(
    eval_rank_importance(
      p, s, n = 2, objective = "add_max_phylo_div_objective",
      extra_args = list(asdf = 1)
    ),
    "tree"
  )
  # specifying extra_args with budget parameter
  expect_tidy_error(
    eval_rank_importance(p, s, n = 2, extra_args = list(budget = 1)),
    "must not have an element named"
  )
  # specifying extra_args with x parameter
  expect_tidy_error(
    eval_rank_importance(p, s, n = 2, extra_args = list(x = 1)),
    "must not have an element named"
  )
  # specifying n as a number that is > number of planning units
  expect_tidy_error(
    eval_rank_importance(p, s, n = 1000),
    "selected planning units"
  )
  # specifying only 1 budget value
  expect_tidy_error(
    eval_rank_importance(p, s, budgets = 2.5),
    "budgets"
  )
  # specifying only 1 budget value
  expect_tidy_error(
    eval_rank_importance(p, s, budgets = matrix(2.5)),
    "budgets"
  )
  # specifying budgets with ncol() > 1 and not equal to number of zones
  expect_tidy_error(
    eval_rank_importance(p, s, budgets = matrix(c(2, 4, 2, 4), ncol = 2)),
    "budgets"
  )
  # specifying complex constraints throws error
  expect_tidy_error(
    p %>%
      add_linear_constraints(100, "<=", pu) %>%
      eval_rank_importance(s, n = 2),
    "pre-computed"
  )
  # warning if non-default by_zone value is used with budgets
  expect_warning(
    eval_rank_importance(p, s, by_zone = FALSE, budgets = budgets),
    "default parameter value"
  )
})
