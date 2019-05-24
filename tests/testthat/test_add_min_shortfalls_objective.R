context("add_min_shortfall_objective")

test_that("compile (compressed formulation, single zone)", {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  b <- floor(raster::cellStats(sim_pu_raster, "sum"))
  targ <- unname(floor(raster::cellStats(sim_features, "sum") * 0.25))
  p <- problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(budget = b) %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  o <- compile(p)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_features <- raster::nlayers(sim_features)
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(rep(0, n_pu), 1 / targ))
  expect_equal(o$sense(), c(rep(">=", n_features), "<="))
  expect_equal(o$rhs(), c(targ, b))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("spp_met", n_features)))
  expect_equal(o$row_ids(), c(rep("spp_target", n_features), "budget"))
  expect_true(all(o$A()[seq_len(n_features),
                        seq_len(n_pu)] == p$data$rij_matrix[[1]]))
  expect_equal(o$A()[n_features + 1, ],
    c(p$planning_unit_costs(), rep(0, n_features)))
  expect_true(all(o$A()[seq_len(n_features), n_pu + seq_len(n_features)] ==
    Matrix::sparseMatrix(i = seq_len(n_features), j = seq_len(n_features),
      x = 1, giveCsparse = FALSE)))
  expect_true(all(o$lb() == 0))
  expect_equal(o$ub(), c(rep(1, n_pu), rep(Inf, n_features)))
})

test_that("solution (compressed formulation, single zone)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # create data
  budget <- 4.23
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
        add_min_shortfall_objective(budget = budget) %>%
        add_locked_in_constraints(locked_in) %>%
        add_locked_out_constraints(locked_out) %>%
        add_absolute_targets(c(2, 10)) %>%
        add_default_solver(gap = 0)
  # solve problem
  s1 <- solve(p)
  s2 <- solve(p)
  # test that solution is correct
  expect_equal(raster::values(s1), c(0, 1, 1, NA))
  expect_equal(raster::values(s1), raster::values(s2))
})

test_that("compile (expanded formulation, single zone)", {
  # generate optimization problem
  data(sim_pu_raster, sim_features)
  b <- floor(raster::cellStats(sim_pu_raster, "sum"))
  targ <- unname(floor(raster::cellStats(sim_features, "sum") * 0.25))
  p <- problem(sim_pu_raster, sim_features) %>%
    add_min_shortfall_objective(budget = b) %>%
    add_absolute_targets(targ) %>%
    add_binary_decisions()
  o <- compile(p, compressed_formulation = FALSE)
  # check that objective has been correctly applied
  n_pu <- length(sim_pu_raster[[1]][!is.na(sim_pu_raster)])
  n_f <- raster::nlayers(sim_features)
  rij <- rij_matrix(sim_pu_raster, sim_features)
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(rep(0, n_pu), rep(0, n_pu * n_f), 1 / targ))
  expect_equal(o$sense(), c(rep("<=", n_pu * n_f), rep(">=", n_f), "<="))
  expect_equal(o$rhs(), c(rep(0, n_pu * n_f), targ, b))
  expect_equal(o$col_ids(), c(rep("pu", n_pu), rep("pu_ijz", n_pu * n_f),
                              rep("spp_met", n_f)))
  expect_equal(o$row_ids(), c(rep("pu_ijz", n_pu * n_f), rep("spp_target", n_f),
                              "budget"))
  expect_equal(o$lb(), rep(0, n_pu + (n_pu * n_f) + n_f))
  expect_equal(o$ub(), c(rep(1, n_pu + (n_pu * n_f)), rep(Inf, n_f)))
  # test that model matrix has been constructed correctly
  row <- 0
  for (i in seq_len(n_f)) {
    for (j in seq_len(n_pu)) {
      row <- row + 1
      curr_row <- rep(0, n_pu + (n_pu * n_f) + n_f)
      curr_row[j] <- -1
      curr_row[n_pu + ( (i - 1) * n_pu) + j] <- 1
      expect_equal(o$A()[row, ], curr_row)
    }
  }
  for (i in seq_len(n_f)) {
    curr_row <- rep(0, n_pu + (n_pu * n_f) + n_f)
    curr_row[(i * n_pu) + seq_len(n_pu)] <- rij[i, ]
    curr_row[n_pu + (n_f * n_pu) + i] <- 1
    expect_equal(o$A()[(n_f * n_pu) + i, ], curr_row)
  }
  expect_equal(o$A()[(n_pu * n_f) + n_f + 1, ], c(p$planning_unit_costs(),
                                                  rep(0, n_f * n_pu),
                                                  rep(0, n_f)))
})

test_that("solution (expanded formulation, single zone)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # create data
  budget <- 4.23
  cost <- raster::raster(matrix(c(1, 2, 2, NA), ncol = 4))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2, 1, 1, 0), ncol = 4)),
                            raster::raster(matrix(c(10, 10, 10, 10), ncol = 4)))
  # create problem
  p <- problem(cost, features) %>%
        add_min_shortfall_objective(budget = budget) %>%
        add_locked_in_constraints(locked_in) %>%
        add_locked_out_constraints(locked_out) %>%
        add_absolute_targets(c(2, 10)) %>%
        add_default_solver(gap = 0)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # test that solution is correct
  expect_equal(raster::values(s), c(0, 1, 1, NA))
})

test_that("invalid inputs (single zone)", {
  # check that invalid arguments result in errors
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_min_shortfall_objective(budget = -5) %>%
      add_absolute_targets(targ)
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_min_shortfall_objective(budget = 0) %>%
      add_absolute_targets(targ)
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_min_shortfall_objective(budget = NA) %>%
      add_absolute_targets(targ)
  })
  expect_error({
    problem(sim_pu_raster, sim_features) %>%
      add_min_shortfall_objective(budget = Inf) %>%
      add_absolute_targets(targ)
  })
})

test_that("compile (compressed formulation, multiple zones, scalar budget)", {
  # generate optimization problem
  data(sim_pu_zones_stack, sim_features_zones)
  b <- min(floor(raster::cellStats(sim_pu_zones_stack, "sum")) * 0.25)
  targs <- tibble::tibble(
    feature = feature_names(sim_features_zones)[1:3],
    zone = list("zone_1", "zone_2", c("zone_1", "zone_3")),
    sense = c(">=", "<=", ">="),
    target = c(5, 300, 10),
    type = c("absolute", "absolute", "absolute"))
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_shortfall_objective(budget = b) %>%
       add_manual_targets(targs) %>%
       add_binary_decisions()
  o <- compile(p)
  # check that constraints and metadata have been correctly applied
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(rep(0, length(p$planning_unit_costs())),
                          1 / targs$target))
  expect_equal(o$sense(), c(targs$sense, rep("<=", length(b)),
                            rep("<=", n_pu)))
  expect_equal(o$rhs(), c(targs$target, b, rep(1, n_pu)))
  expect_equal(o$col_ids(), c(rep("pu", n_pu * n_z),
                              rep("spp_met", nrow(targs))))
  expect_equal(o$row_ids(), c(rep("spp_target", nrow(targs)),
                              rep("budget", length(b)), rep("pu_zone", n_pu)))
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + nrow(targs)))
  expect_equal(o$ub(), c(rep(1, (n_pu * n_z)), rep(Inf, nrow(targs))))
  # check that problem matrix is correct
  m <- matrix(0, nrow = nrow(targs) + length(b) + n_pu,
              ncol = (n_pu * n_z) + nrow(targs))
  counter <- 0
  for (i in seq_len(nrow(targs))) {
    zs <- match(targs$zone[[i]], zone_names(sim_features_zones))
    f <- match(targs$feature[i], feature_names(sim_features_zones))
    counter <- counter + 1
    for (z in zs)
      m[counter, ((z - 1) * n_pu) + seq_len(n_pu)] <- p$data$rij[[z]][f, ]
    m[counter, (n_z * n_pu) + i] <- 1
  }
  counter <- counter + 1
  m[counter, seq_len(n_pu * n_z)] <- c(p$planning_unit_costs())
  for (i in seq_len(n_pu)) {
    m[i + counter, i] <- 1
    m[i + counter, n_pu + i] <- 1
    m[i + counter, (2 * n_pu) + i] <- 1
  }
  m <- as(m, "Matrix")
  expect_true(all(o$A() == m))
})

test_that("solve (compressed formulation, multiple zones, scalar budget)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # make and solve problem
  budget <- 7
  locked_out <- matrix(FALSE, ncol = 2, nrow = 5)
  locked_out[1, 1] <- TRUE
  targs <- tibble::tibble(
    feature = c("f1", "f2", "f2"),
    zone = list("zone_1", "zone_2", c("zone_1", "zone_2")),
    sense = c(">=", ">=", "<="),
    target = c(5, 1, 300),
    type = c("absolute", "absolute", "absolute"))
  cost <- raster::stack(
    raster::raster(matrix(c(1, 2, 4, NA, NA), nrow = 1)),
    raster::raster(matrix(c(0.5, 1, 1, 1, NA), nrow = 1)))
  features <- raster::stack(
    raster::raster(matrix(c(5,   2,  3,  0,  4), nrow = 1)),
    raster::raster(matrix(c(5,   5,  5,  5,  5), nrow = 1)),
    raster::raster(matrix(c(5,   0,  1,  10, 4), nrow = 1)),
    raster::raster(matrix(c(500, 5,  5,  5,  5), nrow = 1)))
  # create problem
  p <- problem(cost, zones(features[[1:2]], features[[3:4]],
                           zone_names = c("zone_1", "zone_2"),
                           feature_names = c("f1", "f2"))) %>%
       add_min_shortfall_objective(budget = budget) %>%
       add_manual_targets(targs) %>%
       add_locked_out_constraints(locked_out) %>%
       add_default_solver(gap = 0)
  # solve problem
  s <- solve(p)
  # test that solution is correct
  expect_equal(raster::values(s[[1]]), c(0, 1, 1, NA,  NA))
  expect_equal(raster::values(s[[2]]), c(0, 0, 0, 1, NA))
})

test_that("compile (expanded formulation, multiple zones, scalar budget)", {
  # generate optimization problem
  data(sim_pu_zones_stack, sim_features_zones)
  b <- min(floor(raster::cellStats(sim_pu_zones_stack, "sum")) * 0.25)
  targs <- tibble::tibble(
    feature = feature_names(sim_features_zones)[1:3],
    zone = list("zone_1", "zone_2", c("zone_1", "zone_3")),
    sense = c(">=", "<=", ">="),
    target = c(5, 300, 10),
    type = c("absolute", "absolute", "absolute"))
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_shortfall_objective(budget = b) %>%
       add_manual_targets(targs) %>%
       add_binary_decisions()
  o <- compile(p, compressed_formulation = FALSE)
  # check that constraints and metadata have been correctly applied
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(rep(0, n_pu * n_z), rep(0, n_pu * n_f * n_z),
               1 / targs$target))
  expect_equal(o$sense(), c(rep("<=", n_f * n_z * n_pu),
                            targs$sense, rep("<=", length(b)),
                            rep("<=", n_pu)))
  expect_equal(o$rhs(), c(rep(0, n_pu * n_z * n_f),
                          rep(targs$target), b, rep(1, n_pu)))
  expect_equal(o$col_ids(), c(rep("pu", n_pu * n_z),
                              rep("pu_ijz", n_pu * n_z * n_f),
                              rep("spp_met", nrow(targs))))
  expect_equal(o$row_ids(), c(rep("pu_ijz", n_pu * n_z * n_f),
                              rep("spp_target", nrow(targs)),
                              rep("budget", length(b)), rep("pu_zone", n_pu)))
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + (n_pu * n_z * n_f) + nrow(targs)))
  expect_equal(o$ub(), c(rep(1, (n_pu * n_z) + (n_pu * n_z * n_f)),
                         rep(Inf, nrow(targs))))
  # check that problem matrix is correct
  m <- matrix(0, nrow = (n_pu * n_z * n_f) + nrow(targs) + length(b) + n_pu,
              ncol = (n_pu * n_z) + (n_pu * n_z * n_f) + nrow(targs))
  counter <- 0
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_f)) {
      for (j in seq_len(n_pu)) {
        counter <- counter + 1
        m[counter, ((z - 1) * n_pu) + j] <- -1
        m[counter, (n_pu * n_z) +
                   ((z - 1) * n_pu * n_f) +
                   ((i - 1) * n_pu) +
                   j] <- 1
      }
    }
  }
  for (i in seq_len(nrow(targs))) {
    zs <- match(targs$zone[[i]], zone_names(sim_features_zones))
    f <- match(targs$feature[i], feature_names(sim_features_zones))
    counter <- counter + 1
    for (z in zs) {
      for (pu in seq_len(n_pu)) {
        col <- (n_pu * n_z) + ((z - 1) * n_f * n_pu) +
               ((f - 1) * n_pu) + pu
        m[counter, col] <- p$data$rij[[z]][f, pu]
      }
      col <- (n_pu * n_z) + (n_pu * n_f * n_z) + i
      m[counter, col] <- 1
    }
  }
  counter <- counter + 1
  m[counter, seq_len(n_pu * n_z)] <- c(p$planning_unit_costs())
  for (i in seq_len(n_pu)) {
    m[i + counter, i] <- 1
    m[i + counter, n_pu + i] <- 1
    m[i + counter, (2 * n_pu) + i] <- 1
  }
  m <- as(m, "Matrix")
  expect_true(all(o$A() == m))
})

test_that("solve (expanded formulation, multiple zones, scalar budget)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # make and solve problem
  budget <- 7
  locked_out <- matrix(FALSE, ncol = 2, nrow = 5)
  locked_out[1, 1] <- TRUE
  targs <- tibble::tibble(
    feature = c("f1", "f2", "f2"),
    zone = list("zone_1", "zone_2", c("zone_1", "zone_2")),
    sense = c(">=", ">=", "<="),
    target = c(5, 1, 300),
    type = c("absolute", "absolute", "absolute"))
  cost <- raster::stack(
    raster::raster(matrix(c(1, 2, 4, NA, NA), nrow = 1)),
    raster::raster(matrix(c(0.5, 1, 1, 1, NA), nrow = 1)))
  features <- raster::stack(
    raster::raster(matrix(c(5,   2,  3,  0,  4), nrow = 1)),
    raster::raster(matrix(c(5,   5,  5,  5,  5), nrow = 1)),
    raster::raster(matrix(c(5,   0,  1,  10, 4), nrow = 1)),
    raster::raster(matrix(c(500, 5,  5,  5,  5), nrow = 1)))
  # create problem
  p <- problem(cost, zones(features[[1:2]], features[[3:4]],
                           zone_names = c("zone_1", "zone_2"),
                           feature_names = c("f1", "f2"))) %>%
       add_min_shortfall_objective(budget = budget) %>%
       add_manual_targets(targs) %>%
       add_locked_out_constraints(locked_out) %>%
       add_default_solver(gap = 0)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # test that solution is correct
  expect_equal(raster::values(s[[1]]), c(0, 1, 1, NA,  NA))
  expect_equal(raster::values(s[[2]]), c(0, 0, 0, 1, NA))
})

test_that("compile (compressed formulation, multiple zones, vector budget)", {
  # generate optimization problem
  data(sim_pu_zones_stack, sim_features_zones)
  b <- unname(floor(raster::cellStats(sim_pu_zones_stack, "sum")) * 0.25)
  targs <- tibble::tibble(
    feature = feature_names(sim_features_zones)[1:3],
    zone = list("zone_1", "zone_2", c("zone_1", "zone_3")),
    sense = c(">=", "<=", ">="),
    target = c(5, 300, 10),
    type = c("absolute", "absolute", "absolute"))
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_shortfall_objective(budget = b) %>%
       add_manual_targets(targs) %>%
       add_binary_decisions()
  o <- compile(p)
  # check that constraints and metadata have been correctly applied
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(rep(0, n_pu * n_z), 1 / targs$target))
  expect_equal(o$sense(), c(targs$sense, rep("<=", length(b)),
                            rep("<=", n_pu)))
  expect_equal(o$rhs(), c(targs$target, b, rep(1, n_pu)))
  expect_equal(o$col_ids(), c(rep("pu", n_pu * n_z),
                              rep("spp_met", nrow(targs))))
  expect_equal(o$row_ids(), c(rep("spp_target", nrow(targs)),
                              rep("budget", length(b)), rep("pu_zone", n_pu)))
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + nrow(targs)))
  expect_equal(o$ub(), c(rep(1, n_pu * n_z), rep(Inf, nrow(targs))))
  # check that problem matrix is correct
  m <- matrix(0, nrow = nrow(targs) + length(b) + n_pu,
              ncol = (n_pu * n_z) + nrow(targs))
  counter <- 0
  for (i in seq_len(nrow(targs))) {
    zs <- match(targs$zone[[i]], zone_names(sim_features_zones))
    f <- match(targs$feature[i], feature_names(sim_features_zones))
    counter <- counter + 1
    for (z in zs)
      m[counter, ((z - 1) * n_pu) + seq_len(n_pu)] <- p$data$rij[[z]][f, ]
    m[counter, (n_z * n_pu) + i] <- 1
  }
  counter <- counter + 1
  m[counter, seq_len(n_pu)] <- p$planning_unit_costs()[, 1]
  counter <- counter + 1
  m[counter, n_pu + seq_len(n_pu)] <- p$planning_unit_costs()[, 2]
  counter <- counter + 1
  m[counter, (2 * n_pu) + seq_len(n_pu)] <- p$planning_unit_costs()[, 3]
  for (i in seq_len(n_pu)) {
    m[i + counter, i] <- 1
    m[i + counter, n_pu + i] <- 1
    m[i + counter, (2 * n_pu) + i] <- 1
  }
  m <- as(m, "Matrix")
  expect_true(all(o$A() == m))
})

test_that("solve (compressed formulation, multiple zones, vector budget)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # make and solve problem
  budget <- c(6, 1)
  locked_out <- matrix(FALSE, ncol = 2, nrow = 5)
  locked_out[1, 1] <- TRUE
  targs <- tibble::tibble(
    feature = c("f1", "f2", "f2"),
    zone = list("zone_1", "zone_2", c("zone_1", "zone_2")),
    sense = c(">=", ">=", "<="),
    target = c(5, 1, 300),
    type = c("absolute", "absolute", "absolute"))
  cost <- raster::stack(
    raster::raster(matrix(c(1, 2, 4, NA, NA), nrow = 1)),
    raster::raster(matrix(c(0.5, 1, 1, 1, NA), nrow = 1)))
  features <- raster::stack(
    raster::raster(matrix(c(5,   2,  3,  0,  4), nrow = 1)),
    raster::raster(matrix(c(5,   5,  5,  5,  5), nrow = 1)),
    raster::raster(matrix(c(5,   0,  1,  10, 4), nrow = 1)),
    raster::raster(matrix(c(500, 5,  5,  5,  5), nrow = 1)))
  # create problem
  p <- problem(cost, zones(features[[1:2]], features[[3:4]],
                           zone_names = c("zone_1", "zone_2"),
                           feature_names = c("f1", "f2"))) %>%
       add_min_shortfall_objective(budget = budget) %>%
       add_manual_targets(targs) %>%
       add_locked_out_constraints(locked_out) %>%
       add_default_solver(gap = 0)
  # solve problem
  s <- solve(p)
  # test that solution is correct
  expect_equal(raster::values(s[[1]]), c(0, 1, 1, NA,  NA))
  expect_equal(raster::values(s[[2]]), c(0, 0, 0, 1, NA))
})

test_that("compile (expanded formulation, multiple zones, vector budget)", {
  # generate optimization problem
  data(sim_pu_zones_stack, sim_features_zones)
  b <- unname(floor(raster::cellStats(sim_pu_zones_stack, "sum")) * 0.25)
  targs <- tibble::tibble(
    feature = feature_names(sim_features_zones)[1:3],
    zone = list("zone_1", "zone_2", c("zone_1", "zone_3")),
    sense = c(">=", "<=", ">="),
    target = c(5, 300, 10),
    type = c("absolute", "absolute", "absolute"))
  p <- problem(sim_pu_zones_stack, sim_features_zones) %>%
       add_min_shortfall_objective(budget = b) %>%
       add_manual_targets(targs) %>%
       add_binary_decisions()
  o <- compile(p, compressed_formulation = FALSE)
  # check that constraints and metadata have been correctly applied
  n_pu <- p$number_of_planning_units()
  n_f <- p$number_of_features()
  n_z <- p$number_of_zones()
  expect_equal(o$modelsense(), "min")
  expect_equal(o$obj(), c(rep(0, n_pu * n_z), rep(0, n_pu * n_f * n_z),
                          1 / targs$target))
  expect_equal(o$sense(), c(rep("<=", n_f * n_z * n_pu),
                            targs$sense, rep("<=", length(b)),
                            rep("<=", n_pu)))
  expect_equal(o$rhs(), c(rep(0, n_pu * n_z * n_f),
                          targs$target, b, rep(1, n_pu)))
  expect_equal(o$col_ids(), c(rep("pu", n_pu * n_z),
                              rep("pu_ijz", n_pu * n_z * n_f),
                              rep("spp_met", nrow(targs))))
  expect_equal(o$row_ids(), c(rep("pu_ijz", n_pu * n_z * n_f),
                              rep("spp_target", nrow(targs)),
                              rep("budget", length(b)), rep("pu_zone", n_pu)))
  expect_equal(o$lb(), rep(0, (n_pu * n_z) + (n_pu * n_z * n_f) + nrow(targs)))
  expect_equal(o$ub(), c(rep(1, (n_pu * n_z) + (n_pu * n_z * n_f)),
                         rep(Inf, nrow(targs))))
  # check that problem matrix is correct
  m <- matrix(0, nrow = (n_pu * n_z * n_f) + nrow(targs) + length(b) + n_pu,
              ncol = (n_pu * n_z) + (n_pu * n_z * n_f) + nrow(targs))
  counter <- 0
  for (z in seq_len(n_z)) {
    for (i in seq_len(n_f)) {
      for (j in seq_len(n_pu)) {
        counter <- counter + 1
        m[counter, ((z - 1) * n_pu) + j] <- -1
        m[counter, (n_pu * n_z) +
                   ((z - 1) * n_pu * n_f) +
                   ((i - 1) * n_pu) +
                   j] <- 1
      }
    }
  }
  for (i in seq_len(nrow(targs))) {
    zs <- match(targs$zone[[i]], zone_names(sim_features_zones))
    f <- match(targs$feature[i], feature_names(sim_features_zones))
    counter <- counter + 1
    for (z in zs) {
      for (pu in seq_len(n_pu)) {
        col <- (n_pu * n_z) + ((z - 1) * n_f * n_pu) +
               ((f - 1) * n_pu) + pu
        m[counter, col] <- p$data$rij[[z]][f, pu]
      }
      col <- (n_pu * n_z) + (n_pu * n_f * n_z) + i
      m[counter, col] <- 1
    }
  }
  counter <- counter + 1
  m[counter, seq_len(n_pu)] <- p$planning_unit_costs()[, 1]
  counter <- counter + 1
  m[counter, n_pu + seq_len(n_pu)] <- p$planning_unit_costs()[, 2]
  counter <- counter + 1
  m[counter, (2 *n_pu) + seq_len(n_pu)] <- p$planning_unit_costs()[, 3]
  for (i in seq_len(n_pu)) {
    m[i + counter, i] <- 1
    m[i + counter, n_pu + i] <- 1
    m[i + counter, (2 * n_pu) + i] <- 1
  }
  m <- as(m, "Matrix")
  expect_true(all(o$A() == m))
})

test_that("solve (expanded formulation, multiple zones, vector budget)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # make and solve problem
  budget <- c(6, 1)
  locked_out <- matrix(FALSE, ncol = 2, nrow = 5)
  locked_out[1, 1] <- TRUE
  targs <- tibble::tibble(
    feature = c("f1", "f2", "f2"),
    zone = list("zone_1", "zone_2", c("zone_1", "zone_2")),
    sense = c(">=", ">=", "<="),
    target = c(5, 1, 300),
    type = c("absolute", "absolute", "absolute"))
  cost <- raster::stack(
    raster::raster(matrix(c(1, 2, 4, NA, NA), nrow = 1)),
    raster::raster(matrix(c(0.5, 1, 1, 1, NA), nrow = 1)))
  features <- raster::stack(
    raster::raster(matrix(c(5,   2,  3,  0,  4), nrow = 1)),
    raster::raster(matrix(c(5,   5,  5,  5,  5), nrow = 1)),
    raster::raster(matrix(c(5,   0,  1,  10, 4), nrow = 1)),
    raster::raster(matrix(c(500, 5,  5,  5,  5), nrow = 1)))
  # create problem
  p <- problem(cost, zones(features[[1:2]], features[[3:4]],
                           zone_names = c("zone_1", "zone_2"),
                           feature_names = c("f1", "f2"))) %>%
       add_min_shortfall_objective(budget = budget) %>%
       add_manual_targets(targs) %>%
       add_locked_out_constraints(locked_out) %>%
       add_default_solver(gap = 0)
  # solve problem
  s <- solve(p, compressed_formulation = FALSE)
  # test that solution is correct
  expect_equal(raster::values(s[[1]]), c(0, 1, 1, NA,  NA))
  expect_equal(raster::values(s[[2]]), c(0, 0, 0, 1, NA))
})

test_that("invalid inptus (multiple zones)", {
  data(sim_pu_zones_stack, sim_features_zones)
  expect_error({
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_min_shortfall_objective(budget = c(1, -5, 1))
  })
  expect_error({
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_min_shortfall_objective(budget = c(1, NA, 1))
  })
  expect_error({
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_min_shortfall_objective(budget = c(NA, NA, NA))
  })
  expect_error({
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_min_shortfall_objective(budget = c(1, Inf, 9))
  })
  expect_error({
    problem(sim_pu_zones_stack, sim_features_zones) %>%
    add_min_shortfall_objective(budget = c(1, Inf, 9))
  })
})
