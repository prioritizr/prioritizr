test_that("problem() (single zone)", {
  skip_on_cran()
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  p <-
    problem(
      matrix(pu$cost, ncol = 1),
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(1) %>%
    add_default_solver(verbose = FALSE)
  # create a solution
  s <- matrix(rep(c(0, 1), 5), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # run calculations
  x <- eval_objective_summary(p, s)
  # create correct result
  y <- tibble::tibble(
    value = sum(s[, 1] * pu$cost, na.rm = TRUE)
  )
  # run tests
  expect_equal(x, y)
})

test_that("multi_problem() (single zone)", {
  skip_on_cran()
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  mp <-
    multi_problem(
      obj1 = problem(
        matrix(pu$cost, ncol = 1),
        data.frame(id = seq_len(2), name = c("spp1", "spp2")),
        as.matrix(t(pu[, 3:4]))
      ) %>%
      add_min_set_objective() %>%
      add_absolute_targets(1),
      obj2 = problem(
        matrix(pu$cost, ncol = 1),
        data.frame(id = seq_len(2), name = c("spp1", "spp2")),
        as.matrix(t(pu[, 3:4]))
      ) %>%
      add_max_wtd_sum_objective(budget = 100) %>%
      add_feature_weights(c(20, 40))
    ) %>%
    add_default_solver(verbose = FALSE)
  # create a solution
  s <- matrix(rep(c(0, 1), 5), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # run calculations
  x <- eval_objective_summary(mp, s)
  # create correct result
  y <- tibble::tibble(
    problem = c("obj1", "obj2"),
    value = c(
      sum(s[, 1] * pu$cost, na.rm = TRUE),
      sum(
        sum(s[, 1] * pu[, 3] * 20, na.rm = TRUE),
        sum(s[, 1] * pu[, 4] * 40, na.rm = TRUE),
        na.rm = TRUE
      )
    )
  )
  # run tests
  expect_equal(x, y)
})

test_that("problem() (multiple zones)", {
  skip_on_cran()
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)),
    cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10), spp2_2 = runif(10)
  )
  # create problem
  p <-
    problem(
      as.matrix(pu[, c("cost_1", "cost_2")]),
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(c(1, 1, 1, 1), nrow = 2, ncol = 2)) %>%
    add_default_solver(verbose = FALSE)
  # create a solution
  s <- matrix(c(rep(c(0, 1), 5), rep(c(1, 0), 5)), ncol = 2)
  s[is.na(as.matrix(pu[, c("cost_1", "cost_2")]))] <- NA_real_
  # run calculations
  x <- eval_objective_summary(p, s)
  # create correct result
  y <- tibble::tibble(
    value = sum(
      sum(s[, 1] * pu$cost_1, na.rm = TRUE),
      sum(s[, 2] * pu$cost_2, na.rm = TRUE),
      na.rm = TRUE
    )
  )
  # run tests
  expect_equal(x, y)
})

test_that("multi_problem() (multiple zones)", {
  skip_on_cran()
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)),
    cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10), spp2_2 = runif(10)
  )
  # create multi-objective problem
  mp <-
    multi_problem(
      obj1 = problem(
        as.matrix(pu[, c("cost_1", "cost_2")]),
        data.frame(id = seq_len(2), name = c("spp1", "spp2")),
        list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))
      ) %>%
        add_min_set_objective() %>%
        add_absolute_targets(matrix(c(1, 1, 1, 1), nrow = 2, ncol = 2)),
      obj2 = problem(
        as.matrix(pu[, c("cost_1", "cost_2")]),
        data.frame(id = seq_len(2), name = c("spp1", "spp2")),
        list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))
      ) %>%
        add_max_wtd_sum_objective(budget = 100) 
    ) %>%
    add_default_solver(verbose = FALSE)
  # create a solution
  s <- matrix(c(rep(c(0, 1), 5), rep(c(1, 0), 5)), ncol = 2)
  s[is.na(as.matrix(pu[, c("cost_1", "cost_2")]))] <- NA_real_
  # run calculations
  x <- eval_objective_summary(mp, s)
  # create correct result
  y <- tibble::tibble(
    problem = c("obj1", "obj2"),
    value = c(
      sum(
        sum(s[, 1] * pu$cost_1, na.rm = TRUE),
        sum(s[, 2] * pu$cost_2, na.rm = TRUE),
        na.rm = TRUE
      ),
      eval_objective_summary(mp$problems[[2]], s)$value
    )
  )
  # run tests
  expect_equal(x, y)
})

test_that("include_penalties = FALSE", {
  skip_on_cran()
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  p <-
    problem(
      matrix(pu$cost, ncol = 1),
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    ) %>%
    add_min_set_objective() %>%
    add_linear_penalties(4, pu$spp1) %>%
    add_absolute_targets(1) %>%
    add_default_solver(verbose = FALSE)
  # create a solution
  s <- matrix(rep(c(0, 1), 5), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # run calculations
  x <- eval_objective_summary(p, s, include_penalties = FALSE)
  # run calculations
  y <- tibble::tibble(
    value = sum(s[, 1] * pu$cost, na.rm = TRUE)
  )
  # run tests
  expect_equal(x, y)
})

test_that("include_penalties = TRUE", {
  skip_on_cran()
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  p <-
    problem(
      matrix(pu$cost, ncol = 1),
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    ) %>%
    add_min_set_objective() %>%
    add_linear_penalties(4, pu$spp1) %>%
    add_absolute_targets(1) %>%
    add_default_solver(verbose = FALSE)
  # create a solution
  s <- matrix(rep(c(0, 1), 5), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # run calculations
  x <- eval_objective_summary(p, s, include_penalties = TRUE)
  # run calculations
  y <- tibble::tibble(
  value =
    sum(s[, 1] * pu$cost, na.rm = TRUE) +
    sum(s[, 1] * pu$spp1 * 4, na.rm = TRUE)
  )
  # run tests
  expect_equal(x, y)
})

test_that("invalid inputs", {
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  p <-
    problem(
      matrix(pu$cost, ncol = 1),
      data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(1) %>%
    add_default_solver(verbose = FALSE)
  # create a solution
  s <- matrix(rep(c(0, 1), 5), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # run tests
  expect_error(eval_objective_summary(NULL, s))
  expect_tidy_error(eval_objective_summary(p, NULL), "solution")
  expect_tidy_error(eval_objective_summary(p, s, include_penalties = NA), "include_penalties")
  expect_tidy_error(eval_objective_summary(p, s, include_penalties = "yes"), "include_penalties")
})
