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

})

test_that("multi_problem() (multiple zones)", {

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

})
