context("replacement_cost")

test_that("numeric", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # create data
  pu <- data.frame(id = seq_len(4),
                   cost = c(10, 2, NA, 3),
                   spp1 = c(0, 0, 0, 1),
                   spp2 = c(10, 5, 10, 6))
  # create problem
  p <-
    problem(pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
            as.matrix(t(pu[, 3:4]))) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = TRUE)
  # create a solution
  s <- c(0, 1, NA, 1)
  # calculate replacement costs
  r <- replacement_cost(p, s)
  # create correct result
  r2 <- c(0, 8, NA, Inf)
  # run tests
  expect_equal(r, r2)
})

test_that("matrix (single zone)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # create data
  pu <- data.frame(id = seq_len(4),
                   cost = c(10, 2, NA, 3),
                   spp1 = c(0, 0, 0, 1),
                   spp2 = c(10, 5, 10, 6))
  # create problem
  p <-
    problem(matrix(pu$cost, ncol = 1),
            data.frame(id = seq_len(2), name = c("spp1", "spp2")),
            as.matrix(t(pu[, 3:4]))) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- matrix(c(0, 1, NA, 1), ncol = 1)
  # calculate replacement costs
  r <- replacement_cost(p, s)
  # create correct result
  r2 <- matrix(c(0, 8, NA, Inf), ncol = 1)
  # run tests
  expect_equal(r, r2)
})

test_that("matrix (multiple zones)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # create data
  pu <- data.frame(id = seq_len(7),
                   cost_1 = c(1,  2,  NA, 3, 100, 100, NA),
                   cost_2 = c(10, 10, 10, 10,  4,   1, NA),
                   spp1_1 = c(1,  2, 0, 0, 0, 0,  0),
                   spp2_1 = c(NA, 0, 1, 1, 0, 0,  0),
                   spp1_2 = c(1,  0, 0, 0, 1, 0,  0),
                   spp2_2 = c(0,  0, 0, 0, 0, 10, 0))
  targets <- matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2)
  # create problem
  p <-
    problem(as.matrix(pu[, c(2, 3)]),
            data.frame(id = seq_len(2), name = c("spp1", "spp2")),
            list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- matrix(c(1, 0, NA, 1, 0, 0, NA, 0, 0, 0, 0, 1, 0, NA), ncol = 2)
  # calculate replacement costs
  r <- replacement_cost(p, s)
  # create correct result
  r2 <- matrix(c(1, 0, NA, Inf, 0, 0, NA, 0, 0, 0, 0, 7, 0, NA), ncol = 2)
  colnames(r2) <- names(pu)[c(2, 3)]
  # run tests
  expect_equal(r, r2)
})

test_that("data.frame (single zone)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(any_solvers_installed())
  # create data
  pu <- data.frame(id = seq_len(4),
                   cost = c(10, 2, NA, 3),
                   spp1 = c(0, 0, 0, 1),
                   spp2 = c(10, 5, 10, 6))
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
  r <- replacement_cost(p, s)
  # create correct result
  r2 <- tibble::tibble(cost = c(0, 8, NA, Inf))
  # run tests
  expect_equal(r, r2)
})

test_that("data.frame (multiple zone)", {
    skip_on_cran()
    skip_on_travis()
    skip_on_appveyor()
    skip_if_not(any_solvers_installed())
})

test_that("Spatial (single zone)", {
    skip_on_cran()
    skip_on_travis()
    skip_on_appveyor()
    skip_if_not(any_solvers_installed())

})

test_that("Spatial (multiple zone)", {
    skip_on_cran()
    skip_on_travis()
    skip_on_appveyor()
    skip_if_not(any_solvers_installed())

})

test_that("Raster (single zone)", {
    skip_on_cran()
    skip_on_travis()
    skip_on_appveyor()
    skip_if_not(any_solvers_installed())

})

test_that("Raster (multiple zone)", {
    skip_on_cran()
    skip_on_travis()
    skip_on_appveyor()
    skip_if_not(any_solvers_installed())

})

test_that("invalid inputs", {
})
