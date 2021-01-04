context("eval_cost")

test_that("binary values (single zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  p <- problem(matrix(pu$cost, ncol = 1),
               data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               as.matrix(t(pu[, 3:4])))
  # create a solution
  s <- matrix(rep(c(0, 1), 5), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # calculate cost
  r1 <- eval_cost(p, s)
  # create correct result
  r2 <- tibble::tibble(
    statistic = "overall",
    cost = sum(s[, 1] * pu$cost, na.rm = TRUE))
  # run tests
  expect_equal(r1, r2)
})

test_that("binary values (multiple zones)", {
  # simulate data
  pu <- data.frame(id = seq_len(10),
                   cost_1 = c(NA, NA, runif(8)),
                   cost_2 = c(0.3, NA, runif(8)),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  # create problem
  p <- problem(as.matrix(pu[, 2:3]),
               data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7]))))
  # create a solution
  s <- matrix(c(rep(c(0, 1), 5), rep(c(1, 0), 5)), ncol = 2)
  s[is.na(as.matrix(pu[, c("cost_1", "cost_2")]))] <- NA_real_
  # calculate cost
  r1 <- eval_cost(p, s)
  # create correct result
  pos <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  costs <- c(sum(pu$cost_1 * s[, 1], na.rm = TRUE),
             sum(pu$cost_2 * s[, 2], na.rm = TRUE))
  r2 <- tibble::tibble(
    statistic = c("overall", "1", "2"),
    cost = c(sum(costs), costs))
  # run tests
  expect_equal(r1, r2)
})

test_that("proportion values (single zone)", {
  # simulate data
  pu <- data.frame(id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
                   spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  p <- problem(matrix(pu$cost, ncol = 1),
               data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               as.matrix(t(pu[, 3:4])))
  # create a solution
  s <- matrix(runif(10), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # calculate cost
  r1 <- eval_cost(p, s)
  # create correct result
  r2 <- tibble::tibble(
    statistic = "overall",
    cost = sum(s[, 1] * pu$cost, na.rm = TRUE))
  # run tests
  expect_equal(r1, r2)
})

test_that("proportion values (multiple zones)", {
  # simulate data
  pu <- data.frame(id = seq_len(10),
                   cost_1 = c(NA, NA, runif(8)),
                   cost_2 = c(0.3, NA, runif(8)),
                   spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
                   spp1_2 = runif(10), spp2_2 = runif(10))
  # create problem
  p <- problem(as.matrix(pu[, 2:3]),
               data.frame(id = seq_len(2), name = c("spp1", "spp2")),
               list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7]))))
  # create a solution
  s <- matrix(runif(20), ncol = 2)
  s[is.na(as.matrix(pu[, c("cost_1", "cost_2")]))] <- NA_real_
  # calculate cost
  r1 <- eval_cost(p, s)
  # create correct result
  pos <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  costs <- c(sum(pu$cost_1 * s[, 1], na.rm = TRUE),
             sum(pu$cost_2 * s[, 2], na.rm = TRUE))
  r2 <- tibble::tibble(
    statistic = c("overall", "1", "2"),
    cost = c(sum(costs), costs))
  # run tests
  expect_equal(r1, r2)
})
