test_that("binary values (single zone)", {
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  p <- problem(
    matrix(pu$cost, ncol = 1),
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    as.matrix(t(pu[, 3:4]))
  )
  # create a solution
  s <- matrix(rep(c(0, 1), 5), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # calculate cost
  r1 <- eval_cost_summary(p, s)
  # create correct result
  r2 <- tibble::tibble(
    summary = "overall",
    cost = sum(s[, 1] * pu$cost, na.rm = TRUE)
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("binary values (multiple zones)", {
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)),
    cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10),
    spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10),
    spp2_2 = runif(10)
  )
  # create problem
  p <- problem(
    as.matrix(pu[, 2:3]),
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))
  )
  # create a solution
  s <- matrix(c(rep(c(0, 1), 5), rep(c(1, 0), 5)), ncol = 2)
  s[is.na(as.matrix(pu[, c("cost_1", "cost_2")]))] <- NA_real_
  # calculate cost
  r1 <- eval_cost_summary(p, s)
  # create correct result
  pos <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  costs <- c(
    sum(pu$cost_1 * s[, 1], na.rm = TRUE),
    sum(pu$cost_2 * s[, 2], na.rm = TRUE)
  )
  r2 <- tibble::tibble(
    summary = c("overall", "1", "2"),
    cost = c(sum(costs), costs)
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("proportion values (single zone)", {
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  p <- problem(
    matrix(pu$cost, ncol = 1),
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    as.matrix(t(pu[, 3:4]))
  )
  # create a solution
  s <- matrix(runif(10), ncol = 1)
  s[is.na(pu$cost)] <- NA_real_
  # calculate cost
  r1 <- eval_cost_summary(p, s)
  # create correct result
  r2 <- tibble::tibble(
    summary = "overall",
    cost = sum(s[, 1] * pu$cost, na.rm = TRUE)
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("proportion values (multiple zones)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)),
    cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10),
    spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10),
    spp2_2 = runif(10)
  )
  # create problem
  p <- problem(
    as.matrix(pu[, 2:3]),
    data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    list(as.matrix(t(pu[, 4:5])), as.matrix(t(pu[, 6:7])))
  )
  # create a solution
  s <- matrix(runif(20), ncol = 2)
  s[is.na(as.matrix(pu[, c("cost_1", "cost_2")]))] <- NA_real_
  # calculate cost
  r1 <- eval_cost_summary(p, s)
  # create correct result
  pos <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  costs <- c(
    sum(pu$cost_1 * s[, 1], na.rm = TRUE),
    sum(pu$cost_2 * s[, 2], na.rm = TRUE)
  )
  r2 <- tibble::tibble(
    summary = c("overall", "1", "2"),
    cost = c(sum(costs), costs)
  )
  # run tests
  expect_equal(r1, r2)
})

test_that("multi_problem (single zone)", {
  # create data
  pu <- data.frame(
    id = seq_len(10),
    cost1 = c(0.2, NA_real_, runif(8)),
    cost2 = c(0.9, NA_real_, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA)
  )
  # create problem
  mp <-
    multi_problem(
      obj1 =
        problem(
          matrix(pu$cost1, ncol = 1),
          data.frame(id = seq_len(2), name = c("spp1", "spp2")),
          as.matrix(t(pu[, 3:4])
        )
      ) %>%
      add_max_wtd_sum_objective(1000) %>%
      add_binary_decisions(),
      obj2 =
        problem(
          matrix(pu$cost2, ncol = 1),
          data.frame(id = seq_len(2), name = c("spp1", "spp2")),
          as.matrix(t(pu[, 3:4])
        )
      ) %>%
      add_max_wtd_sum_objective(1000) %>%
      add_binary_decisions()
    )
  # create a solution
  s <- matrix(rep(c(0, 1), 5), ncol = 1)
  s[is.na(pu$cost1)] <- NA_real_
  # calculate cost
  x <- eval_cost_summary(mp, s)
  y <- tibble::tibble(
    problem = problem_names(mp),
    summary = "overall",
    cost = c(sum(pu$cost1 * s, na.rm = TRUE), sum(pu$cost2 * s, na.rm = TRUE))
  )
  # run tests
  expect_equal(x, y)
})

test_that("multi_problem (multiple zones)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost_11 = c(NA, NA, runif(8)),
    cost_12 = c(0.3, NA, runif(8)),
    cost_21 = c(NA, NA, runif(8)),
    cost_22 = c(0.8, NA, runif(8)),
    spp1_1 = runif(10),
    spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10),
    spp2_2 = runif(10)
  )
  # create problem
  mp <-
    multi_problem(
      obj1 =
        problem(
          as.matrix(pu[, 2:3]),
          data.frame(id = seq_len(2), name = c("spp1", "spp2")),
          list(as.matrix(t(pu[, 6:7])), as.matrix(t(pu[, 8:9])))
        ) %>%
        add_max_wtd_sum_objective(1000) %>%
        add_binary_decisions(),
      obj2 =
        problem(
          as.matrix(pu[, 4:5]),
          data.frame(id = seq_len(2), name = c("spp1", "spp2")),
          list(as.matrix(t(pu[, 6:7])), as.matrix(t(pu[, 8:9])))
        ) %>%
        add_max_wtd_sum_objective(1000) %>%
        add_binary_decisions()
    )
  # create a solution
  s <- matrix(runif(20), ncol = 2)
  s[is.na(as.matrix(pu[, c("cost_11", "cost_12")]))] <- NA_real_
  # calculate cost
  x <- eval_cost_summary(mp, s)
  y <- tibble::as_tibble(
    rbind(
      cbind(
        data.frame(problem = "obj1"),
        eval_cost_summary(mp$problems[[1]], s)
      ),
      cbind(
        data.frame(problem = "obj2"),
        eval_cost_summary(mp$problems[[2]], s)
      )
    )
  )
  # run tests
  expect_equal(x, y)
})
