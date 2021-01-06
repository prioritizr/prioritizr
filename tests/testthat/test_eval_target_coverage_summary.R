context("eval_target_coverage_summary")

test_that("binary values (single zone)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  p <- problem(
    pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    as.matrix(t(pu[, 3:4]))) %>%
    add_relative_targets(c(0.3, 0.8))
  # create a solution
  s <- rep(c(0, 1), 5)
  s[is.na(pu$cost)] <- NA_real_
  # calculate target coverage
  r1 <- eval_target_coverage_summary(p, s)
  # create correct result
  r2 <- tibble::tibble(
    feature = c("spp1", "spp2"),
    total_amount = c(
      sum(pu$spp1, na.rm = TRUE),
      sum(pu$spp2, na.rm = TRUE)),
    absolute_target = total_amount * c(0.3, 0.8),
    absolute_held = c(
      sum(s * pu$spp1, na.rm = TRUE),
      sum(s * pu$spp2, na.rm = TRUE)),
    absolute_shortfall =
      ifelse(absolute_held > absolute_target, c(0, 0),
      absolute_target - absolute_held),
    relative_target = c(0.3, 0.8),
    relative_held = absolute_held / total_amount,
    relative_shortfall = absolute_shortfall / total_amount,
    met = absolute_shortfall < 1e-10)
  r2 <- r2[, c("feature", "met", "total_amount",
               "absolute_target", "absolute_held", "absolute_shortfall",
               "relative_target", "relative_held", "relative_shortfall")]
  # run tests
  expect_equal(r1, r2)
})

test_that("proportion values (single zone)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10), spp2 = c(rpois(9, 4), NA))
  # create problem
  p <- problem(
    pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
    as.matrix(t(pu[, 3:4]))) %>%
    add_relative_targets(c(0.3, 0.8))
  # create a solution
  s <- runif(10)
  s[is.na(pu$cost)] <- NA_real_
  # calculate target coverage
  r1 <- eval_target_coverage_summary(p, s)
  # create correct result
  r2 <- tibble::tibble(
    feature = c("spp1", "spp2"),
    total_amount = c(
      sum(pu$spp1, na.rm = TRUE),
      sum(pu$spp2, na.rm = TRUE)),
    absolute_target = total_amount * c(0.3, 0.8),
    absolute_held = c(
      sum(s * pu$spp1, na.rm = TRUE),
      sum(s * pu$spp2, na.rm = TRUE)),
    absolute_shortfall =
      ifelse(absolute_held > absolute_target, c(0, 0),
      absolute_target - absolute_held),
    relative_target = c(0.3, 0.8),
    relative_held = absolute_held / total_amount,
    relative_shortfall = absolute_shortfall / total_amount,
    met = absolute_shortfall < 1e-10)
  r2 <- r2[, c("feature", "met", "total_amount",
               "absolute_target", "absolute_held", "absolute_shortfall",
               "relative_target", "relative_held", "relative_shortfall")]
  # run tests
  expect_equal(r1, r2)
})

test_that("binary values (multiple zones)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)),
    cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10), spp2_2 = runif(10),
    s1 = c(NA, NA, rep(c(0, 1), 4)),
    s2 = c(1, NA, rep(c(1, 0), 4)))
  targets <- tibble::tibble(
    feature = c("spp1", "spp2"),
    zone = list(c("z1", "z2"), c("z2")),
    sense = ">=", type = "absolute",
    target = c(6, 10))
  # create problem
  p <- problem(
    pu, cost_column = c("cost_1", "cost_2"),
    zones(z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
          feature_names = c("spp1", "spp2"))) %>%
    add_manual_targets(targets)
  # calculate target coverage
  r1 <- eval_target_coverage_summary(p, pu[, c("s1", "s2")])
  # create correct result
  idx <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  r2 <- tibble::tibble(
    feature = c("spp1", "spp2"),
    zone = targets$zone,
    sense = targets$sense,
    total_amount = c(
      sum(pu$spp1_1, pu$spp1_2, na.rm = TRUE),
      sum(pu$spp2_2, na.rm = TRUE)),
    absolute_target = targets$target,
    absolute_held = c(
      sum(c(pu$s1 * pu$spp1_1)[idx], c(pu$s2 * pu$spp1_2)[idx], na.rm = TRUE),
      sum(pu$s2 * pu$spp2_2, na.rm = TRUE)),
    absolute_shortfall =
      ifelse(absolute_held > absolute_target, c(0, 0),
      absolute_target - absolute_held),
    relative_target = absolute_target / total_amount,
    relative_held = absolute_held / total_amount,
    relative_shortfall = absolute_shortfall / total_amount,
    met = absolute_shortfall < 1e-10)
  r2 <- r2[, c("feature", "zone", "sense", "met", "total_amount",
               "absolute_target", "absolute_held", "absolute_shortfall",
               "relative_target", "relative_held", "relative_shortfall")]
  # run tests
  expect_equal(r1, r2)
})

test_that("proportion values (multiple zones)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10),
    cost_1 = c(NA, NA, runif(8)),
    cost_2 = c(0.3, NA, runif(8)),
    spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
    spp1_2 = runif(10), spp2_2 = runif(10),
    s1 = c(NA, NA, runif(8)),
    s2 = c(1, NA, runif(8)))
  targets <- tibble::tibble(
    feature = c("spp1", "spp2"),
    zone = list(c("z1", "z2"), c("z2")),
    sense = ">=", type = "absolute",
    target = c(6, 10))
  # create problem
  p <- problem(
    pu, cost_column = c("cost_1", "cost_2"),
    zones(z1 = c("spp1_1", "spp2_1"), z2 = c("spp1_2", "spp2_2"),
          feature_names = c("spp1", "spp2"))) %>%
    add_manual_targets(targets)
  # calculate target coverage
  r1 <- eval_target_coverage_summary(p, pu[, c("s1", "s2")])
  # create correct result
  idx <- which(!is.na(pu$cost_1) | !is.na(pu$cost_2))
  r2 <- tibble::tibble(
    feature = c("spp1", "spp2"),
    zone = targets$zone,
    sense = targets$sense,
    total_amount = c(
      sum(pu$spp1_1, pu$spp1_2, na.rm = TRUE),
      sum(pu$spp2_2, na.rm = TRUE)),
    absolute_target = targets$target,
    absolute_held = c(
      sum(c(pu$s1 * pu$spp1_1)[idx], c(pu$s2 * pu$spp1_2)[idx], na.rm = TRUE),
      sum(pu$s2 * pu$spp2_2, na.rm = TRUE)),
    absolute_shortfall =
      ifelse(absolute_held > absolute_target, c(0, 0),
      absolute_target - absolute_held),
    relative_target = absolute_target / total_amount,
    relative_held = absolute_held / total_amount,
    relative_shortfall = absolute_shortfall / total_amount,
    met = absolute_shortfall < 1e-10)
  r2 <- r2[, c("feature", "zone", "sense", "met", "total_amount",
               "absolute_target", "absolute_held", "absolute_shortfall",
               "relative_target", "relative_held", "relative_shortfall")]
  # run tests
  expect_equal(r1, r2)
})

test_that("binary values (single zone, variable target sense, none met)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(10), cost = c(0.2, NA_real_, runif(8)),
    spp1 = runif(10),
    spp2 = c(rpois(9, 4), NA),
    spp3 = runif(10) ^ 2,
    s = c(1, NA, rep(c(0, 1), 4)))
  # simulate targets
  targets <- tibble::tibble(
    feature = c("spp1", "spp2", "spp3"),
    sense = c(">=", "<=", "="),
    type = rep("relative", 3),
    target = c(0.8, 0.05, 0.01))
  # create problem
  p <- problem(pu, targets$feature, "cost") %>%
       add_manual_targets(targets)
  # calculate target coverage
  r1 <- eval_target_coverage_summary(p, pu[, "s", drop = FALSE])
  # create correct result
  r2 <- tibble::tibble(
    feature = targets$feature,
    total_amount = c(
      sum(pu$spp1, na.rm = TRUE),
      sum(pu$spp2, na.rm = TRUE),
      sum(pu$spp3, na.rm = TRUE)),
    absolute_target = total_amount * targets$target,
    absolute_held = c(
      sum(pu$s * pu$spp1, na.rm = TRUE),
      sum(pu$s * pu$spp2, na.rm = TRUE),
      sum(pu$s * pu$spp3, na.rm = TRUE)),
    absolute_shortfall = c(
      max(absolute_target[1] - absolute_held[1], 0),
      absolute_held[2] - absolute_target[2],
      abs(absolute_held[3] - absolute_target[3])),
    relative_target = targets$target,
    relative_held = absolute_held / total_amount,
    relative_shortfall = absolute_shortfall / total_amount,
    met = absolute_shortfall < 1e-10)
  r2 <- r2[, c("feature", "met", "total_amount",
               "absolute_target", "absolute_held", "absolute_shortfall",
               "relative_target", "relative_held", "relative_shortfall")]
  # run tests
  expect_equal(r1, r2)
})

test_that("binary values (single zone, variable target sense, all met)", {
  # simulate data
  pu <- data.frame(
    id = seq_len(3),
    cost = c(0.2, NA_real_, 5),
    spp1 = c(0.1, 0.2, 0.4),
    spp2 = c(0.1, 0.2, 0.01),
    spp3 = c(5, 10, 12),
    s = c(0, NA, 1))
  # simulate targets
  targets <- tibble::tibble(
    feature = c("spp1", "spp2", "spp3"),
    sense = c(">=", "<=", "="),
    type = rep("absolute", 3),
    target = c(0.35, 0.05, 12))
  # create problem
  p <- problem(pu, targets$feature, "cost") %>%
       add_manual_targets(targets)
  # calculate target coverage
  r1 <- eval_target_coverage_summary(p, pu[, "s", drop = FALSE])
  # create correct result
  r2 <- tibble::tibble(
    feature = targets$feature,
    total_amount = c(
      sum(pu$spp1, na.rm = TRUE),
      sum(pu$spp2, na.rm = TRUE),
      sum(pu$spp3, na.rm = TRUE)),
    absolute_target = targets$target,
    absolute_held = c(
      sum(pu$s * pu$spp1, na.rm = TRUE),
      sum(pu$s * pu$spp2, na.rm = TRUE),
      sum(pu$s * pu$spp3, na.rm = TRUE)),
    absolute_shortfall = rep(0, 3),
    relative_target = absolute_target / total_amount,
    relative_held = absolute_held / total_amount,
    relative_shortfall = absolute_shortfall / total_amount,
    met = TRUE)
  r2 <- r2[, c("feature", "met", "total_amount",
               "absolute_target", "absolute_held", "absolute_shortfall",
               "relative_target", "relative_held", "relative_shortfall")]
  # run tests
  expect_equal(r1, r2)
})
