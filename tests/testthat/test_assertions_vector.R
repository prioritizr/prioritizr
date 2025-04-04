test_that("is_match_of", {
  expect_true(is_match_of(1, c(1, 2)))
  expect_true(is_match_of("1", c("1", "2")))
  expect_true(is_match_of(1L, c(1L, 2L)))
  expect_false(is_match_of(1, c(0, 2)))
  expect_false(is_match_of("1", c("0", "2")))
  expect_false(is_match_of(1L, c(0L, 2L)))
  expect_error(assert(is_match_of(1, c(0, 2))), "must be")
  expect_error(assert(is_match_of(1, seq(3, 300))), "values")
})

test_that("all_match_of", {
  expect_true(all_match_of(c(1, 2), c(1, 2, 3)))
  expect_true(all_match_of(c("1", "2"), c("1", "2", "3")))
  expect_true(all_match_of(c(1L, 2L), c(1L, 2L, 3L)))
  expect_false(all_match_of(c(1, 4), c(0, 2)))
  expect_false(all_match_of(c("1", "4"), c("0", "2")))
  expect_false(all_match_of(c(1L, 4L), c(0L, 2L)))
  expect_error(assert(all_match_of(c(1, 4), c(0, 2))), "must be")
  expect_error(assert(all_match_of(c(1, 4), seq(3, 300))), "values")
})

test_that("no_duplicates", {
  expect_true(no_duplicates(c(1, 2, 3)))
  expect_true(no_duplicates(c("1", "2", "3")))
  expect_true(no_duplicates(c(1L, 2L, 3L)))
  expect_false(no_duplicates(c(1, 1, 2)))
  expect_false(no_duplicates(c("1", "1", "2")))
  expect_false(no_duplicates(c(1L, 1L, 2L)))
  expect_error(assert(no_duplicates(c(1, 1, 2))), "duplicate")
})

test_that("is_count_vector", {
  expect_true(is_count_vector(c(1, 2, 3)))
  expect_true(is_count_vector(c(1L, 2L, 3L)))
  expect_false(is_count_vector(c(-1, 1, 2, 3)))
  expect_false(is_count_vector(c(-1L, 1L, 2L, 3L)))
  expect_error(assert(is_count_vector(c("1", "2", "3"))), "integer")
  expect_error(assert(is_count_vector(c(-1, 1, 2, 3))), "integer")
})

test_that("all_is_valid_total_unit_ids (data.frame)", {
  # import data
  sim_pu_data <- get_sim_pu_polygons()
  sim_pu_data <- sf::st_drop_geometry(sim_pu_data)[1:5, , drop = FALSE]
  sim_pu_data$id <- c(1, 3, 90, 5, 2)
  sim_pu_data$cost <- c(1, NA, 3, 4, 8)
  sim_pu_data$spp_1 <- runif(5)
  sim_pu_data$spp_2 <- runif(5)
  sim_pu_data$spp_3 <- runif(5)
  # create problem
  p <- problem(sim_pu_data, c("spp_1", "spp_2", "spp_3"), cost_column = "cost")
  # tests
  expect_true(all_is_valid_total_unit_ids(p, 3))
  expect_true(all_is_valid_total_unit_ids(p, 90))
  expect_true(all_is_valid_total_unit_ids(p, c(3, 90)))
  expect_false(all_is_valid_total_unit_ids(p, 1000))
  expect_false(all_is_valid_total_unit_ids(p, c(3, 90, 1000)))
  expect_false(all_is_valid_total_unit_ids(p, c(3, 90, NA_real_)))
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "valid identifiers"
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, c(3, 90, 1000))),
    "valid identifiers"
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, c(3, 90, NA_real_))),
    "valid identifiers"
  )
})

test_that("all_is_valid_total_unit_ids (sf)", {
  # import data
  sim_pu_data <- get_sim_pu_polygons()
  sim_pu_data <- sim_pu_data[1:5, , drop = FALSE]
  sim_pu_data$cost <- c(1, NA, 3, 4, 8)
  sim_pu_data$spp_1 <- runif(5)
  sim_pu_data$spp_2 <- runif(5)
  sim_pu_data$spp_3 <- runif(5)
  # create problem
  p <- problem(sim_pu_data, c("spp_1", "spp_2", "spp_3"), cost_column = "cost")
  # tests
  expect_true(all_is_valid_total_unit_ids(p, 1))
  expect_true(all_is_valid_total_unit_ids(p, 2))
  expect_true(all_is_valid_total_unit_ids(p, c(1, 2)))
  expect_false(all_is_valid_total_unit_ids(p, 1000))
  expect_false(all_is_valid_total_unit_ids(p, c(1, 2, 1000)))
  expect_false(all_is_valid_total_unit_ids(p, c(1, 2, NA_real_)))
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "valid identifiers"
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, c(1, 2, 1000))),
    "valid identifiers"
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, c(1, 2, NA_real_))),
    "valid identifiers"
  )
})

test_that("all_is_valid_total_unit_ids (assorted data types)", {
  # import data
  sim_pu_data <- get_sim_pu_polygons()
  sim_pu_data <- sim_pu_data[1:5, , drop = FALSE]
  sim_pu_data$id <- seq_len(5)
  sim_pu_data$spp_1 <- runif(5)
  sim_pu_data$spp_2 <- runif(5)
  # tests
  ## sf
  p <- problem(sim_pu_data, c("spp_1", "spp_2"), cost_column = "cost")
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "row numbers"
  )
  ## Spatial
  p <- suppressWarnings(problem(
    sf::as_Spatial(sim_pu_data),
    c("spp_1", "spp_2"),
    cost_column = "cost"
  ))
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "row numbers"
  )
  ## data.frame
  p <- problem(
    sf::st_drop_geometry(sim_pu_data),
    c("spp_1", "spp_2"),
    cost_column = "cost"
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "column"
  )
  ## matrix
  p <- problem(
    matrix(sim_pu_data$cost, ncol = 1),
    data.frame(id = seq_len(2), name = c("spp_1", "spp_2")),
    as.matrix(t(sf::st_drop_geometry(sim_pu_data[, c("spp_1", "spp_2")])))
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "row numbers"
  )
  ## numeric
  suppressMessages(
    p <- problem(
      sim_pu_data$cost,
      data.frame(id = seq_len(2), name = c("spp_1", "spp_2")),
      as.matrix(t(sf::st_drop_geometry(sim_pu_data[, c("spp_1", "spp_2")])))
    )
  )
  expect_error(
    assert(all_is_valid_total_unit_ids(p, 1000)),
    "row numbers"
  )
})
