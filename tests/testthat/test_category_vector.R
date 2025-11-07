test_that("matrix (binary)", {
  # create data
  x <- matrix(c(1, 0, 0, NA, 0, 1, 0, NA, 0, 0, 0, NA), ncol = 3)
  # create category vector
  y <- category_vector(x)
  # run tests
  expect_equal(y, c(1, 2, 0, NA))
})

test_that("matrix (continuous)", {
  # create data
  x <- matrix(c(0.9, 0.2, 0, NA, 0.3, 0.3, 0, NA, 0, 0, 0, NA), ncol = 3)
  # create category vector
  y <- category_vector(x)
  # run tests
  expect_equal(y, c(1, 2, 0, NA))
})

test_that("data.frame", {
  # create data
  x <- as.data.frame(matrix(c(1, 0, 0, NA, 0, 1, 0, NA, 0, 0, 0, NA), ncol = 3))
  # create category vector
  y <- category_vector(x)
  # run tests
  expect_equal(y, c(1, 2, 0, NA))
})

test_that("Spatial", {
  # create data
  sim_pu_polygons <- get_sim_pu_polygons()
  x <- sf::as_Spatial(sim_pu_polygons[seq_len(4), ])
  x$V1 <- c(1, 0, 0, NA)
  x$V2 <- c(0, 1, 0, NA)
  x$V3 <- c(0, 0, 0, NA)
  # create category vector
  expect_warning(
    y <- category_vector(x[, c("V1", "V2", "V3")]),
    "deprecated"
  )
  # run tests
  expect_equal(y, c(1, 2, 0, NA))
})

test_that("sf", {
  # create data
  sim_pu_polygons <- get_sim_pu_polygons()
  x <- sim_pu_polygons[seq_len(4), ]
  x$V1 <- c(1, 0, 0, NA)
  x$V2 <- c(0, 1, 0, NA)
  x$V3 <- c(0, 0, 0, NA)
  # create category vector
  y <- category_vector(x[, c("V1", "V2", "V3")])
  # run tests
  expect_equal(y, c(1, 2, 0, NA))
})

test_that("invalid inputs", {
  expect_tidy_error(category_vector(data.frame(integer(0), integer(0))))
  expect_tidy_error(category_vector(data.frame(a = 1, b = "a")))
  expect_tidy_error(category_vector(matrix(c("a", "b"), ncol = 2)))
})
