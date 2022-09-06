context("marxan_boundary_data_to_matrix")

test_that("single zone (x=NULL)", {
  # create data
  d <- expand.grid(id1 = seq_len(4), id2 = c(1, 4))
  d$boundary <- 1
  d$boundary[d$id1 == d$id2] <- 0.5
  # create object
  x <- marxan_boundary_data_to_matrix(NULL, d)
  # create correct result
  x2 <- matrix(0, ncol = 4, nrow = 4)
  x2[1, ] <- 1
  x2[, 1] <- 1
  x2[4, ] <- 1
  x2[, 4] <- 1
  x2[1, 1] <- 0.5
  x2[4, 4] <- 0.5
  # tests
  expect_is(x, "dsCMatrix")
  expect_equal(x, as_Matrix(x2, "dsCMatrix"))
})

test_that("multiple zones (x = NULL)", {
  # create data
  d <- data.frame(
    id1 = c(1, 2, 3, 4, 1, 2),
    id2 = c(1, 1, 1, 2, 2, 2),
    zone1 = c("a", "a", "b", "a", "a", "b"),
    zone2 = c("a", "b", "b", "a", "b", "b"),
    boundary = seq_len(6)
  )
  # create object
  x <- marxan_boundary_data_to_matrix(NULL, d)
  # create correct result
  x2 <- array(0, dim = c(4, 4, 2, 2))
  for (i in seq_len(nrow(d))) {
    curr_z1 <- match(d$zone1[i], c("a", "b"))
    curr_z2 <- match(d$zone2[i], c("a", "b"))
    x2[d$id1[i], d$id2[i], curr_z1, curr_z2] <- d$boundary[i]
  }
  # tests
  expect_is(x, "array")
  expect_equal(x, x2)
})

test_that("single zone (x=ConservationProblem)", {
  # create data
  d <- expand.grid(id1 = seq_len(4), id2 = c(1, 4))
  d$boundary <- 1
  d$boundary[d$id1 == d$id2] <- 0.5
  p <- problem(
    x = runif(5),
    features = data.frame(id = seq_len(2), name = letters[seq_len(2)]),
    rij_matrix = matrix(0, ncol = 5, nrow = 2)
  )
  # create object
  x <- marxan_boundary_data_to_matrix(p, d)
  # create correct result
  x2 <- matrix(0, ncol = 5, nrow = 5)
  x2[1, seq_len(4)] <- 1
  x2[seq_len(4), 1] <- 1
  x2[4, seq_len(4)] <- 1
  x2[seq_len(4), 4] <- 1
  x2[1, 1] <- 0.5
  x2[4, 4] <- 0.5
  # tests
  expect_is(x, "dsCMatrix")
  expect_equal(x, as_Matrix(x2, "dsCMatrix"))
})

test_that("multiple zones (x=ConservationProblem)", {
  # create data
  d <- data.frame(
    id1 = c(1, 2, 3, 4, 1, 2),
    id2 = c(1, 1, 1, 2, 2, 2),
    zone1 = c("a", "a", "b", "a", "a", "b"),
    zone2 = c("a", "b", "b", "a", "b", "b"),
    boundary = seq_len(6)
  )
  p <- problem(
    x = matrix(1, nrow = 5, ncol = 2),
    features = data.frame(id = seq_len(2), name = letters[seq_len(2)]),
    rij_matrix = list(
      "a" = matrix(0, ncol = 5, nrow = 2),
      "b" = matrix(0, ncol = 5, nrow = 2)
    )
  )
  # create object
  x <- marxan_boundary_data_to_matrix(p, d)
  # create correct result
  x2 <- array(0, dim = c(5, 5, 2, 2))
  for (i in seq_len(nrow(d))) {
    curr_z1 <- match(d$zone1[i], c("a", "b"))
    curr_z2 <- match(d$zone2[i], c("a", "b"))
    x2[d$id1[i], d$id2[i], curr_z1, curr_z2] <- d$boundary[i]
  }
  # tests
  expect_is(x, "array")
  expect_equal(x, x2)
})
