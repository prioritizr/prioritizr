test_that("single zone (x = NULL, symmetric = TRUE)", {
  # create data
  d <- expand.grid(id1 = seq_len(4), id2 = c(1, 4))
  d$boundary <- 1
  d$boundary[d$id1 == d$id2] <- 0.5
  # create object
  x <- marxan_connectivity_data_to_matrix(NULL, d, symmetric = TRUE)
  # create correct result
  x2 <- matrix(0, ncol = 4, nrow = 4)
  x2[1, ] <- 1
  x2[, 1] <- 1
  x2[4, ] <- 1
  x2[, 4] <- 1
  x2[1, 1] <- 0.5
  x2[4, 4] <- 0.5
  # tests
  expect_inherits(x, "dsCMatrix")
  expect_equal(x, as_Matrix(x2, "dsCMatrix"))
})

test_that("multiple zones (x = NULL, symmetric = TRUE)", {
  # create data
  d <- data.frame(
    id1 = c(1, 2, 3, 4, 1, 2),
    id2 = c(1, 1, 1, 2, 2, 2),
    zone1 = c("a", "a", "b", "a", "a", "b"),
    zone2 = c("a", "b", "b", "a", "b", "b"),
    boundary = seq_len(6)
  )
  # create object
  x <- marxan_connectivity_data_to_matrix(NULL, d, symmetric = TRUE)
  # create correct result
  x2 <- array(0, dim = c(4, 4, 2, 2))
  for (i in seq_len(nrow(d))) {
    curr_z1 <- match(d$zone1[i], c("a", "b"))
    curr_z2 <- match(d$zone2[i], c("a", "b"))
    x2[d$id1[i], d$id2[i], curr_z1, curr_z2] <- d$boundary[i]
  }
  # tests
  expect_inherits(x, "array")
  expect_equal(x, x2)
})

test_that("multiple zones (x = NULL, symmetric = TRUE, zone = factor)", {
  # create data
  d <- data.frame(
    id1 = c(1, 2, 3, 4, 1, 2),
    id2 = c(1, 1, 1, 2, 2, 2),
    zone1 = factor(c("a", "a", "b", "a", "a", "b")),
    zone2 = factor(c("a", "b", "b", "a", "b", "b")),
    boundary = seq_len(6)
  )
  # create object
  x <- marxan_connectivity_data_to_matrix(NULL, d, symmetric = TRUE)
  # create correct result
  x2 <- array(0, dim = c(4, 4, 2, 2))
  for (i in seq_len(nrow(d))) {
    curr_z1 <- match(d$zone1[i], c("a", "b"))
    curr_z2 <- match(d$zone2[i], c("a", "b"))
    x2[d$id1[i], d$id2[i], curr_z1, curr_z2] <- d$boundary[i]
  }
  # tests
  expect_inherits(x, "array")
  expect_equal(x, x2)
})

test_that("single zone (x = ConservationProblem, symmetric = TRUE)", {
  # create data
  d <- expand.grid(id1 = seq_len(4), id2 = c(1, 4))
  d$boundary <- 1
  d$boundary[d$id1 == d$id2] <- 0.5
  p <- problem(
    x = matrix(runif(5), ncol = 1),
    features = data.frame(id = seq_len(2), name = letters[seq_len(2)]),
    rij_matrix = matrix(1, ncol = 5, nrow = 2)
  )
  # create object
  x <- marxan_connectivity_data_to_matrix(p, d, symmetric = TRUE)
  # create correct result
  x2 <- matrix(0, ncol = 5, nrow = 5)
  x2[1, seq_len(4)] <- 1
  x2[seq_len(4), 1] <- 1
  x2[4, seq_len(4)] <- 1
  x2[seq_len(4), 4] <- 1
  x2[1, 1] <- 0.5
  x2[4, 4] <- 0.5
  # tests
  expect_inherits(x, "dsCMatrix")
  expect_equal(x, as_Matrix(x2, "dsCMatrix"))
})

test_that("multiple zones (x = ConservationProblem, symmetric = TRUE)", {
  # create data
  d <- data.frame(
    id1 = c(1, 2, 3, 4, 2),
    id2 = c(1, 1, 1, 2, 2),
    zone1 = c("a", "a", "b", "a", "b"),
    zone2 = c("a", "b", "b", "a", "b"),
    boundary = seq_len(5)
  )
  p <- problem(
    x = matrix(1, nrow = 5, ncol = 2),
    features = data.frame(id = seq_len(2), name = letters[seq_len(2)]),
    rij_matrix = list(
      "a" = matrix(1, ncol = 5, nrow = 2),
      "b" = matrix(1, ncol = 5, nrow = 2)
    )
  )
  # create object
  x <- marxan_connectivity_data_to_matrix(p, d, symmetric = TRUE)
  # create correct result
  x2 <- array(0, dim = c(5, 5, 2, 2))
  for (i in seq_len(nrow(d))) {
    curr_z1 <- match(d$zone1[i], c("a", "b"))
    curr_z2 <- match(d$zone2[i], c("a", "b"))
    x2[d$id1[i], d$id2[i], curr_z1, curr_z2] <- d$boundary[i]
  }
  # tests
  expect_inherits(x, "array")
  expect_equal(x, x2)
})

test_that("single zone (x = NULL, symmetric = FALSE)", {
  # create data
  d <- data.frame(id1 = c(1, 2, 2), id2 = c(1, 1, 3), boundary = c(4, 5, 6))
  # create object
  x <- marxan_connectivity_data_to_matrix(NULL, d, symmetric = FALSE)
  # create correct result
  x2 <- matrix(0, ncol = 3, nrow = 3)
  x2[1, 1] <- 4
  x2[2, 1] <- 5
  x2[2, 3] <- 6
  # tests
  expect_inherits(x, "dgCMatrix")
  expect_equal(x, as_Matrix(x2, "dgCMatrix"))
})

test_that("multiple zones (x = NULL, symmetric = FALSE)", {
  # create data
  d <- data.frame(
    id1 = c(1, 2, 3, 4, 1, 2),
    id2 = c(1, 1, 1, 2, 2, 2),
    zone1 = c("a", "a", "b", "a", "a", "b"),
    zone2 = c("a", "b", "b", "a", "b", "b"),
    boundary = seq_len(6)
  )
  # create object
  x <- marxan_connectivity_data_to_matrix(NULL, d, symmetric = FALSE)
  # create correct result
  x2 <- array(0, dim = c(4, 4, 2, 2))
  for (i in seq_len(nrow(d))) {
    curr_z1 <- match(d$zone1[i], c("a", "b"))
    curr_z2 <- match(d$zone2[i], c("a", "b"))
    x2[d$id1[i], d$id2[i], curr_z1, curr_z2] <- d$boundary[i]
  }
  # tests
  expect_inherits(x, "array")
  expect_equal(x, x2)
})

test_that("single zone (x = ConservationProblem, symmetric = FALSE)", {
  # create data
  d <- data.frame(id1 = c(1, 2, 2), id2 = c(1, 1, 3), boundary = c(4, 5, 6))
  p <- problem(
    x = matrix(runif(5), ncol = 1),
    features = data.frame(id = seq_len(2), name = letters[seq_len(2)]),
    rij_matrix = matrix(1, ncol = 5, nrow = 2)
  )
  # create object
  x <- marxan_connectivity_data_to_matrix(p, d, symmetric = FALSE)
  # create correct result
  x2 <- matrix(0, ncol = 5, nrow = 5)
  x2[1, 1] <- 4
  x2[2, 1] <- 5
  x2[2, 3] <- 6
  # tests
  expect_inherits(x, "dgCMatrix")
  expect_equal(x, as_Matrix(x2, "dgCMatrix"))
})

test_that("multiple zones (x = ConservationProblem, symmetric = FALSE)", {
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
      "a" = matrix(1, ncol = 5, nrow = 2),
      "b" = matrix(1, ncol = 5, nrow = 2)
    )
  )
  # create object
  x <- marxan_connectivity_data_to_matrix(p, d, symmetric = FALSE)
  # create correct result
  x2 <- array(0, dim = c(5, 5, 2, 2))
  for (i in seq_len(nrow(d))) {
    curr_z1 <- match(d$zone1[i], c("a", "b"))
    curr_z2 <- match(d$zone2[i], c("a", "b"))
    x2[d$id1[i], d$id2[i], curr_z1, curr_z2] <- d$boundary[i]
  }
  # tests
  expect_inherits(x, "array")
  expect_equal(x, x2)
})
