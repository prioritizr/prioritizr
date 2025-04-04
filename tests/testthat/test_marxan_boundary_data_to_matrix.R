test_that("single zone (x = NULL)", {
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
  x2[1, 1] <- 3.5
  x2[2, 2] <- 2
  x2[3, 3] <- 2
  x2[4, 4] <- 3.5
  # tests
  expect_inherits(x, "dsCMatrix")
  expect_equal(x, as_Matrix(x2, "dsCMatrix"))
})

test_that("single zone (x = ConservationProblem)", {
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
  x <- marxan_boundary_data_to_matrix(p, d)
  # create correct result
  x2 <- matrix(0, ncol = 5, nrow = 5)
  x2[1, seq_len(4)] <- 1
  x2[seq_len(4), 1] <- 1
  x2[4, seq_len(4)] <- 1
  x2[seq_len(4), 4] <- 1
  x2[1, 1] <- 3.5
  x2[2, 2] <- 2
  x2[3, 3] <- 2
  x2[4, 4] <- 3.5
  # tests
  expect_inherits(x, "dsCMatrix")
  expect_equal(x, as_Matrix(x2, "dsCMatrix"))
})
