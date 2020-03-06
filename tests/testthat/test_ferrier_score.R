context("ferrier_score")

test_that("numeric", {
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
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- c(0, 1, NA, 1)
  # calculate scores
  r1 <- ferrier_score(p, s)
  # create correct result
  r2 <- c(0, 0.272683958214909, NA, 1.30274715364078)
  # run tests
  expect_equal(r1, r2)
})

test_that("matrix (single zone)", {
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
  # calculate scores
  r1 <- ferrier_score(p, s)
  # create correct result
  r2 <- matrix(c(0, 0.272683958214909, NA, 1.30274715364078), ncol = 1)
  colnames(r2) <- "fs"
  # run tests
  expect_equal(r1, r2)
})

test_that("data.frame (single zone)", {
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
  # calculate scores
  r1 <- ferrier_score(p, s)
  # create correct result
  r2 <- tibble::tibble(fs = c(0, 0.272683958214909, NA, 1.30274715364078))
  # run tests
  expect_equal(r1, r2)
})

test_that("Spatial (single zone)", {
  # create data
  data(sim_pu_polygons)
  pu <- sim_pu_polygons[1:4, ]
  pu@data <- data.frame(id = seq_len(4),
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
  pu$solution <- c(0, 1, NA, 1)
  # calculate scores
  r1 <- ferrier_score(p, pu[, "solution"])
  # create correct result
  pu$fs <- c(0, 0.272683958214909, NA, 1.30274715364078)
  # run tests
  expect_equivalent(r1, pu[, "fs"])
})

test_that("Raster (single zone)", {
  # create data
  pu <- raster::raster(matrix(c(10, 2, NA, 3), nrow = 1))
  features <- raster::stack(raster::raster(matrix(c(0, 0, 0, 1), nrow = 1)),
                            raster::raster(matrix(c(10, 5, 10, 6), nrow = 1)))
  # create problem
  p <-
    problem(pu, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- raster::raster(matrix(c(0, 1, NA, 1), nrow = 1))
  # calculate scores
  r1 <- ferrier_score(p, s)
  # create correct result
  r2 <- raster::raster(matrix(c(0, 0.272683958214909, NA, 1.30274715364078),
                              nrow = 1))
  # run tests
  expect_equal(r1, r2)
})

test_that("data.frame (complex dataset)", {
  # test data were kindly provided by Bob Smith (@AnotherBobSmith)
  # - rij.rds file contains a feature by planning unit matrix
  # - targets.rds file contains a data.frame with feature information
  # - portfolio_size is based on a Marxan analyses
  # - scores.rds file contains the irreplaceability scores for each feature in
  #   each planning unit

  # define raw data
  rij <- readRDS(system.file("testdata", "rij.rds", package = "prioritizr"))
  targ <- readRDS(system.file("testdata", "targets.rds",
                              package = "prioritizr"))
  portfolio_size <- 803
  r2 <- readRDS(system.file("testdata", "scores.rds", package = "prioritizr"))
  # prepare for prioritizr
  s <- sample.int(ncol(rij), portfolio_size)
  s <- tibble::tibble(solution = replace(rep(0, ncol(rij)), s, 1))
  pu <- as.data.frame(as.matrix(t(rij)))
  names(pu) <- targ$Name
  pu$cost <- 1
  # create problem
  p <-
    problem(pu, targ$Name, "cost") %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ$Target) %>%
    add_binary_decisions()
  # calculate scores
  r1 <- ferrier_score(p, s)
  # correct result
  r2 <- tibble::tibble(fs = Matrix::colSums(r2))
  r2$fs[!as.logical(s[[1]])] <- 0
  # run tests
  expect_lte(max(abs(r1$fs - r2$fs)), 1e-5)
})
