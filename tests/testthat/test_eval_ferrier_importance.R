context("eval_ferrier_importance")

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
  r1 <- eval_ferrier_importance(p, s)
  # create correct total scores
  r2 <- as.matrix(
    ferrier_scores_r(
      rij = t(as.matrix(pu[, c("spp1", "spp2")])),
      targets = c(1, 10),
      solution = c(s)
    )
  )
  # run tests
  expect_is(r1, "matrix")
  expect_equal(ncol(r1), 3)
  expect_equal(nrow(r1), 4)
  expect_equal(colnames(r1), c("spp1", "spp2", "total"))
  expect_equal(rowSums(r1[, -3]), r1[, 3])
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
  r1 <- eval_ferrier_importance(p, s)
  # create correct total scores
  r2 <- as.matrix(
    ferrier_scores_r(
      rij = t(as.matrix(pu[, c("spp1", "spp2")])),
      targets = c(1, 10),
      solution = c(s)
    )
  )
  # run tests
  expect_is(r1, "matrix")
  expect_equal(ncol(r1), 3)
  expect_equal(nrow(r1), 4)
  expect_equal(colnames(r1), c("spp1", "spp2", "total"))
  expect_equal(rowSums(r1[, -3]), r1[, 3])
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
  r1 <- eval_ferrier_importance(p, s)
  # create correct total scores
  r2 <- as.data.frame(
    as.matrix(
      ferrier_scores_r(
        rij = t(as.matrix(pu[, c("spp1", "spp2")])),
        targets = c(1, 10),
        solution = c(s[[1]])
      )
    )
  )
  # run tests
  expect_is(r1, "data.frame")
  expect_equal(ncol(r1), 3)
  expect_equal(nrow(r1), 4)
  expect_equal(colnames(r1), c("spp1", "spp2", "total"))
  expect_equal(rowSums(as.matrix(r1[, -3])), r1[[3]])
  expect_equal(r1, tibble::as_tibble(r2))
})

test_that("Spatial (single zone)", {
  # create data
  sim_pu_polygons <- get_sim_pu_polygons()
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
  r1 <- eval_ferrier_importance(p, pu[, "solution"])
  # create correct total scores
  r2 <- as.data.frame(
    as.matrix(
      ferrier_scores_r(
        rij = t(as.matrix(pu@data[, c("spp1", "spp2")])),
        targets = c(1, 10),
        solution = pu$solution
      )
    )
  )
  # run tests
  expect_is(r1, "SpatialPolygonsDataFrame")
  expect_equal(ncol(r1), 3)
  expect_equal(nrow(r1), 4)
  expect_equal(names(r1), c("spp1", "spp2", "total"))
  expect_equal(unname(rowSums(as.matrix(r1@data[, -3]))), r1@data[[3]])
  expect_equal(tibble::as_tibble(r1@data), tibble::as_tibble(r2))
})

test_that("sf (single zone)", {
  # create data
  sim_pu_polygons <- get_sim_pu_polygons()
  pu <- sim_pu_polygons[1:4, ]
  pu$id <- seq_len(4)
  pu$cost <- c(10, 2, NA, 3)
  pu$spp1 <- c(0, 0, 0, 1)
  pu$spp2 <- c(10, 5, 10, 6)
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
  r1 <- eval_ferrier_importance(p, pu[, "solution"])
  # create correct total scores
  r2 <- as.data.frame(
    as.matrix(
      ferrier_scores_r(
        rij = t(as.matrix(sf::st_drop_geometry(pu)[, c("spp1", "spp2")])),
        targets = c(1, 10),
        solution = pu$solution
      )
    )
  )
  # run tests
  expect_is(r1, "sf")
  expect_equal(ncol(sf::st_drop_geometry(r1)), 3)
  expect_equal(nrow(r1), 4)
  expect_equal(names(sf::st_drop_geometry(r1)), c("spp1", "spp2", "total"))
  expect_equivalent(r1$spp1 + r1$spp2, r1$total)
  expect_equal(
    tibble::as_tibble(sf::st_drop_geometry(r1)[, c("spp1", "spp2", "total")]),
    tibble::as_tibble(r2)
  )
})

test_that("Raster (single zone)", {
  # create data
  pu <- raster::raster(matrix(c(10, 2, NA, 3), nrow = 2))
  features <- raster::stack(raster::raster(matrix(c(0, 0, 0, 1), nrow = 2)),
                            raster::raster(matrix(c(10, 5, 10, 6), nrow = 2)))
  names(features) <- c("spp1", "spp2")
  # create problem
  p <-
    problem(pu, features) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(1, 10)) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # create a solution
  s <- raster::raster(matrix(c(0, 1, NA, 1), nrow = 2))
  # calculate scores
  r1 <- eval_ferrier_importance(p, s)
  # create correct total scores
  r2 <- as.data.frame(
    as.matrix(
      ferrier_scores_r(
        rij = t(as.matrix(raster::as.data.frame(features))),
        targets = c(1, 10),
        solution = raster::values(s)
      )
    )
  )
  # run tests
  expect_is(r1, "Raster")
  expect_equal(raster::nlayers(r1), 3)
  expect_equal(raster::ncell(r1), 4)
  expect_equal(raster::ncol(r1), raster::ncol(pu))
  expect_equal(raster::nrow(r1), raster::nrow(pu))
  expect_equal(names(r1), c("spp1", "spp2", "total"))
  expect_equal(raster::getValues(r1[["spp1"]]), r2$spp1)
  expect_equal(raster::getValues(r1[["spp2"]]), r2$spp2)
  expect_equal(raster::getValues(r1[["total"]]), r2$total)
})

test_that("data.frame (complex dataset)", {
  # test data were kindly provided by Bob Smith (@AnotherBobSmith)
  # - rij.rds file contains a feature by planning unit matrix
  # - targets.rds file contains a data.frame with feature information
  # - portfolio_size is based on a Marxan analyses
  # define raw data
  rij <- readRDS(system.file("testdata", "rij.rds", package = "prioritizr"))
  targ <- readRDS(system.file("testdata", "targets.rds",
                              package = "prioritizr"))
  portfolio_size <- 803
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
  r1 <- eval_ferrier_importance(p, s)
  # correct result
  r2 <- as.data.frame(
    as.matrix(
      ferrier_scores_r(
        rij = t(as.matrix(pu[, targ$Name, drop = FALSE])),
        targets = targ$Target,
        solution = s[[1]]
      )
    )
  )
  # run tests
  expect_lte(max(abs(as.matrix(r1) - as.matrix(r2))), 1e-5)
})
