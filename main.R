devtools::load_all()
library(Matrix)
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define weights
  weights <- c(1, 10)
  # calculate targets data
  targ1 <- floor(
    terra::global(sim_features[[1:2]], "sum", na.rm = TRUE)[[1]] * 0.25
  )
  targ2 <- floor(
    terra::global(sim_features[[3:5]], "sum", na.rm = TRUE)[[1]] * 0.1
  )
  # create problems
  p1 <-
    problem(sim_pu_raster * 1, sim_features[[1:2]]) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ1) %>%
    add_binary_decisions()
  p2 <-
    problem(sim_pu_raster * 3, sim_features[[3:5]]) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targ2) %>%
    add_binary_decisions()
  # compile problem
  x <- compile_ws_approach(list(p1, p2), weights)
  # calculations for tests
  # TODO
  # tests
  expect_equal(x$rhs(), c(targ1, targ2))
  expect_equal(x$sense(), rep(">=", 5))
  expect_equal(
    x$obj(),
    c(sim_pu_raster[[1]][!is.na(sim_pu_raster)] * 1 * weights[1]) +
    c(sim_pu_raster[[1]][!is.na(sim_pu_raster)] * 3 * weights[2])
  )

  
  