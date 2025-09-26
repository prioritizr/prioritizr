test_that("compile dual min set problems", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # define weights
  weights <- c(1, 0.2)
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
  # tests
  ....
})

test_that("solve dual min set problems", {
  # load data


})


test_that("compile dual min shortfalls problems", {
  # load data

})

test_that("solve dual min shortfalls problems", {
  # load data

})

test_that("compile multiple mixed problems", {
  # load data

})

test_that("solve multiple mixed problems", {
  # load data

})
