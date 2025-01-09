test_that("terra_can_process_in_memory", {
  # import data
  sim_features <- get_sim_features()
  # tests
  expect_true(
    is.logical(terra_can_process_in_memory(sim_features))
  )
  # additional tests
  skip_on_cran()
  expect_true(
    terra_can_process_in_memory(sim_features)
  )
  expect_true(
    terra_can_process_in_memory(sim_features, n = 1)
  )
  expect_true(
    terra_can_process_in_memory(sim_features, n = 5)
  )
})

test_that("terra_n_process_in_memory", {
  # import data
  sim_features <- get_sim_features()
  # tests
  expect_true(
    is.integer(terra_n_process_in_memory(sim_features))
  )
  expect_gte(
    terra_n_process_in_memory(sim_features),
    1L
  )
})
