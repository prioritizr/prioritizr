test_that("is_thread_count", {
  expect_true(is_thread_count(1))
  expect_false(is_thread_count("a"))
  expect_false(is_thread_count(NA))
  expect_error(
    assert(is_thread_count("a")),
    "cores"
  )
})

test_that("is_budget_length", {
  # load data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  p1 <- problem(sim_pu_raster, sim_features)
  p2 <- problem(sim_zones_pu_raster, sim_zones_features)
  # tests
  expect_true(is_budget_length(p1, 1))
  expect_true(is_budget_length(p2, rep(1, number_of_zones(sim_zones_features))))
  expect_false(is_budget_length(p1, c(1, 2)))
  expect_false(is_budget_length(p2, rep(1, 1000)))
  expect_error(
    assert(is_budget_length(p1, c(1, 2))),
    "budget"
  )
  expect_error(
    assert(is_budget_length(p2, rep(1, 1000))),
    "budget"
  )
})

test_that("is_installed", {
  expect_true(is_installed("base"))
  expect_false(is_installed("a a a")) # R pkgs should not have space in name
})

test_that("is_area_units", {
  expect_true(is_area_units("km^2"))
  expect_true(is_area_units("m^2"))
  expect_false(is_area_units("g"))
  expect_false(is_area_units("greg"))
  expect_false(is_area_units(c("m^2", "km^2")))
})

test_that("all_area_units", {
  expect_true(all_area_units("km^2"))
  expect_true(all_area_units("m^2"))
  expect_true(all_area_units(c("m^2", "km^2")))
  expect_false(all_area_units("g"))
  expect_false(all_area_units("g", "km^2"))
  expect_false(all_area_units("greg"))
  expect_false(all_area_units(1))
  expect_false(all_area_units("greg", "km^2"))
})
