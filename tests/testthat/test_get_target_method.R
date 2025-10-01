test_that("jung", {
  expect_equal(
    get_target_method("jung"),
    spec_jung_targets()
  )
})

test_that("ward", {
  expect_equal(
    get_target_method("ward"),
    spec_ward_targets()
  )
})

test_that("polak", {
  expect_equal(
    get_target_method("polak"),
    spec_polak_targets()
  )
})

test_that("watson", {
  expect_equal(
    get_target_method("watson"),
    spec_watson_targets()
  )
})

test_that("rodrigues", {
  expect_equal(
    get_target_method("rodrigues"),
    spec_rodrigues_targets()
  )
})

test_that("rl_species", {
  # create objects
  x1 <- get_target_method("rl_species_en_a1_b2")
  x2 <- spec_rl_species_targets(
    status = "en", criterion_a = "a1", criterion_b = "b2", prop_uplift = 0
  )
  # set frame to NULL so that tests don't compare it
  x1$frame <- NULL
  x2$frame <- NULL
  # run tests
  expect_equal(x1, x2)
})

test_that("rl_ecosystem", {
  # create objects
  x1 <- get_target_method("rl_ecosystem_en_a1_b2")
  x2 <- spec_rl_ecosystem_targets(
    status = "en", criterion_a = "a1", criterion_b = "b2", prop_uplift = 0
  )
  # set frame to NULL so that tests don't compare it
  x1$frame <- NULL
  x2$frame <- NULL
  # run tests
  expect_equal(x1, x2)
})
