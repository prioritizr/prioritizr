test_that("jung_targets", {
  expect_equal(
    get_target_method("jung"),
    jung_targets()
  )
})

test_that("ward_targets", {
  expect_equal(
    get_target_method("ward"),
    ward_targets()
  )
})

test_that("polak_targets", {
  expect_equal(
    get_target_method("polak"),
    polak_targets()
  )
})

test_that("watson_targets", {
  expect_equal(
    get_target_method("watson"),
    watson_targets()
  )
})

test_that("rodrigues_targets", {
  expect_equal(
    get_target_method("rodrigues"),
    rodrigues_targets()
  )
})

test_that("rl_species_targets", {
  # create objects
  x1 <- get_target_method("rl_species_en_a1_b2")
  x2 <- rl_species_targets(
    status = "en", criterion_a = "a1", criterion_b = "b2", prop_uplift = 0
  )
  # set frame to NULL so that tests don't compare it
  x1$frame <- NULL
  x2$frame <- NULL
  # run tests
  expect_equal(x1, x2)
})

test_that("rl_ecosystem_targets", {
  # create objects
  x1 <- get_target_method("rl_ecosystem_en_a1_b2")
  x2 <- rl_ecosystem_targets(
    status = "en", criterion_a = "a1", criterion_b = "b2", prop_uplift = 0
  )
  # set frame to NULL so that tests don't compare it
  x1$frame <- NULL
  x2$frame <- NULL
  # run tests
  expect_equal(x1, x2)
})
