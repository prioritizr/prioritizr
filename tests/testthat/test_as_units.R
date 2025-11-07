test_that("as_km2", {
  expect_equal(as_km2(1, "km2"), 1)
  expect_equal(as_km2(1, "km^2"), 1)
  expect_equal(as_km2(1, "ha"), 0.01)
  expect_equal(as_km2(c(1, 2, 3), "km2"), c(1, 2, 3))
  expect_equal(as_km2(c(1, 2, 3), "km^2"), c(1, 2, 3))
  expect_equal(as_km2(c(1, 2, 3), "ha"), c(0.01, 0.02, 0.03))
  expect_equal(
    as_km2(c(1, 2, 3), c("km2", "km2", "ha")),
    c(1, 2, 0.03)
  )
})

test_that("as_per_km2 (varying units", {
  expect_equal(as_per_km2(1, "km2"), 1)
  expect_equal(as_per_km2(1, "km^2"), 1)
  expect_equal(as_per_km2(1, "ha"), 100)
  expect_equal(as_per_km2(c(1, 2, 3), "km2"), c(1, 2, 3))
  expect_equal(as_per_km2(c(1, 2, 3), "km^2"), c(1, 2, 3))
  expect_equal(as_per_km2(c(1, 2, 3), "ha"), c(100, 200, 300))
  expect_equal(
    as_per_km2(c(1, 2, 3), c("km2", "km2", "ha")),
    c(1, 2, 300)
  )
})
