context("align_text")

test_that("works", {
  expect_equal(
    align_text("animals: horse\npig\nbear", 9),
    "animals: horse\n         pig\n         bear"
  )
})
