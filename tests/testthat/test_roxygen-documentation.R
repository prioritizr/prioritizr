test_that("locked_documentation()", {
  x <- locked_documentation("XXXXX")
  expect_inherits(x, "character")
  expect_length(x, 1)
  expect_true(grepl("XXXXX", x, fixed = TRUE))
})

test_that("solution_format_documentation()", {
  x <- solution_format_documentation("XXXXX")
  expect_inherits(x, "character")
  expect_length(x, 1)
  expect_true(grepl("XXXXX", x, fixed = TRUE))
})
