test_that("solution_format_documentation", {
  r <- solution_format_documentation("solution")
  expect_inherits(r, "character")
  expect_length(r, 1)
  expect_true(all(!is.na(r)))
  expect_true(grepl("`solution`", r, fixed = TRUE))
})
