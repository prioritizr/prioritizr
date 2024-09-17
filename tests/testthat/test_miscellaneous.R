test_that("citation works", {
  cit <- citation(package = "prioritizr")
  expect_s3_class(cit, "citation")
})
