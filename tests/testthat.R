# determine if tests should be skipped
is_skip <- !identical(Sys.getenv("NOT_CRAN"), "true")

# run tests if needed
if (!is_skip) {
  ## load packages
  library(testthat)
  library(prioritizr)

  ## try to load solver packages
  require(gurobi)
  require(lpsymphony)
  require(Rsymphony)

  ## disable parallel testing
  Sys.unsetenv("R_TESTS")

  ## determine reporter
  if (identical(Sys.getenv("CI"), "true")) {
    reporter <- "progress"
  } else {
    reporter <- testthat::check_reporter()
  }

  ## check tests
  test_check("prioritizr", reporter = reporter)
}
