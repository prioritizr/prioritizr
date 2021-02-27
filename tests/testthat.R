# load packages
library(testthat)
library(prioritizr)

# load solver packages
require(gurobi)
require(lpsymphony)
require(Rsymphony)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# set reporter
## we will use the default reporter for local tests
reporter <- "check"

## if we are running the tests on CRAN we will use location reporter
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  reporter <- "location"
}

## if we are running the tests on CI we will use location reporter
if (identical(Sys.getenv("CI"), "true")) {
  reporter <- "location"
}

# run tests
## except on CRAN's Windows systems to reduce test timings
if ((!identical(.Platform$OS.type, "windows")) &&
  identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("prioritizr", reporter = reporter)
}
