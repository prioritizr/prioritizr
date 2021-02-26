# load packages
library(testthat)
library(prioritizr)

# load solver packages
require(gurobi)
require(lpsymphony)
require(Rsymphony)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# run tests
## except on CRAN's Windows systems to reduce test timings
if ((!identical(.Platform$OS.type, "windows")) &&
  identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("prioritizr", reporter = "location")
}
