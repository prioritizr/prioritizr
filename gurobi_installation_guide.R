## ----include = FALSE----------------------------------------------------------
h = 3.5
w = 3.5
is_check <-
  ("CheckExEnv" %in% search()) ||
  any(c("_R_CHECK_TIMINGS_", "_R_CHECK_LICENSE_") %in% names(Sys.getenv())) ||
  !identical(Sys.getenv("MY_UNIVERSE"), "")
knitr::opts_chunk$set(
  fig.align = "center",
  eval = !is_check, purl = !is_check
)


## ----include = FALSE----------------------------------------------------------
# set up print method
print <- function(x, ...) {
  if (inherits(x, "ConservationProblem")) {
    prioritizr::knit_print.ConservationProblem(x)
  } else if (inherits(x, "OptimizationProblem")) {
    prioritizr::knit_print.OptimizationProblem(x)
  } else {
    base::print(x)
  }
}


## gurobi_cl c:\gurobi800\win64\examples\data\coins.lp

## gurobi_cl /opt/gurobi800/linux64/examples/data/coins.lp

## ----eval = FALSE-------------------------------------------------------------
## install.packages("c:/gurobi800/win64/R/gurobi_8.0-0.zip", repos = NULL)


## ----eval = FALSE-------------------------------------------------------------
## install.packages(
##   file.path(
##     Sys.getenv("GUROBI_HOME"),
##     "R/gurobi_8.0-0_R_x86_64-pc-linux-gnu.tar.gz"
##   ),
##   repos = NULL
## )


## ----eval  = FALSE------------------------------------------------------------
## install.packages("slam", repos = "https://cloud.r-project.org")


## -----------------------------------------------------------------------------
# load gurobi package
library(gurobi)

# create optimization problem
model <- list()
model$obj        <- c(1, 1, 2)
model$modelsense <- "max"
model$rhs        <- c(4, 1)
model$sense      <- c("<", ">")
model$vtype      <- "B"
model$A          <- matrix(c(1, 2, 3, 1, 1, 0), nrow = 2, ncol = 3,
                           byrow = TRUE)

# solve the optimization problem using Gurobi
result <- gurobi(model, list())

# print the solution
print(result$objval) # objective
print(result$x)      # decision variables


## ----fig.height = h, fig.width = w--------------------------------------------
# load package
library(prioritizr)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# formulate the problem
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_gurobi_solver()

# solve the problem
s <- solve(p)

# plot solution
plot(
  s, col = c("grey90", "darkgreen"), main = "Solution",
  xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)
)

