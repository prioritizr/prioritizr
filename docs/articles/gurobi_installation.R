## ---- include = FALSE----------------------------------------------------
h = 3.5
w = 3.5
is_check <- ("CheckExEnv" %in% search()) || any(c("_R_CHECK_TIMINGS_",
             "_R_CHECK_LICENSE_") %in% names(Sys.getenv()))
knitr::opts_chunk$set(fig.align = "center", eval = !is_check,
                      root.dir = normalizePath("../.."))

## ---- include = FALSE----------------------------------------------------
devtools::load_all()

## ---- echo = FALSE-------------------------------------------------------
print(getwd())

## ---- echo = FALSE, results = "asis"-------------------------------------
cat(paste0("![](", normalizePath("man/figures/free-academic-license-page.png"),
           ")"))

## ---- echo = FALSE, results = "asis"-------------------------------------
cat(paste0("![](", normalizePath("man/figures/actual-license.png"), ")"))

## ---- echo = FALSE, results = "asis"-------------------------------------
cat(paste0("![](", normalizePath("man/figures/cmd-windows-success.png"), ")"))

## ---- echo = FALSE, results = "asis"-------------------------------------
cat(paste0("![](", normalizePath("man/figures/model-test.png"), ")"))

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("c:/gurobi751/win64/R/gurobi_7.5-1.zip", repos = NULL)

## ---- eval = FALSE-------------------------------------------------------
#  install.packages(file.path(Sys.getenv("GUROBI_HOME"), "R/gurobi_7.5-1.zip"),
#                   repos = NULL)

## ---- eval  = FALSE------------------------------------------------------
#  install.packages("slam", repos = "https://cloud.r-project.org")

## ------------------------------------------------------------------------
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

## ---- fig.height = h, fig.width = w--------------------------------------
library(prioritizr)

# formulate the problem
p <- problem(sim_pu_raster, sim_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_gurobi_solver()

# solve the problem
s <- solve(p)

# plot solution
plot(s, col = c("grey90", "darkgreen"), main = "Solution",
     xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))

