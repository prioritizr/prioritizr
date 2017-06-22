## ---- eval = FALSE-------------------------------------------------------
#  if (!require(devtools))
#    install.packages("devtools")
#  devtools::install_github("prioritizr/prioritizr")

## ---- include = FALSE----------------------------------------------------
devtools::load_all()
h = 3.5
w = 3.5

## ---- eval = FALSE-------------------------------------------------------
#  # load package
#  library(prioritizr)
#  # and for pretty plotting
#  library(rasterVis)
#  library(viridis)
#  library(ggplot2)

## ---- fig.height = h * 0.7, fig.width = w * 3.25, echo=FALSE-------------
# load feature data
data(sim_features)
# plot the distribution of suitable habitat for each feature
plot(sim_features, main = paste("Feature", seq_len(nlayers(sim_features))),
     nr = 1)

## ---- fig.align='center', fig.height = h * 1, fig.width = w * 3, echo=FALSE----
# load feature data
data(sim_pu_polygons)
data(sim_pu_lines)
data(sim_pu_points)
# plot the distribution of suitable habitat for each feature
par(mfrow=c(1,3))
plot(sim_pu_polygons, main="Polygons")
plot(sim_pu_lines, main="Lines")
plot(sim_pu_points, pch = 16, main="Points")

## ---- eval=TRUE----------------------------------------------------------
# explore simulated data
head(sim_pu_polygons)

## ---- eval = FALSE-------------------------------------------------------
#  p1 <- problem(sim_pu_polygons, sim_features, cost_column = sim_pu_polygons$cost) #polygons
#  p2 <- problem(sim_pu_lines, sim_features, cost_column = sim_pu_lines$cost) #lines
#  p3 <- problem(sim_pu_points, sim_features, cost_column = sim_pu_points$cost) #points

## ---- fig.height = h * 1, fig.width = w * 1, fig.align='center', echo=FALSE----
# load feature data
data(sim_pu_raster)
# plot the distribution of suitable habitat for each feature
plot(sim_pu_raster, main = "Raster")

## ---- eval = FALSE-------------------------------------------------------
#  #raster planning units
#  p4 <- problem(sim_pu_raster, sim_features)

## ---- eval = FALSE-------------------------------------------------------
#  p4 <- problem(sim_pu_raster, sim_features) %>%
#    add_min_set_objective()
#  
#  #or to store objective in new problem object:
#  p4 <- problem(sim_pu_raster, sim_features)
#  p5 <- p4 %>% add_min_set_objective()

## ---- eval = FALSE-------------------------------------------------------
#  p4 <- problem(sim_pu_raster, sim_features) %>%
#    add_max_cover_objective(budget = 5000)

## ---- eval = FALSE-------------------------------------------------------
#  p4 <- problem(sim_pu_raster, sim_features) %>%
#    add_max_features_objective(budget = 5000)

## ---- eval = FALSE-------------------------------------------------------
#  data(sim_phylogeny) #load simulated phylogeny data
#  p4 <- problem(sim_pu_raster, sim_features) %>%
#    add_max_phylo_objective(budget = 5000, tree = sim_phylogeny)

## ---- eval = FALSE-------------------------------------------------------
#  # add relative targets to existing problem
#  p4 <- problem(sim_pu_raster, sim_features) %>%
#    add_min_set_objective() %>%
#    add_relative_targets(0.1)
#  
#  # Or, create new problem objects to try different target types
#  # create problem with added relative targets
#  p.r <- p4 %>% add_relative_targets(0.1)
#  
#  # create problem with added absolute targets
#  p.a <- p4 %>% add_absolute_targets(3)
#  
#  # create problem with added log-linear target
#  p.l <- p4 %>% add_loglinear_targets(10, 0.9, 100, 0.2)

## ---- eval = FALSE-------------------------------------------------------
#  # vary targets by feature
#  p.r2 <- p4 %>% add_relative_targets(c(0.5, 0.1, 0.1, 0.1, 0.1))

## ---- eval = FALSE-------------------------------------------------------
#  # add relative targets to existing problem
#  p4 <- problem(sim_pu_raster, sim_features) %>%
#    add_min_set_objective() %>%
#    add_relative_targets(0.1) %>%
#    add_connected_constraints()

## ---- fig.height = h * 1, fig.width = w * 2, fig.align='center', echo=FALSE----
# load feature data
data(sim_locked_in_raster)
data(sim_locked_out_raster)
# plot the distribution of suitable habitat for each feature
par(mfrow=c(1,2))
plot(sim_locked_in_raster, main = "Locked In Raster", legend = FALSE)
plot(sim_locked_out_raster, main = "Locked Out Raster", legend = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  # create problem with added locked in constraints using integers
#  p.lock1 <- p1 %>% add_locked_in_constraints(which(sim_pu_polygons$locked_in))
#  
#  # create problem with added locked in constraints using a field name
#  p.lock2 <- p1 %>% add_locked_in_constraints("locked_in")
#  
#  # create problem with added locked in constraints using raster data
#  p.lock3 <- p4 %>% add_locked_in_constraints(sim_locked_in_raster)

## ---- eval = FALSE-------------------------------------------------------
#  # create problem with boundary penalties added to it
#  pBLM <- p4 %>% add_boundary_penalties(penalty = 500, edge_factor = 0.5)

## ---- fig.height = h * 1, fig.width = w * 3, fig.align='center', echo=FALSE, eval=FALSE----
#  p.1 <- problem(sim_pu_raster, sim_features) %>%
#    add_min_set_objective() %>%
#    add_relative_targets(0.2) %>%
#    add_gurobi_solver()
#  p.2 <- problem(sim_pu_raster, sim_features) %>%
#    add_min_set_objective() %>%
#    add_relative_targets(0.2) %>%
#    add_connected_constraints() %>%
#    add_gurobi_solver()
#  p.3 <- problem(sim_pu_raster, sim_features) %>%
#    add_min_set_objective() %>%
#    add_relative_targets(0.2) %>%
#    add_boundary_penalties(penalty = 500, edge_factor = 1) %>%
#    add_gurobi_solver()
#  
#  # solve problems
#  s <- stack(solve(p.1), solve(p.2), solve(p.3))
#  
#  # plot solutions
#  plot(s, main = c("no connectivity requirement", "connected constraint", "boundary penalty"))

## ---- eval = TRUE, results='hide', message=FALSE, fig.align='center', fig.height = h, fig.width = w----
#formulate the problem
p5 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(penalty = 500, edge_factor = 0.5) %>%
  add_binary_decisions() %>%
  add_gurobi_solver()

#solve the problem
s5 <- solve(p5)
plot(s5, col = c('grey90', 'darkgreen'),
          main = "Solution", xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))

## ----eval=TRUE, results='hide'-------------------------------------------
#complete input file
input <- system.file("extdata/input.dat", package="prioritizr")
mp1 <- marxan_problem(input)
ms1 <- solve(mp1)

## ----eval=TRUE-----------------------------------------------------------
head(ms1)

## ----eval=FALSE----------------------------------------------------------
#  pu <- system.file("extdata/input/pu.dat", package="prioritizr") #missing correct ID field
#  features <- system.file("extdata/input/spec.dat", package="prioritizr")
#  bound <- system.file("extdata/input/bound.dat", package="prioritizr")
#  rij <- system.file("extdata/input/puvspr.dat", package="prioritizr")
#  
#  mp2 <- marxan_problem(x = pu, spec = features, puvspr = rij, bound = bound, blm = 0)
#  #throws error, pu must be reformatted

## ----eval=TRUE, results='hide', fig.align='center', fig.height = h * 0.7----
mp3 <- marxan_problem(x = sim_pu_polygons, features = sim_features, 
                      targets = 0.2, targets_type = "relative", 
                      locked_in = sim_locked_in_raster)
ms3 <- solve(mp3)
spplot(ms3, zcol = 'solution', main='Solution')

## ----eval = TRUE---------------------------------------------------------
#raster solution
  #number of cells of each feature selected or not selected in solutions
zonal(s5, sim_features[[1]], fun=sum) #feature 1
zonal(s5, sim_features[[2]], fun=sum) #feature 2
zonal(s5, sim_features[[3]], fun=sum) #feature 3
zonal(s5, sim_features[[4]], fun=sum) #feature 4
zonal(s5, sim_features[[5]], fun=sum) #feature 5

#marxan solution
  #count number of selected planning units 
sol <- ms1[ms1$Solution == 1]
sol

