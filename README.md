[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis Build Status](https://img.shields.io/travis/prioritizr/prioritizr/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/prioritizr/prioritizr)
[![AppVeyor Build Status](https://img.shields.io/appveyor/ci/jeffreyhanson/prioritizr/master.svg?label=Windows)](https://ci.appveyor.com/project/jeffreyhanson/prioritizr)
[![Coverage Status](https://codecov.io/github/prioritizr/prioritizr/coverage.svg?branch=master)](https://codecov.io/github/prioritizr/prioritizr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/prioritizr)](https://CRAN.R-project.org/package=prioritizr)

# Systematic conservation prioritization in R

_Prioritizr_ is an _R_ package for solving systematic conservation prioritization problems using integer linear programming (ILP) techniques. The package offers a flexible interface for creating conservation problems using a range of different objectives and constraints that can be tailored to the specific needs of the conservation planner. Conservation problems can be solved using a variety of commercial and open-source exact algorithm solvers. In contrast to the algorithms conventionally used to solve conservation problems, such as greedy heuristics or simulated annealing, the exact algorithms used by _prioritizr_ are guaranteed to find optimal solutions. This package also has the functionality to read [Marxan](http://marxan.net/) input data and find much cheaper solutions in a much shorter period of time than _Marxan_ ([Beyer _et al. 2016_](http://marxan.net/downloads/papers/beyer_etal_2015.pdf)). Check out the [_prioritizrshiny_ _R_ package](https://github.com/prioritizr/prioritizrshiny) to interactively build and customize conservation planning problems.

## Overview

This package largely consists of six main types of functions. These functions are used to:
- create a new reserve design [problem](https://prioritizr.github.io/prioritizr/reference/problem.html) by specifying the planning units and features of conservation interest (eg. species, eco-systems).
- add an [objective](https://prioritizr.github.io/prioritizr/reference/objectives.html) to a reserve design problem. For example, the [`add_min_set_objective`](https://prioritizr.github.io/prioritizr/reference/add_min_set_objective.html) function can used to specify that the overall goal of the prioritization is to adequately represent each feature for minimal cost.
- add [constraints](https://prioritizr.github.io/prioritizr/reference/constraints.html) to a problem to obtain better solutions. For example, the [`add_locked_in_constraints`](https://prioritizr.github.io/prioritizr/reference/add_locked_in_constraints.html) function can be used to ensure that specific planning units will be prioritized. This can be useful when identifying new places to add to an exsiting reserve network.
- add [penalties](https://prioritizr.github.io/prioritizr/reference/penalties.html) to a problem to penalize ineffective solutions. For example, the [`add_boundary_penalties`](https://prioritizr.github.io/prioritizr/reference/add_boundary_penalties.html) function can be used to add penalties to the problem that result in solutions being clumpped into contiguous reserves.
- add [decisions](https://prioritizr.github.io/prioritizr/reference/decisions.html) to a problem to specify the nature of the conservation decision on the planning units. For example, the [`add_binary_decisions`](https://prioritizr.github.io/prioritizr/reference/add_binary_decisions.html) function specifies that planning units are either prioritized or not. Whereas,  the [`add_proportion_decisions`](https://prioritizr.github.io/prioritizr/reference/add_proportion_decisions.html) can be used to specify the a proportion of each
planning unit can be prioritized.
- [solve](https://prioritizr.github.io/prioritizr/reference/solve.html) a conservation problem.

The currently supported solvers are listed below. Each must be installed separately from this package. The details of the solvers are intentionally abstracted away so that minimal knowledge is required to use a given solver.

- [Gurobi:](http://gurobi.com) Install the [_gurobi_ _R_ package](http://www.gurobi.com/products/modeling-languages/r) to use this solver.
- [SYMPHONY:](https://projects.coin-or.org/SYMPHONY) Install either the [_Rsymphony_](https://CRAN.R-project.org/package=Rsymphony) or [_lpsymphony_](https://bioconductor.riken.jp/packages/3.3/bioc/html/lpsymphony.html) _R_ packages to use this solver.

## Installation

To install the developmental version of _prioritizr_, use the following _R_ code:


```r
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("prioritizr/prioritizr")
```

## Citation

```
Hanson JO, Schuster R, Strimas-Mackey M, Arcese P, Bennett J (2017).
prioritizr: Systematic Conservation Prioritization in R. R package version
1.0.0. https://github.com/prioritizr/prioritizr.
```

## Example usage

Here we will provide a simple example on using this package to solve conservation problems. We will use one of the built-in simulated data sets that is distributed with the package. First, we will load the _prioritizr_ package.


```r
# load package
library(prioritizr)
```



We will use the `sim_pu_polygons` object to represent our planning units. Although _prioritizr_ can support many different types of planning unit data, here our planning units are represented as polygons in a vector format (ie. `SpatialPolygonsDataFrame`). Each polygon represents a planning unit. This object contains 90 planning units. The attribute table associates each planning unit an acquisition cost ("cost" column), and a value indicating if the unit is inside a simulated protected area ("locked_in" column). Let's explore the planning unit data.


```r
# load planning unit data
data(sim_pu_polygons)

# show the first 20 rows in the attribute table
head(sim_pu_polygons@data)
```

```
##       cost locked_in locked_out
## 1 215.8638     FALSE      FALSE
## 2 212.7823     FALSE      FALSE
## 3 207.4962     FALSE      FALSE
## 4 208.9322     FALSE       TRUE
## 5 214.0419     FALSE      FALSE
## 6 213.7636     FALSE      FALSE
```

```r
# plot the planning units and colour by acquisition cost
spplot(sim_pu_polygons, "cost")
```

![plot of chunk unnamed-chunk-4](inst/vign/readme-figure/unnamed-chunk-4-1.png)


```r
# plot the planning units and show which planning units are inside protected
# areas. Units inside protected areas are shown in yellow.
spplot(sim_pu_polygons, "locked_in")
```

![plot of chunk unnamed-chunk-5](inst/vign/readme-figure/unnamed-chunk-5-1.png)

Biodiversity features are represented using a stack of raster data (ie. `RasterStack` objects). A `RasterStack` represents a collection of `RasterLayers` with the same spatial properties (ie. spatial extent, coordinate system, dimensionality, and resolution). Each `RasterLayer` in the stack describes the distribution of a biodiversity feature.

In our example, the `sim_features` object is a `RasterStack` object that contains 5 layers. Each `RasterLayer` describes the distribution of a species. Specifically, the cell values denote the proportion of suitable habitat in across the study area. For a given layer, cells with a value of one are entirely comprized of suitable habitat for the feature, and cells with a value of zero contain no suitable habitat.


```r
# load feature data
data(sim_features)
# plot the distribution of suitable habitat for each feature
plot(sim_features, main = paste("Feature", seq_len(nlayers(sim_features))),
     nr = 1)
```

![plot of chunk unnamed-chunk-6](inst/vign/readme-figure/unnamed-chunk-6-1.png)

We want to develop a reserve network that will secure 20 % of the distribution for each feature for minimal cost. In this planning scenario, we can either purchase all of the land inside a given planning unit, or none of the land inside a given planning unit. Thus we will create a new [`problem`](https://prioritizr.github.io/prioritizr/reference/problem.html) that will use a minimum set objective ([`add_min_set_objective`](https://prioritizr.github.io/prioritizr/reference/add_min_set_objective.html)), with relative targets of 20 % ([`add_relative_targets`](https://prioritizr.github.io/prioritizr/reference/add_relative_targets.html)), and binary decisions (`[add_binary_decisions](https://prioritizr.github.io/prioritizr/reference/add_binary_decisions.html)`).


```r
# create problem
p1 <- problem(sim_pu_polygons, features = sim_features, cost_name = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions()
```

After we have built a [`problem`](https://prioritizr.github.io/prioritizr/reference/problem.html), we can solve it to obtain a solution. Since we have not specified the method used to solve the problem, _prioritizr_ will automatically use the best solver currently installed. *It is strongly encouraged to install the [Gurobi software suite and the _gurobi_ _R_ package to solve problems quickly](http://gurobi.com).*


```r
# solve the problem
s1 <- solve(p1)
```

```
## Optimize a model with 5 rows, 90 columns and 450 nonzeros
## Variable types: 0 continuous, 90 integer (90 binary)
## Coefficient statistics:
##   Matrix range     [2e-01, 9e-01]
##   Objective range  [2e+02, 2e+02]
##   Bounds range     [1e+00, 1e+00]
##   RHS range        [6e+00, 1e+01]
## Found heuristic solution: objective 4135.27
## Presolve time: 0.00s
## Presolved: 5 rows, 90 columns, 450 nonzeros
## Variable types: 0 continuous, 90 integer (90 binary)
## Presolved: 5 rows, 90 columns, 450 nonzeros
## 
## 
## Root relaxation: objective 3.490348e+03, 17 iterations, 0.00 seconds
## 
##     Nodes    |    Current Node    |     Objective Bounds      |     Work
##  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
## 
##      0     0 3490.34813    0    4 4135.27447 3490.34813  15.6%     -    0s
## H    0     0                    3597.0951275 3490.34813  2.97%     -    0s
## 
## Explored 0 nodes (17 simplex iterations) in 0.00 seconds
## Thread count was 1 (of 4 available processors)
## 
## Solution count 2: 3597.1 4135.27 
## Pool objective bound 3490.35
## 
## Optimal solution found (tolerance 1.00e-01)
## Best objective 3.597095127479e+03, best bound 3.490348127696e+03, gap 2.9676%
```

```r
# plot the solution
s1$solution <- factor(s1$solution)
spplot(s1, "solution", col.regions = c('grey80', 'darkgreen'))
```

![plot of chunk unnamed-chunk-8](inst/vign/readme-figure/unnamed-chunk-8-1.png)

Although this solution adequately conserves each feature, it is inefficient because it does not consider the fact some of the planning units are already inside protected areas. Since our vector data contains information on which planning units are inside protected areas in the `"locked_in"` column, we can add constraints to ensure they are prioritized in the solution ([`add_locked_in_constraints`](https://prioritizr.github.io/prioritizr/reference/add_locked_in_constraints.html)).


```r
# create problem with locked in constraints added to it
p2 <- p1 %>% add_locked_in_constraints("locked_in")
# solve the problem
s2 <- solve(p2)
```

```
## Optimize a model with 5 rows, 90 columns and 450 nonzeros
## Variable types: 0 continuous, 90 integer (90 binary)
## Coefficient statistics:
##   Matrix range     [2e-01, 9e-01]
##   Objective range  [2e+02, 2e+02]
##   Bounds range     [1e+00, 1e+00]
##   RHS range        [6e+00, 1e+01]
## Found heuristic solution: objective 4020.2
## Presolve removed 0 rows and 10 columns
## Presolve time: 0.00s
## Presolved: 5 rows, 80 columns, 400 nonzeros
## Variable types: 0 continuous, 80 integer (80 binary)
## Presolved: 5 rows, 80 columns, 400 nonzeros
## 
## 
## Root relaxation: objective 3.620461e+03, 11 iterations, 0.00 seconds
## 
##     Nodes    |    Current Node    |     Objective Bounds      |     Work
##  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
## 
##      0     0 3620.46082    0    3 4020.20382 3620.46082  9.94%     -    0s
## 
## Explored 0 nodes (11 simplex iterations) in 0.00 seconds
## Thread count was 1 (of 4 available processors)
## 
## Solution count 1: 4020.2 
## Pool objective bound 3620.46
## 
## Optimal solution found (tolerance 1.00e-01)
## Best objective 4.020203818008e+03, best bound 3.620460824006e+03, gap 9.9434%
```

```r
# plot the solution
s2$solution <- factor(s2$solution)
spplot(s2, "solution", col.regions = c('grey80', 'darkgreen'))
```

![plot of chunk unnamed-chunk-9](inst/vign/readme-figure/unnamed-chunk-9-1.png)

This solution is an improvement over the the previous solution. However, it is also highly fragmented. As a consequence, implementing this solution may be associated with increased management costs and be susceptible to edge effects. We can further constrain the solution by adding penalties that punish overly fragmented solutions ([`add_boundary_penalties`](https://prioritizr.github.io/prioritizr/reference/add_boundary_penalties.html)). Here we will use a penalty factor of 1 (ie. boundary length modifier; BLM), and an edge factor of 50 % so that planning units along the coastline are not overly penalized.


```r
# create problem with boundary penalties added to it
p3 <- p2 %>% add_boundary_penalties(penalty = 500, edge_factor = 0.5)
# solve the problem
s3 <- solve(p3)
```

```
## Optimize a model with 293 rows, 234 columns and 1026 nonzeros
## Variable types: 0 continuous, 234 integer (234 binary)
## Coefficient statistics:
##   Matrix range     [2e-01, 1e+00]
##   Objective range  [1e+02, 4e+02]
##   Bounds range     [1e+00, 1e+00]
##   RHS range        [6e+00, 1e+01]
## Found heuristic solution: objective 6420.2
## Presolve removed 72 rows and 46 columns
## Presolve time: 0.00s
## Presolved: 221 rows, 188 columns, 832 nonzeros
## Variable types: 0 continuous, 188 integer (188 binary)
## Presolved: 221 rows, 188 columns, 832 nonzeros
## 
## 
## Root relaxation: objective 5.477092e+03, 245 iterations, 0.00 seconds
## 
##     Nodes    |    Current Node    |     Objective Bounds      |     Work
##  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
## 
##      0     0 5477.09167    0   89 6420.20382 5477.09167  14.7%     -    0s
## H    0     0                    6230.2475434 5477.09167  12.1%     -    0s
## H    0     0                    6162.3460667 5477.09167  11.1%     -    0s
##      0     0 5493.46801    0  108 6162.34607 5493.46801  10.9%     -    0s
##      0     0 5513.60658    0   93 6162.34607 5513.60658  10.5%     -    0s
## H    0     0                    5878.3503827 5513.60658  6.20%     -    0s
## 
## Cutting planes:
##   Gomory: 1
## 
## Explored 0 nodes (278 simplex iterations) in 0.07 seconds
## Thread count was 1 (of 4 available processors)
## 
## Solution count 4: 5878.35 6162.35 6230.25 6420.2 
## Pool objective bound 5513.61
## 
## Optimal solution found (tolerance 1.00e-01)
## Best objective 5.878350382730e+03, best bound 5.513606581997e+03, gap 6.2049%
```

```r
# plot the solution
s3$solution <- factor(s3$solution)
spplot(s3, "solution", col.regions = c('grey80', 'darkgreen'))
```

![plot of chunk unnamed-chunk-10](inst/vign/readme-figure/unnamed-chunk-10-1.png)

This solution is even better then the previous solution. However, we are not content just yet. This solution does not maintain connectivity between reserves, and so species may have limited ability to disperse throughout the reserve network. To avoid this, we can add connected constraints ([`add_connected_constraints`](https://prioritizr.github.io/prioritizr/reference/add_connected_constraints.html)).


```r
# create problem with connected constraints
p4 <- p3 %>% add_connected_constraints()
# solve the problem
s4 <- solve(p4)
```

```
## Optimize a model with 655 rows, 506 columns and 2293 nonzeros
## Variable types: 0 continuous, 506 integer (506 binary)
## Coefficient statistics:
##   Matrix range     [2e-01, 1e+00]
##   Objective range  [1e+02, 4e+02]
##   Bounds range     [1e+00, 1e+00]
##   RHS range        [1e+00, 1e+01]
## Presolve removed 187 rows and 129 columns
## Presolve time: 0.01s
## Presolved: 468 rows, 377 columns, 1688 nonzeros
## Variable types: 0 continuous, 377 integer (376 binary)
## Presolved: 468 rows, 377 columns, 1688 nonzeros
## 
## 
## Root relaxation: objective 5.647064e+03, 500 iterations, 0.01 seconds
## 
##     Nodes    |    Current Node    |     Objective Bounds      |     Work
##  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
## 
##      0     0 5647.06401    0  153          - 5647.06401      -     -    0s
##      0     0 5890.44226    0   78          - 5890.44226      -     -    0s
##      0     0 5929.07276    0  120          - 5929.07276      -     -    0s
##      0     0 5961.21299    0  117          - 5961.21299      -     -    0s
##      0     0 5966.92740    0  112          - 5966.92740      -     -    0s
##      0     0 5979.36953    0  115          - 5979.36953      -     -    0s
##      0     0 5996.08152    0  125          - 5996.08152      -     -    0s
##      0     0 5996.08152    0  125          - 5996.08152      -     -    0s
##      0     0 6004.87609    0  108          - 6004.87609      -     -    0s
##      0     0 6005.23390    0  107          - 6005.23390      -     -    0s
##      0     0 6006.41853    0  123          - 6006.41853      -     -    0s
##      0     0 6006.58145    0  122          - 6006.58145      -     -    0s
##      0     0 6006.67025    0  122          - 6006.67025      -     -    0s
##      0     0 6006.67025    0  121          - 6006.67025      -     -    0s
## H    0     0                    12170.818346 6006.67025  50.6%     -    0s
##      0     2 6009.61026    0  121 12170.8183 6009.61026  50.6%     -    0s
## *    9     7               7    6451.9495997 6009.61026  6.86%  23.4    0s
## 
## Cutting planes:
##   Gomory: 5
##   MIR: 2
##   Zero half: 22
## 
## Explored 10 nodes (1090 simplex iterations) in 0.19 seconds
## Thread count was 1 (of 4 available processors)
## 
## Solution count 2: 6451.95 12170.8 
## Pool objective bound 6009.61
## 
## Optimal solution found (tolerance 1.00e-01)
## Best objective 6.451949599667e+03, best bound 6.009610264212e+03, gap 6.8559%
```

```r
# plot the solution
s4$solution <- factor(s4$solution)
spplot(s4, "solution", col.regions = c('grey80', 'darkgreen'))
```

![plot of chunk unnamed-chunk-11](inst/vign/readme-figure/unnamed-chunk-11-1.png)

This short example demonstrates how the _prioritizr_ package can be used to build a minimal conservation problem, and how constraints can be iteratively added to the problem to obtain a solution that fulfils the needs of the conservation planner. Here, we explored several constraints using the minimum set objective. The _prioritizr_ package provides many other constraints and also different objectives that can be used to build a conservation problem.
