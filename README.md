
<!--- README.md is generated from README.Rmd. Please edit that file -->
Systematic Conservation Prioritization in R <img src="man/figures/logo.png" align="right" width=10% />
======================================================================================================

[![lifecycle](https://img.shields.io/badge/Lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable) [![Travis Build Status](https://img.shields.io/travis/prioritizr/prioritizr/master.svg?label=Linux%20%26%20Mac%20OSX)](https://travis-ci.org/prioritizr/prioritizr) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/jeffreyhanson/prioritizr/master.svg?label=Windows)](https://ci.appveyor.com/project/jeffreyhanson/prioritizr) [![Coverage Status](https://codecov.io/github/prioritizr/prioritizr/coverage.svg?branch=master)](https://codecov.io/github/prioritizr/prioritizr?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/prioritizr)](https://CRAN.R-project.org/package=prioritizr)

The *prioritizr R* package uses integer linear programming (ILP) techniques to provide a flexible interface for building and solving conservation planning problems. It supports a broad range of objectives, constraints, and penalties that can be used to custom-tailor conservation planning problems to the specific needs of a conservation planning exercise. Once built, conservation planning problems can be solved using a variety of commercial and open-source exact algorithm solvers. In contrast to the algorithms conventionally used to solve conservation problems, such as heuristics or simulated annealing, the exact algorithms used here are guaranteed to find optimal solutions. Furthermore, conservation problems can be constructed to optimize the spatial allocation of different management actions or zones, meaning that conservation practitioners can identify solutions that benefit multiple stakeholders. Finally, this package has the functionality to read input data formatted for the *Marxan* conservation planning program, and find much cheaper solutions in a much shorter period of time than *Marxan*.

Installation
------------

The latest official version of the *prioritizr R* package can be installed using the following *R* code.

``` r
install.packages("prioritizr", repos = "https://cran.rstudio.com/")
```

Alternatively, the latest development version can be installed using the following code. Please note that while developmental versions may contain additional features not present in the official version, they may also contain coding errors.

``` r
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("prioritizr/prioritizr")
```

Citation
--------

Please using the following citation to cite the *prioritizr R* package in publications:

Hanson JO, Schuster R, Morrell N, Strimas-Mackey M, Watts ME, Arcese P, Bennett J, Possingham HP (2018). prioritizr: Systematic Conservation Prioritization in R. R package version 4.0.0.11. Available at <https://github.com/prioritizr/prioritizr>.

Additionally, we keep a [record of publications](https://prioritizr.github.io/prioritizr/articles/publication_record.html) that use the *prioritizr R* package. If you use this package in any reports or publications, please [file an issue on GitHub](https://github.com/prioritizr/prioritizr/issues/new) so we can add it to the record.

Usage
-----

Here we will provide a short example showing how the *prioritizr R* package can be used to build and solve conservation problems. For brevity, we will use one of the built-in simulated data sets that is distributed with the package. First, we will load the *prioritizr R* package.

``` r
# load package
library(prioritizr)
```

We will use the `sim_pu_polygons` object to represent our planning units. Although the *prioritizr R* can support many different types of planning unit data, here our planning units are represented as polygons in a spatial vector format (i.e. `SpatialPolygonsDataFrame`). Each polygon represents a different planning unit and we have 90 planning units in total. The attribute table associated with this data set contains information describing the acquisition cost of each planning ("cost" column), and a value indicating if the unit is already located in protected area ("locked\_in" column). Let's explore the planning unit data.

``` r
# load planning unit data
data(sim_pu_polygons)

# show the first 6 rows in the attribute table
head(sim_pu_polygons@data)
```

    ##       cost locked_in locked_out
    ## 1 215.8638     FALSE      FALSE
    ## 2 212.7823     FALSE      FALSE
    ## 3 207.4962     FALSE      FALSE
    ## 4 208.9322     FALSE       TRUE
    ## 5 214.0419     FALSE      FALSE
    ## 6 213.7636     FALSE      FALSE

``` r
# plot the planning units and color them according to acquisition cost
spplot(sim_pu_polygons, "cost", main = "Planning unit cost",
       xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="400" style="display: block; margin: auto;" />

``` r
# plot the planning units and show which planning units are inside protected
# areas (colored in yellow)
spplot(sim_pu_polygons, "locked_in", main = "Planning units in protected areas",
       xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="400" style="display: block; margin: auto;" />

Conservation features are represented using a stack of raster data (i.e. `RasterStack` objects). A `RasterStack` represents a collection of `RasterLayers` with the same spatial properties (i.e. spatial extent, coordinate system, dimensionality, and resolution). Each `RasterLayer` in the stack describes the distribution of a conservation feature.

In our example, the `sim_features` object is a `RasterStack` object that contains 5 layers. Each `RasterLayer` describes the distribution of a species. Specifically, the pixel values denote the proportion of suitable habitat across different areas inside the study area. For a given layer, pixels with a value of one are comprised entirely of suitable habitat for the feature, and pixels with a value of zero contain no suitable habitat.

``` r
# load feature data
data(sim_features)

# plot the distribution of suitable habitat for each feature
plot(sim_features, main = paste("Feature", seq_len(nlayers(sim_features))),
     nr = 2)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="800" style="display: block; margin: auto;" />

Let's say that we want to develop a reserve network that will secure 20 % of the distribution for each feature in the study area for minimal cost. In this planning scenario, we can either purchase all of the land inside a given planning unit, or none of the land inside a given planning unit. Thus we will create a new [`problem`](https://prioritizr.github.io/prioritizr/reference/problem.html) that will use a minimum set objective ([`add_min_set_objective`](https://prioritizr.github.io/prioritizr/reference/add_min_set_objective.html)), with relative targets of 20 % ([`add_relative_targets`](https://prioritizr.github.io/prioritizr/reference/add_relative_targets.html)), and binary decisions ([`add_binary_decisions`](https://prioritizr.github.io/prioritizr/reference/add_binary_decisions.html)).

``` r
# create problem
p1 <- problem(sim_pu_polygons, features = sim_features,
              cost_column = "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(0.2) %>%
      add_binary_decisions()
```

After we have built a [`problem`](https://prioritizr.github.io/prioritizr/reference/problem.html), we can solve it to obtain a solution. Since we have not specified the method used to solve the problem, *prioritizr* will automatically use the best solver currently installed. **It is strongly encouraged to install the [Gurobi software suite and the *gurobi* *R* package to solve problems quickly](http://gurobi.com), for more information on this please refer to the [Gurobi Installation Gude](https://prioritizr.github.io/prioritizr/articles/gurobi_installation.html)**

``` r
# solve the problem
s1 <- solve(p1)
```

    ## Optimize a model with 5 rows, 90 columns and 450 nonzeros
    ## Variable types: 0 continuous, 90 integer (90 binary)
    ## Coefficient statistics:
    ##   Matrix range     [2e-01, 9e-01]
    ##   Objective range  [2e+02, 2e+02]
    ##   Bounds range     [1e+00, 1e+00]
    ##   RHS range        [6e+00, 1e+01]
    ## Found heuristic solution: objective 3934.6218396
    ## Presolve time: 0.00s
    ## Presolved: 5 rows, 90 columns, 450 nonzeros
    ## Variable types: 0 continuous, 90 integer (90 binary)
    ## Presolved: 5 rows, 90 columns, 450 nonzeros
    ## 
    ## 
    ## Root relaxation: objective 3.496032e+03, 16 iterations, 0.00 seconds
    ## 
    ##     Nodes    |    Current Node    |     Objective Bounds      |     Work
    ##  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
    ## 
    ##      0     0 3496.03193    0    4 3934.62184 3496.03193  11.1%     -    0s
    ## H    0     0                    3585.9601335 3496.03193  2.51%     -    0s
    ## 
    ## Explored 1 nodes (16 simplex iterations) in 0.01 seconds
    ## Thread count was 1 (of 4 available processors)
    ## 
    ## Solution count 2: 3585.96 3934.62 
    ## 
    ## Optimal solution found (tolerance 1.00e-01)
    ## Best objective 3.585960133519e+03, best bound 3.496031931890e+03, gap 2.5078%

``` r
# extract the objective (cost of solution in this case)
print(attr(s1, "objective"))
```

    ## solution_1 
    ##    3585.96

``` r
# extract time spent solving the problem
print(attr(s1, "runtime"))
```

    ##  solution_1 
    ## 0.006181002

``` r
# extract state message from the solver
print(attr(s1, "status"))
```

    ## solution_1 
    ##  "OPTIMAL"

``` r
# plot the solution
s1$solution_1 <- factor(s1$solution_1)
spplot(s1, "solution_1", col.regions = c('grey90', 'darkgreen'),
       main = "Solution", xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="400" style="display: block; margin: auto;" />

Although this solution adequately conserves each feature, it is inefficient because it does not consider the fact some of the planning units are already inside protected areas. Since our planning unit data contains information on which planning units are already inside protected areas (in the `"locked_in"` column of the attribute table), we can add constraints to ensure they are prioritized in the solution ([`add_locked_in_constraints`](https://prioritizr.github.io/prioritizr/reference/add_locked_in_constraints.html)).

``` r
# create new problem with locked in constraints added to it
p2 <- p1 %>% add_locked_in_constraints("locked_in")

# solve the problem
s2 <- solve(p2)
```

    ## Optimize a model with 5 rows, 90 columns and 450 nonzeros
    ## Variable types: 0 continuous, 90 integer (90 binary)
    ## Coefficient statistics:
    ##   Matrix range     [2e-01, 9e-01]
    ##   Objective range  [2e+02, 2e+02]
    ##   Bounds range     [1e+00, 1e+00]
    ##   RHS range        [6e+00, 1e+01]
    ## Found heuristic solution: objective 4017.6427161
    ## Presolve removed 0 rows and 10 columns
    ## Presolve time: 0.00s
    ## Presolved: 5 rows, 80 columns, 400 nonzeros
    ## Variable types: 0 continuous, 80 integer (80 binary)
    ## Presolved: 5 rows, 80 columns, 400 nonzeros
    ## 
    ## 
    ## Root relaxation: objective 3.610717e+03, 15 iterations, 0.00 seconds
    ## 
    ##     Nodes    |    Current Node    |     Objective Bounds      |     Work
    ##  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
    ## 
    ##      0     0 3610.71743    0    4 4017.64272 3610.71743  10.1%     -    0s
    ## H    0     0                    3649.3763088 3610.71743  1.06%     -    0s
    ## 
    ## Explored 1 nodes (15 simplex iterations) in 0.00 seconds
    ## Thread count was 1 (of 4 available processors)
    ## 
    ## Solution count 2: 3649.38 4017.64 
    ## 
    ## Optimal solution found (tolerance 1.00e-01)
    ## Best objective 3.649376308848e+03, best bound 3.610717428789e+03, gap 1.0593%

``` r
# plot the solution
s2$solution_1 <- factor(s2$solution_1)
spplot(s2, "solution_1", col.regions = c('grey90', 'darkgreen'),
       main = "Solution", xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="400" style="display: block; margin: auto;" />

This solution is an improvement over the previous solution. However, it is also highly fragmented. As a consequence, this solution may be associated with increased management costs and the species in this scenario may not benefit substantially from this solution due to edge effects. We can further modify the problem by adding penalties that punish overly fragmented solutions ([`add_boundary_penalties`](https://prioritizr.github.io/prioritizr/reference/add_boundary_penalties.html)). Here we will use a penalty factor of 1 (i.e. boundary length modifier; BLM), and an edge factor of 50 % so that planning units that occur outer edge of the study area are not overly penalized.

``` r
# create new problem with boundary penalties added to it
p3 <- p2 %>% add_boundary_penalties(penalty = 500, edge_factor = 0.5)

# solve the problem
s3 <- solve(p3)
```

    ## Optimize a model with 293 rows, 234 columns and 1026 nonzeros
    ## Variable types: 0 continuous, 234 integer (234 binary)
    ## Coefficient statistics:
    ##   Matrix range     [2e-01, 1e+00]
    ##   Objective range  [1e+02, 4e+02]
    ##   Bounds range     [1e+00, 1e+00]
    ##   RHS range        [6e+00, 1e+01]
    ## Found heuristic solution: objective 20287.196992
    ## Found heuristic solution: objective 6551.3909374
    ## Presolve removed 66 rows and 43 columns
    ## Presolve time: 0.00s
    ## Presolved: 227 rows, 191 columns, 844 nonzeros
    ## Variable types: 0 continuous, 191 integer (191 binary)
    ## Presolved: 227 rows, 191 columns, 844 nonzeros
    ## 
    ## 
    ## Root relaxation: objective 5.465868e+03, 113 iterations, 0.00 seconds
    ## 
    ##     Nodes    |    Current Node    |     Objective Bounds      |     Work
    ##  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
    ## 
    ##      0     0 5465.86773    0  119 6551.39094 5465.86773  16.6%     -    0s
    ##      0     0 5523.88444    0   88 6551.39094 5523.88444  15.7%     -    0s
    ## H    0     0                    6435.6212242 5523.88444  14.2%     -    0s
    ## H    0     0                    6323.9683139 5523.88444  12.7%     -    0s
    ##      0     0 5541.09124    0  101 6323.96831 5541.09124  12.4%     -    0s
    ## H    0     0                    5967.6038942 5541.09124  7.15%     -    0s
    ## 
    ## Cutting planes:
    ##   Gomory: 2
    ## 
    ## Explored 1 nodes (200 simplex iterations) in 0.05 seconds
    ## Thread count was 1 (of 4 available processors)
    ## 
    ## Solution count 5: 5967.6 6323.97 6435.62 ... 20287.2
    ## 
    ## Optimal solution found (tolerance 1.00e-01)
    ## Best objective 5.967603894163e+03, best bound 5.541091236907e+03, gap 7.1471%

``` r
# plot the solution
s3$solution_1 <- factor(s3$solution_1)
spplot(s3, "solution_1", col.regions = c('grey90', 'darkgreen'),
       main = "Solution", xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="400" style="display: block; margin: auto;" />

This solution is even better then the previous solution. However, we are not finished yet. This solution does not maintain connectivity between reserves, and so species may have limited capacity to disperse throughout the solution. To avoid this, we can add contiguity constraints ([`add_contiguity_constraints`](https://prioritizr.github.io/prioritizr/reference/add_contiguity_constraints.html)).

``` r
# create new problem with contiguity constraints
p4 <- p3 %>% add_contiguity_constraints()

# solve the problem
s4 <- solve(p4)
```

    ## Optimize a model with 654 rows, 506 columns and 2292 nonzeros
    ## Variable types: 0 continuous, 506 integer (506 binary)
    ## Coefficient statistics:
    ##   Matrix range     [2e-01, 1e+00]
    ##   Objective range  [1e+02, 4e+02]
    ##   Bounds range     [1e+00, 1e+00]
    ##   RHS range        [1e+00, 1e+01]
    ## Presolve removed 282 rows and 210 columns
    ## Presolve time: 0.01s
    ## Presolved: 372 rows, 296 columns, 1182 nonzeros
    ## Variable types: 0 continuous, 296 integer (296 binary)
    ## Found heuristic solution: objective 9816.9791056
    ## Found heuristic solution: objective 7879.0795356
    ## Presolve removed 3 rows and 0 columns
    ## Presolved: 369 rows, 296 columns, 1176 nonzeros
    ## 
    ## 
    ## Root relaxation: objective 6.456778e+03, 153 iterations, 0.00 seconds
    ## 
    ##     Nodes    |    Current Node    |     Objective Bounds      |     Work
    ##  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
    ## 
    ##      0     0 6456.77823    0  137 7879.07954 6456.77823  18.1%     -    0s
    ## H    0     0                    7805.4228157 6456.77823  17.3%     -    0s
    ##      0     0 6636.21984    0   88 7805.42282 6636.21984  15.0%     -    0s
    ##      0     0 6668.07936    0  105 7805.42282 6668.07936  14.6%     -    0s
    ##      0     0 6918.46244    0   91 7805.42282 6918.46244  11.4%     -    0s
    ## H    0     0                    7778.5559284 6918.46244  11.1%     -    0s
    ##      0     0 6920.19113    0  104 7778.55593 6920.19113  11.0%     -    0s
    ##      0     0 6920.80033    0  102 7778.55593 6920.80033  11.0%     -    0s
    ##      0     0 6920.91434    0  103 7778.55593 6920.91434  11.0%     -    0s
    ##      0     0 6977.94703    0   97 7778.55593 6977.94703  10.3%     -    0s
    ##      0     0 6985.02370    0   98 7778.55593 6985.02370  10.2%     -    0s
    ##      0     0 6985.75100    0   90 7778.55593 6985.75100  10.2%     -    0s
    ##      0     0 6993.48245    0   99 7778.55593 6993.48245  10.1%     -    0s
    ##      0     0 7000.52722    0  109 7778.55593 7000.52722  10.0%     -    0s
    ##      0     0 7002.81388    0  101 7778.55593 7002.81388  10.0%     -    0s
    ## 
    ## Cutting planes:
    ##   Gomory: 6
    ##   MIR: 2
    ##   Zero half: 15
    ## 
    ## Explored 1 nodes (472 simplex iterations) in 0.07 seconds
    ## Thread count was 1 (of 4 available processors)
    ## 
    ## Solution count 4: 7778.56 7805.42 7879.08 9816.98 
    ## 
    ## Optimal solution found (tolerance 1.00e-01)
    ## Best objective 7.778555928371e+03, best bound 7.002813884239e+03, gap 9.9728%

``` r
# plot the solution
s4$solution_1 <- factor(s4$solution_1)
spplot(s4, "solution_1", col.regions = c('grey90', 'darkgreen'),
       main = "Solution", xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="400" style="display: block; margin: auto;" />

This short example demonstrates how the *prioritizr R* package can be used to build a minimal conservation problem, and how constraints and penalties can be iteratively added to the problem to obtain a solution. Although we explored just a few different functions for modifying the a conservation problem, the *prioritizr R* package provides many functions for specifying objectives, constraints, penalties, and decision variables, so that you can build and custom-tailor a conservation planning problem to suit your exact planning scenario.

Getting help
------------

Please refer to the [package website](https://prioritizr.github.io/prioritizr/index.html) for more information on the *prioritizr R* package. This website contains [a comprehensive tutorial on systematic conservation planning using the package](https://prioritizr.github.io/prioritizr/articles/prioritizr.html), [instructions for installing the *Gurobi* software suite to solve large-scale and complex conservation planning problems](https://prioritizr.github.io/prioritizr/articles/gurobi_installation.html), [a tutorial on building and solving problems that contain multiple management zones](https://prioritizr.github.io/prioritizr/articles/zones.html), and two worked examples involving real-world data in [Tasmania, Australia](https://prioritizr.github.io/prioritizr/articles/tasmania.html) and [Salt Spring Island, Canada](https://prioritizr.github.io/prioritizr/articles/saltspring.html). Additionally, check out the [teaching repository](https://github.com/prioritizr/teaching) for seminar slides and workshop materials. If you have any questions about using the *prioritizr R* package or suggestions from improving it, please [file an issue at the package's online code repository](https://github.com/prioritizr/prioritizr/issues/new).
