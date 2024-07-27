
<!--- README.md is generated from README.Rmd. Please edit that file -->

# prioritizr <img src="man/figures/logo.png" align="right" style="width:140px"/>

## Systematic Conservation Prioritization in R

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/Lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check-Ubuntu](https://img.shields.io/github/actions/workflow/status/prioritizr/prioritizr/R-CMD-check-ubuntu.yaml?branch=main&label=Ubuntu)](https://github.com/prioritizr/prioritizr/actions)
[![R-CMD-check-Windows](https://img.shields.io/github/actions/workflow/status/prioritizr/prioritizr/R-CMD-check-windows.yaml?branch=main&label=Windows)](https://github.com/prioritizr/prioritizr/actions)
[![R-CMD-check-macOS](https://img.shields.io/github/actions/workflow/status/prioritizr/prioritizr/R-CMD-check-macos.yaml?branch=main&label=macOS)](https://github.com/prioritizr/prioritizr/actions)
[![Documentation](https://img.shields.io/github/actions/workflow/status/prioritizr/prioritizr/documentation.yaml?branch=main&label=Documentation)](https://github.com/prioritizr/prioritizr/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/prioritizr/prioritizr?label=Coverage)](https://app.codecov.io/gh/prioritizr/prioritizr/branch/main)
[![CRAN-Status-Badge](http://www.r-pkg.org/badges/version/prioritizr)](https://CRAN.R-project.org/package=prioritizr)
<!-- badges: end -->

The *prioritizr R* package uses mixed integer linear programming (MILP)
techniques to provide a flexible interface for building and solving
conservation planning problems. It supports a broad range of objectives,
constraints, and penalties that can be used to custom-tailor
conservation planning problems to the specific needs of a conservation
planning exercise. Once built, conservation planning problems can be
solved using a variety of commercial and open-source exact algorithm
solvers. In contrast to the algorithms conventionally used to solve
conservation problems, such as heuristics or simulated annealing, the
exact algorithms used here are guaranteed to find optimal solutions.
Furthermore, conservation problems can be constructed to optimize the
spatial allocation of different management actions or zones, meaning
that conservation practitioners can identify solutions that benefit
multiple stakeholders. Finally, this package has the functionality to
read input data formatted for the *Marxan* conservation planning
program, and find much cheaper solutions in a much shorter period of
time than *Marxan*.

## Installation

#### Official version

The latest official version of the *prioritizr R* package can be
installed from the [Comprehensive R Archive Network
(CRAN)](https://cran.r-project.org/) using the following *R* code.

``` r
install.packages("prioritizr", repos = "https://cran.rstudio.com/")
```

#### Developmental version

The latest development version can be installed to gain access to new
functionality that is not yet present in the latest official version.
Please note that the developmental version is more likely to contain
coding errors than the official version. To install the developmental
version, you can install it directly from the [GitHub online code
repository](https://github.com/prioritizr/prioritizr) or from the [R
Universe](https://prioritizr.r-universe.dev/prioritizr). In general, we
recommend installing the developmental version from the [R
Universe](https://prioritizr.r-universe.dev/prioritizr). This is because
installation via [R
Universe](https://prioritizr.r-universe.dev/prioritizr) does not require
any additional software (e.g.,
[RTools](https://cran.r-project.org/bin/windows/Rtools/) for Windows
systems, or [Xcode and gfortran](https://mac.r-project.org/tools/) for
macOS systems).

-   To install the latest development version from [R
    Universe](https://prioritizr.r-universe.dev/prioritizr), use the
    following *R* code.

    ``` r
    install.packages(
      "prioritizr",
      repos = c(
        "https://prioritizr.r-universe.dev",
        "https://cloud.r-project.org"
      )
    )
    ```

-   To install the latest development version from
    [GitHub](https://github.com/prioritizr/prioritizr), use the
    following *R* code.

    ``` r
    if (!require(remotes)) install.packages("remotes")
    remotes::install_github("prioritizr/prioritizr")
    ```

## Citation

Please cite the *prioritizr R* package when using it in publications. To
cite the latest official version, please use:

> Hanson JO, Schuster R, Morrell N, Strimas-Mackey M, Edwards BPM, Watts
> ME, Arcese P, Bennett J, Possingham HP (2024). prioritizr: Systematic
> Conservation Prioritization in R. R package version 8.0.4. Available
> at <https://CRAN.R-project.org/package=prioritizr>.

Alternatively, to cite the latest development version, please use:

> Hanson JO, Schuster R, Morrell N, Strimas-Mackey M, Edwards BPM, Watts
> ME, Arcese P, Bennett J, Possingham HP (2024). prioritizr: Systematic
> Conservation Prioritization in R. R package version 8.0.4.2. Available
> at <https://github.com/prioritizr/prioritizr>.

Additionally, we keep a [record of
publications](https://prioritizr.net/articles/publication_record.html)
that use the *prioritizr R* package. If you use this package in any
reports or publications, please [file an issue on
GitHub](https://github.com/prioritizr/prioritizr/issues/new) so we can
add it to the record.

## Usage

Here we provide a short example showing how the *prioritizr R* package
can be used to build and solve conservation problems. Specifically, we
will use an example dataset available through the *prioritizrdata R*
package. Additionally, we will use the *terra R* package to perform
raster calculations. To begin with, we will load the packages.

``` r
# load packages
library(prioritizr)
library(prioritizrdata)
library(terra)
```

We will use the Washington dataset in this example. To import the
planning unit data, we will use the `get_wa_pu()` function. Although the
*prioritizr R* package can support many different types of planning unit
data, here our planning units are represented as a single-layer raster
(i.e., `terra::rast()` object). Each cell represents a different
planning unit, and cell values denote land acquisition costs.
Specifically, there are 10757 planning units in total (i.e., cells with
non-missing values).

``` r
# import planning unit data
wa_pu <- get_wa_pu()

# preview data
print(wa_pu)
```

    ## class       : SpatRaster 
    ## dimensions  : 109, 147, 1  (nrow, ncol, nlyr)
    ## resolution  : 4000, 4000  (x, y)
    ## extent      : -1816382, -1228382, 247483.5, 683483.5  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs 
    ## source      : wa_pu.tif 
    ## name        :         cost 
    ## min value   :    0.2986647 
    ## max value   : 1804.1838379

``` r
# plot data
plot(wa_pu, main = "Costs", axes = FALSE)
```

<img src="man/figures/README-planning_units-1.png" width="500" style="display: block; margin: auto;" />

Next, we will use the `get_wa_features()` function to import the
conservation feature data. Although the *prioritizr R* package can
support many different types of feature data, here our feature data are
represented as a multi-layer raster (i.e., `terra::rast()` object). Each
layer describes the spatial distribution of a feature. Here, our feature
data correspond to different bird species. To account for migratory
patterns, the breeding and non-breeding distributions of species are
represented as different features. Specifically, the cell values denote
the relative abundance of individuals, with higher values indicating
greater abundance.

``` r
# import feature data
wa_features <- get_wa_features()

# preview data
print(wa_features)
```

    ## class       : SpatRaster 
    ## dimensions  : 109, 147, 396  (nrow, ncol, nlyr)
    ## resolution  : 4000, 4000  (x, y)
    ## extent      : -1816382, -1228382, 247483.5, 683483.5  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs 
    ## source      : wa_features.tif 
    ## names       : Recur~ding), Botau~ding), Botau~ding), Corvu~ding), Corvu~ding), Cincl~full), ... 
    ## min values  :       0.000,       0.000,       0.000,       0.000,       0.000,        0.00, ... 
    ## max values  :       0.514,       0.812,       3.129,       0.115,       0.296,        0.06, ...

``` r
# plot the first nine features
plot(wa_features[[1:9]], nr = 3, axes = FALSE)
```

<img src="man/figures/README-features-1.png" width="800" style="display: block; margin: auto;" />

Let’s make sure that you have a solver installed on your computer. This
is important so that you can use optimization algorithms to generate
spatial prioritizations. If this is your first time using the
*prioritizr R* package, please install the HiGHS solver using the
following *R* code. Although the HiGHS solver is relatively fast and
easy to install, please note that you’ll need to install the [Gurobi
software suite and the *gurobi* *R* package](https://www.gurobi.com/)
for best performance (see the [Gurobi Installation
Guide](https://prioritizr.net/articles/gurobi_installation_guide.html)
for details).

``` r
# if needed, install HiGHS solver
install.packages("highs", repos = "https://cran.rstudio.com/")
```

Now, let’s generate a spatial prioritization. To ensure feasibility, we
will set a budget. Specifically, the total cost of the prioritization
will represent a 5% of the total land value in the study area. Given
this budget, we want the prioritization to increase feature
representation, as much as possible, so that each feature would,
ideally, have 20% of its distribution covered by the prioritization. In
this scenario, we can either purchase all of the land inside a given
planning unit, or none of the land inside a given planning unit. Thus we
will create a new `problem()` that will use a minimum shortfall
objective (via `add_min_shortfall_objective()`), with relative targets
of 20% (via `add_relative_targets()`), binary decisions (via
`add_binary_decisions()`), and specify that we want near-optimal
solutions (i.e., 10% from optimality) using the best solver installed on
our computer (via `add_default_solver()`).

``` r
# calculate budget
budget <- terra::global(wa_pu, "sum", na.rm = TRUE)[[1]] * 0.05

# create problem
p1 <-
  problem(wa_pu, features = wa_features) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.1, verbose = FALSE)

# print problem
print(p1)
```

    ## A conservation problem (<ConservationProblem>)
    ## ├•data
    ## │├•features:    "Recurvirostra americana (breeding)", … (396 total)
    ## │└•planning units:
    ## │ ├•data:       <SpatRaster> (10757 total)
    ## │ ├•costs:      continuous values (between 0.2987 and 1804.1838)
    ## │ ├•extent:     -1816381.6182, 247483.5211, -1228381.6182, 683483.5211 (xmin, ymin, xmax, ymax)
    ## │ └•CRS:        +proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs (projected)
    ## ├•formulation
    ## │├•objective:   minimum shortfall objective (`budget` = 8748.4908)
    ## │├•penalties:   none specified
    ## │├•targets:     relative targets (between 0.2 and 0.2)
    ## │├•constraints: none specified
    ## │└•decisions:   binary decision
    ## └•optimization
    ##  ├•portfolio:   default portfolio
    ##  └•solver:      gurobi solver (`gap` = 0.1, `time_limit` = 2147483647, `first_feasible` = FALSE, …)
    ## # ℹ Use `summary(...)` to see complete formulation.

After we have built a `problem()`, we can solve it to obtain a solution.

``` r
# solve the problem
s1 <- solve(p1)

# extract the objective
print(attr(s1, "objective"))
```

    ## solution_1 
    ##   4.463003

``` r
# extract time spent solving the problem
print(attr(s1, "runtime"))
```

    ## solution_1 
    ##      3.622

``` r
# extract state message from the solver
print(attr(s1, "status"))
```

    ## solution_1 
    ##  "OPTIMAL"

``` r
# plot the solution
plot(s1, main = "Solution", axes = FALSE)
```

<img src="man/figures/README-minimal_solution-1.png" width="500" style="display: block; margin: auto;" />

After generating a solution, it is important to evaluate it. Here, we
will calculate the number of planning units selected by the solution,
and the total cost of the solution. We can also check how many
representation targets are met by the solution.

``` r
# calculate number of selected planning units by solution
eval_n_summary(p1, s1)
```

    ## # A tibble: 1 × 2
    ##   summary     n
    ##   <chr>   <dbl>
    ## 1 overall  2308

``` r
# calculate total cost of solution
eval_cost_summary(p1, s1)
```

    ## # A tibble: 1 × 2
    ##   summary  cost
    ##   <chr>   <dbl>
    ## 1 overall 8748.

``` r
# calculate target coverage for the solution
p1_target_coverage <- eval_target_coverage_summary(p1, s1)
print(p1_target_coverage)
```

    ## # A tibble: 396 × 9
    ##    feature   met   total_amount absolute_target absolute_held absolute_shortfall
    ##    <chr>     <lgl>        <dbl>           <dbl>         <dbl>              <dbl>
    ##  1 Recurvir… TRUE         100.             20.0          23.3               0   
    ##  2 Botaurus… TRUE          99.9            20.0          29.1               0   
    ##  3 Botaurus… TRUE         100.             20.0          34.0               0   
    ##  4 Corvus b… TRUE          99.9            20.0          20.2               0   
    ##  5 Corvus b… FALSE         99.9            20.0          18.5               1.53
    ##  6 Cinclus … TRUE         100.             20.0          20.5               0   
    ##  7 Spinus t… TRUE          99.9            20.0          22.2               0   
    ##  8 Spinus t… TRUE          99.9            20.0          22.6               0   
    ##  9 Falco sp… TRUE          99.9            20.0          24.3               0   
    ## 10 Falco sp… TRUE         100.             20.0          24.2               0   
    ## # ℹ 386 more rows
    ## # ℹ 3 more variables: relative_target <dbl>, relative_held <dbl>,
    ## #   relative_shortfall <dbl>

``` r
# check percentage of the features that have their target met given the solution
print(mean(p1_target_coverage$met) * 100)
```

    ## [1] 96.21212

Although this solution helps meet the representation targets, it does
not account for existing protected areas inside the study area. As such,
it does not account for the possibility that some features could be
partially – or even fully – represented by existing protected areas and,
in turn, might fail to identify meaningful priorities for new protected
areas. To address this issue, we will use the `get_wa_locked_in()`
function to import spatial data for protected areas in the study area.
We will then add constraints to the `problem()` to ensure they are
selected by the solution (via `add_locked_in_constraints()`).

``` r
# import locked in data
wa_locked_in <- get_wa_locked_in()

# print data
print(wa_locked_in)
```

    ## class       : SpatRaster 
    ## dimensions  : 109, 147, 1  (nrow, ncol, nlyr)
    ## resolution  : 4000, 4000  (x, y)
    ## extent      : -1816382, -1228382, 247483.5, 683483.5  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs 
    ## source      : wa_locked_in.tif 
    ## name        : protected areas 
    ## min value   :               0 
    ## max value   :               1

``` r
# plot data
plot(wa_locked_in, main = "Existing protected areas", axes = FALSE)
```

<img src="man/figures/README-locked_in_constraints-1.png" width="500" style="display: block; margin: auto;" />

``` r
# create new problem with locked in constraints added to it
p2 <-
  p1 %>%
  add_locked_in_constraints(wa_locked_in)

# solve the problem
s2 <- solve(p2)

# plot the solution
plot(s2, main = "Solution", axes = FALSE)
```

<img src="man/figures/README-locked_in_constraints-2.png" width="500" style="display: block; margin: auto;" />

This solution is an improvement over the previous solution. However,
there are some places in the study area that are not available for
protected area establishment (e.g., due to land tenure). As a
consequence, the solution might not be practical for implementation,
because it might select some places that are not available for
protection. To address this issue, we will use the `get_wa_locked_out()`
function to import spatial data describing which planning units are not
available for protection. We will then add constraints to the
`problem()` to ensure they are not selected by the solution (via
`add_locked_out_constraints()`).

``` r
# import locked out data
wa_locked_out <- get_wa_locked_out()

# print data
print(wa_locked_out)
```

    ## class       : SpatRaster 
    ## dimensions  : 109, 147, 1  (nrow, ncol, nlyr)
    ## resolution  : 4000, 4000  (x, y)
    ## extent      : -1816382, -1228382, 247483.5, 683483.5  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs 
    ## source      : wa_locked_out.tif 
    ## name        : urban areas 
    ## min value   :           0 
    ## max value   :           1

``` r
# plot data
plot(wa_locked_out, main = "Areas not available for protection", axes = FALSE)
```

<img src="man/figures/README-locked_out_constraints-1.png" width="500" style="display: block; margin: auto;" />

``` r
# create new problem with locked out constraints added to it
p3 <-
  p2 %>%
  add_locked_out_constraints(wa_locked_out)

# solve the problem
s3 <- solve(p3)

# plot the solution
plot(s3, main = "Solution", axes = FALSE)
```

<img src="man/figures/README-locked_out_constraints-2.png" width="500" style="display: block; margin: auto;" />

This solution is even better then the previous solution. However, we are
not finished yet. The planning units selected by the solution are fairly
fragmented. This can cause issues because fragmentation increases
management costs and reduces conservation benefits through edge effects.
To address this issue, we can further modify the problem by adding
penalties that punish overly fragmented solutions (via
`add_boundary_penalties()`). Here we will use a penalty factor (i.e.,
boundary length modifier) of 0.003, and an edge factor of 50% so that
planning units that occur on the outer edge of the study area are not
overly penalized.

``` r
# create new problem with boundary penalties added to it
p4 <-
  p3 %>%
  add_boundary_penalties(penalty = 0.003, edge_factor = 0.5)

# solve the problem
s4 <- solve(p4)

# plot the solution
plot(s4, main = "Solution", axes = FALSE)
```

<img src="man/figures/README-boundary_penalties-1.png" width="500" style="display: block; margin: auto;" />

Now, let’s explore which planning units selected by the solution are
most important for cost-effectively meeting the targets. To achieve
this, we will calculate importance (irreplaceability) scores using the
Ferrier method. Although this method produces scores for each feature
separately, we will examine the total scores that summarize overall
importance across all features.

``` r
# calculate importance scores
rc <-
  p4 %>%
  eval_ferrier_importance(s4)

# print scores
print(rc)
```

    ## class       : SpatRaster 
    ## dimensions  : 109, 147, 397  (nrow, ncol, nlyr)
    ## resolution  : 4000, 4000  (x, y)
    ## extent      : -1816382, -1228382, 247483.5, 683483.5  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs 
    ## source(s)   : memory
    ## varnames    : wa_pu 
    ##               wa_pu 
    ##               wa_pu 
    ##               ...
    ## names       :  Recur~ding),  Botau~ding),  Botau~ding),  Corvu~ding),  Corvu~ding),  Cincl~full), ... 
    ## min values  : 0.0000000000, 0.0000000000, 0.0000000000, 0.000000e+00, 0.000000e+00, 0.000000e+00, ... 
    ## max values  : 0.0003227724, 0.0002213034, 0.0006622152, 7.771815e-05, 8.974447e-05, 8.483296e-05, ...

``` r
# plot the total importance scores
## note that gray cells are not selected by the prioritization
plot(
  rc[["total"]], main = "Importance scores", axes = FALSE,
  breaks = c(0, 1e-10, 0.005, 0.01, 0.025),
  col = c("#e5e5e5", "#fff7ec", "#fc8d59", "#7f0000")
)
```

<img src="man/figures/README-importance-1.png" width="500" style="display: block; margin: auto;" />

This short example demonstrates how the *prioritizr R* package can be
used to build and customize conservation problems, and then solve them
to generate solutions. Although we explored just a few different
functions for modifying a conservation problem, the package provides
many functions for specifying objectives, constraints, penalties, and
decision variables, so that you can build and custom-tailor conservation
planning problems to suit your planning scenario.

## Learning resources

The [package website](https://prioritizr.net/index.html) contains
information on the *prioritizr R* package. Here you can find
[documentation for every function and built-in
dataset](https://prioritizr.net/reference/index.html), and [news
describing the updates in each package
version](https://prioritizr.net/news/index.html). It also contains the
following articles and tutorials.

-   [**Getting
    started**](https://prioritizr.net/articles/prioritizr.html): Short
    tutorial on using the package.
-   [**Package
    overview**](https://prioritizr.net/articles/package_overview.html):
    Introduction to systematic conservation planning and a comprehensive
    overview of the package.
-   [**Connectivity
    tutorial**](https://prioritizr.net/articles/connectivity_tutorial.html):
    Tutorial on incorporating connectivity into prioritizations.
-   [**Calibrating trade-offs
    tutorial**](https://prioritizr.net/articles/calibrating_trade-offs_tutorial.html):
    Tutorial on running calibration analyses to satisfy multiple
    criteria.
-   [**Management zones
    tutorial**](https://prioritizr.net/articles/management_zones_tutorial.html):
    Tutorial on incorporating multiple management zones and actions into
    prioritizations.
-   [**Gurobi installation
    guide**](https://prioritizr.net/articles/gurobi_installation_guide.html):
    Instructions for installing the *Gurobi* optimization suite for
    generating prioritizations.
-   [**Solver
    benchmarks**](https://prioritizr.net/articles/solver_benchmarks.html):
    Performance comparison of optimization solvers for generating
    prioritizations.
-   [**Publication
    record**](https://prioritizr.net/articles/publication_record.html):
    List of publications that have cited the package.

Additional resources can also be found in [online repositories under the
*prioritizr* organization](https://github.com/prioritizr). These
resources include [slides for talks and seminars about the
package](https://github.com/prioritizr/teaching). Additionally, workshop
materials are available too (e.g., the [Carleton 2023
workshop](https://prioritizr.github.io/workshop/) and [ECCB 2024
workshop](https://iiasa.github.io/eccb2024/)).

## Getting help

If you have any questions about the *prioritizr R* package or
suggestions for improving it, please [post an issue on the code
repository](https://github.com/prioritizr/prioritizr/issues/new).
