
<!--- README.md is generated from README.Rmd. Please edit that file -->

# prioritizr: <img src="man/figures/logo.png" align="right" style="height:90px!important;" />

# Systematic Conservation Prioritization in R

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/Lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check-Ubuntu](https://img.shields.io/github/actions/workflow/status/prioritizr/prioritizr/R-CMD-check-ubuntu.yaml?branch=master&label=Ubuntu)](https://github.com/prioritizr/prioritizr/actions)
[![R-CMD-check-Windows](https://img.shields.io/github/actions/workflow/status/prioritizr/prioritizr/R-CMD-check-windows.yaml?branch=master&label=Windows)](https://github.com/prioritizr/prioritizr/actions)
[![R-CMD-check-macOS](https://img.shields.io/github/actions/workflow/status/prioritizr/prioritizr/R-CMD-check-macos.yaml?branch=master&label=macOS)](https://github.com/prioritizr/prioritizr/actions)
[![Documentation](https://img.shields.io/github/actions/workflow/status/prioritizr/prioritizr/documentation.yaml?branch=master&label=Documentation)](https://github.com/prioritizr/prioritizr/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/prioritizr/prioritizr?label=Coverage)](https://codecov.io/github/prioritizr/prioritizr?branch=master)
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

The latest official version of the *prioritizr R* package can be
installed from the [Comprehensive R Archive Network
(CRAN)](https://cran.r-project.org/) using the following *R* code.

``` r
install.packages("prioritizr", repos = "https://cran.rstudio.com/")
```

Alternatively, the latest development version can be installed from
[GitHub](https://github.com/prioritizr/prioritizr) using the following
code. Please note that while developmental versions may contain
additional features not present in the official version, they may also
contain coding errors.

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("prioritizr/prioritizr")
```

## Citation

Please cite the *prioritizr R* package when using it in publications. To
cite the latest official version, please use:

> Hanson JO, Schuster R, Morrell N, Strimas-Mackey M, Edwards BPM, Watts
> ME, Arcese P, Bennett J, Possingham HP (2022). prioritizr: Systematic
> Conservation Prioritization in R. R package version 7.2.2. Available
> at <https://CRAN.R-project.org/package=prioritizr>.

Alternatively, to cite the latest development version, please use:

> Hanson JO, Schuster R, Morrell N, Strimas-Mackey M, Edwards BPM, Watts
> ME, Arcese P, Bennett J, Possingham HP (2023). prioritizr: Systematic
> Conservation Prioritization in R. R package version 8.0.0.0. Available
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
package. To begin with, we will load the packages.

``` r
# load packages
library(prioritizr)
library(prioritizrdata)
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
    ## min value   :    0.2988945 
    ## max value   : 1845.9683838

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
    ## dimensions  : 109, 147, 400  (nrow, ncol, nlyr)
    ## resolution  : 4000, 4000  (x, y)
    ## extent      : -1816382, -1228382, 247483.5, 683483.5  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs 
    ## source      : wa_features.tif 
    ## names       : Ameri~ding), Ameri~ding), Ameri~ding), Ameri~ding), Ameri~ding), Ameri~full), ...

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

Now, let’s generate a spatial prioritization. Let’s say that we want to
develop a protected area system that will secure 20% of the distribution
for each feature for minimal cost. In this scenario, we can either
purchase all of the land inside a given planning unit, or none of the
land inside a given planning unit. Thus we will create a new `problem()`
that will use a minimum set objective (via `add_min_set_objective()`),
with relative targets of 20% (via `add_relative_targets()`), binary
decisions (via `add_binary_decisions()`), and specify that we want to
want near-optimal solutions (i.e., 10% from optimality) using the best
solver installed on our computer (via `add_default_solver()`).

``` r
# create problem
p1 <-
  problem(wa_pu, features = wa_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.1)
```

``` r
# print the problem
print(p1)
```

    ## A conservation problem (<ConservationProblem>)
    ## ├•data:
    ## │├•features:    "American Avocet (breeding)", "American Bittern (breeding)" , … (400 total)
    ## │└•planning units:
    ## │ ├•data:       <SpatRaster> (10757 total)
    ## │ ├•costs:      continuous values (between 0.2989 and 1845.9684)
    ## │ ├•extent:     -1816381.6182, 247483.5211, -1228381.6182, 683483.5211 (xmin, ymin, xmax, ymax)
    ## │ └•CRS:        PROJCRS["unknown", (unknown)
    ## └•formulation:
    ##  ├•objective:   minimum set objective
    ##  ├•penalties:   none specified
    ##  ├•targets:     relative targets (between 0.2 and 0.2)
    ##  ├•constraints: none specified
    ##  ├•decisions:   binary decision
    ##  ├•portfolio:   shuffle portfolio (`number_solutions` = 1, …)
    ##  └•solver:      gurobi solver (`gap` = 0.1, `time_limit` = 2147483647, `first_feasible` = FALSE, …)
    ## # ℹ Use `summary(...)` to see complete formulation.

After we have built a `problem()`, we can solve it to obtain a solution.

``` r
# solve the problem
s1 <- solve(p1)
```

    ## Gurobi Optimizer version 10.0.0 build v10.0.0rc2 (linux64)
    ## 
    ## CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
    ## Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
    ## 
    ## Optimize a model with 400 rows, 10757 columns and 1524361 nonzeros
    ## Model fingerprint: 0xc260297a
    ## Variable types: 0 continuous, 10757 integer (10757 binary)
    ## Coefficient statistics:
    ##   Matrix range     [1e-03, 1e+02]
    ##   Objective range  [3e-01, 2e+03]
    ##   Bounds range     [1e+00, 1e+00]
    ##   RHS range        [2e+01, 2e+01]
    ## Found heuristic solution: objective 62359.684274
    ## Presolve removed 17 rows and 5 columns (presolve time = 5s) ...
    ## Presolve removed 17 rows and 5 columns
    ## Presolve time: 8.32s
    ## Presolved: 383 rows, 10752 columns, 1523132 nonzeros
    ## Variable types: 0 continuous, 10752 integer (10752 binary)
    ## Found heuristic solution: objective 24763.927629
    ## Root relaxation presolved: 383 rows, 10752 columns, 1523132 nonzeros
    ## 
    ## 
    ## Root simplex log...
    ## 
    ## Iteration    Objective       Primal Inf.    Dual Inf.      Time
    ##        0    2.8348868e+02   5.900880e+05   0.000000e+00      9s
    ##      304    1.3493756e+04   0.000000e+00   0.000000e+00      9s
    ##      304    1.3493756e+04   0.000000e+00   0.000000e+00      9s
    ## 
    ## Root relaxation: objective 1.349376e+04, 304 iterations, 0.31 seconds (0.64 work units)
    ## 
    ##     Nodes    |    Current Node    |     Objective Bounds      |     Work
    ##  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
    ## 
    ##      0     0 13493.7559    0   46 24763.9276 13493.7559  45.5%     -    8s
    ## H    0     0                    15821.627271 13493.7559  14.7%     -    8s
    ## H    0     0                    14441.536489 13493.7559  6.56%     -    8s
    ## 
    ## Explored 1 nodes (304 simplex iterations) in 8.98 seconds (9.14 work units)
    ## Thread count was 1 (of 8 available processors)
    ## 
    ## Solution count 4: 14441.5 15821.6 24763.9 62359.7 
    ## 
    ## Optimal solution found (tolerance 1.00e-01)
    ## Best objective 1.444153648916e+04, best bound 1.349375593467e+04, gap 6.5629%

``` r
# extract the objective
print(attr(s1, "objective"))
```

    ## solution_1 
    ##   14441.54

``` r
# extract time spent solving the problem
print(attr(s1, "runtime"))
```

    ## solution_1 
    ##      9.214

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
and the total cost of the solution. We can also check that all the
representation targets are met by the solution.

``` r
# calculate number of selected planning units by solution
eval_n_summary(p1, s1)
```

    ## # A tibble: 1 × 2
    ##   summary     n
    ##   <chr>   <dbl>
    ## 1 overall  2216

``` r
# calculate total cost of solution
eval_cost_summary(p1, s1)
```

    ## # A tibble: 1 × 2
    ##   summary   cost
    ##   <chr>    <dbl>
    ## 1 overall 14442.

``` r
# calculate target coverage for the solution
p1_target_coverage <- eval_target_coverage_summary(p1, s1)
print(p1_target_coverage)
```

    ## # A tibble: 400 × 9
    ##    feature         met   total…¹ absol…² absol…³ absol…⁴ relat…⁵ relat…⁶ relat…⁷
    ##    <chr>           <lgl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 American Avoce… TRUE    100.     20.0    22.0       0     0.2   0.220       0
    ##  2 American Bitte… TRUE     99.9    20.0    26.8       0     0.2   0.268       0
    ##  3 American Bitte… TRUE    100.     20.0    35.1       0     0.2   0.351       0
    ##  4 American Crow … TRUE     99.9    20.0    20.1       0     0.2   0.201       0
    ##  5 American Crow … TRUE     99.9    20.0    20.0       0     0.2   0.200       0
    ##  6 American Dippe… TRUE    100.     20.0    20.0       0     0.2   0.200       0
    ##  7 American Goldf… TRUE     99.9    20.0    21.1       0     0.2   0.211       0
    ##  8 American Goldf… TRUE     99.9    20.0    22.3       0     0.2   0.224       0
    ##  9 American Kestr… TRUE     99.9    20.0    22.2       0     0.2   0.222       0
    ## 10 American Kestr… TRUE    100.     20.0    22.2       0     0.2   0.222       0
    ## # … with 390 more rows, and abbreviated variable names ¹​total_amount,
    ## #   ²​absolute_target, ³​absolute_held, ⁴​absolute_shortfall, ⁵​relative_target,
    ## #   ⁶​relative_held, ⁷​relative_shortfall

``` r
# check if the solution meets all the targets
print(all(p1_target_coverage$met))
```

    ## [1] TRUE

Although this solution meets all of the representation targets, it does
not account for existing protected areas inside the study area. As such,
it may be inefficient. This is because it might not account for the
possibility that some features could be partially – or even fully –
represented by existing protected areas. To address this issue, we will
use the `get_wa_locked_in()` function to import spatial data for
protected areas in the study area. We will then add constraints to the
`problem()` to ensure they are selected by the solution (via
`add_locked_in_constraints()`).

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
```

    ## Gurobi Optimizer version 10.0.0 build v10.0.0rc2 (linux64)
    ## 
    ## CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
    ## Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
    ## 
    ## Optimize a model with 400 rows, 10757 columns and 1524361 nonzeros
    ## Model fingerprint: 0x1510fcfd
    ## Variable types: 0 continuous, 10757 integer (10757 binary)
    ## Coefficient statistics:
    ##   Matrix range     [1e-03, 1e+02]
    ##   Objective range  [3e-01, 2e+03]
    ##   Bounds range     [1e+00, 1e+00]
    ##   RHS range        [2e+01, 2e+01]
    ## Found heuristic solution: objective 52650.002712
    ## Presolve removed 171 rows and 3937 columns
    ## Presolve time: 1.45s
    ## Presolved: 229 rows, 6820 columns, 540180 nonzeros
    ## Variable types: 0 continuous, 6820 integer (6820 binary)
    ## Found heuristic solution: objective 38666.194816
    ## Root relaxation presolved: 229 rows, 6820 columns, 540180 nonzeros
    ## 
    ## 
    ## Root relaxation: objective 3.061712e+04, 107 iterations, 0.08 seconds (0.18 work units)
    ## 
    ##     Nodes    |    Current Node    |     Objective Bounds      |     Work
    ##  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
    ## 
    ##      0     0 30617.1194    0   23 38666.1948 30617.1194  20.8%     -    1s
    ## H    0     0                    31972.957002 30617.1194  4.24%     -    1s
    ## 
    ## Explored 1 nodes (107 simplex iterations) in 1.61 seconds (2.74 work units)
    ## Thread count was 1 (of 8 available processors)
    ## 
    ## Solution count 3: 31973 38666.2 52650 
    ## 
    ## Optimal solution found (tolerance 1.00e-01)
    ## Best objective 3.197295700160e+04, best bound 3.061711939500e+04, gap 4.2406%

``` r
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
# import locked in data
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
# create new problem with locked in constraints added to it
p3 <-
  p2 %>%
  add_locked_in_constraints(wa_locked_out)

# solve the problem
s3 <- solve(p3)
```

    ## Gurobi Optimizer version 10.0.0 build v10.0.0rc2 (linux64)
    ## 
    ## CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
    ## Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
    ## 
    ## Optimize a model with 400 rows, 10757 columns and 1524361 nonzeros
    ## Model fingerprint: 0xef95833c
    ## Variable types: 0 continuous, 10757 integer (10757 binary)
    ## Coefficient statistics:
    ##   Matrix range     [1e-03, 1e+02]
    ##   Objective range  [3e-01, 2e+03]
    ##   Bounds range     [1e+00, 1e+00]
    ##   RHS range        [2e+01, 2e+01]
    ## Found heuristic solution: objective 120313.46438
    ## 
    ## Explored 0 nodes (0 simplex iterations) in 0.04 seconds (0.04 work units)
    ## Thread count was 1 (of 8 available processors)
    ## 
    ## Solution count 1: 120313 
    ## 
    ## Optimal solution found (tolerance 1.00e-01)
    ## Best objective 1.203134643751e+05, best bound 1.172794714357e+05, gap 2.5217%

``` r
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
`add_boundary_penalties()`). Here we will use a penalty factor of 0.003
(i.e., boundary length modifier), and an edge factor of 50% so that
planning units that occur outer edge of the study area are not overly
penalized.

``` r
# create new problem with boundary penalties added to it
p4 <-
  p3 %>%
  add_boundary_penalties(penalty = 0.003, edge_factor = 0.5)

# solve the problem
s4 <- solve(p4)
```

    ## Gurobi Optimizer version 10.0.0 build v10.0.0rc2 (linux64)
    ## 
    ## CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
    ## Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
    ## 
    ## Optimize a model with 42572 rows, 31843 columns and 1608705 nonzeros
    ## Model fingerprint: 0x91181e1e
    ## Variable types: 0 continuous, 31843 integer (31843 binary)
    ## Coefficient statistics:
    ##   Matrix range     [1e-03, 1e+02]
    ##   Objective range  [2e+01, 2e+03]
    ##   Bounds range     [1e+00, 1e+00]
    ##   RHS range        [2e+01, 2e+01]
    ## Found heuristic solution: objective 182167.38001
    ## Found heuristic solution: objective 169909.46438
    ## Presolve removed 21651 rows and 15464 columns
    ## Presolve time: 2.02s
    ## Presolved: 20921 rows, 16379 columns, 189666 nonzeros
    ## Found heuristic solution: objective 158124.25349
    ## Variable types: 0 continuous, 16379 integer (16379 binary)
    ## Found heuristic solution: objective 157859.67459
    ## Root relaxation presolve removed 6 rows and 4 columns
    ## Root relaxation presolved: 20915 rows, 16375 columns, 184657 nonzeros
    ## 
    ## 
    ## Root relaxation: objective 1.466646e+05, 4591 iterations, 0.27 seconds (0.63 work units)
    ## 
    ##     Nodes    |    Current Node    |     Objective Bounds      |     Work
    ##  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
    ## 
    ##      0     0 146664.650    0   44 157859.675 146664.650  7.09%     -    2s
    ## 
    ## Explored 1 nodes (4591 simplex iterations) in 2.38 seconds (5.70 work units)
    ## Thread count was 1 (of 8 available processors)
    ## 
    ## Solution count 4: 157860 158124 169909 182167 
    ## 
    ## Optimal solution found (tolerance 1.00e-01)
    ## Best objective 1.578596745860e+05, best bound 1.466646495302e+05, gap 7.0918%

``` r
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
    ## dimensions  : 109, 147, 401  (nrow, ncol, nlyr)
    ## resolution  : 4000, 4000  (x, y)
    ## extent      : -1816382, -1228382, 247483.5, 683483.5  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs 
    ## source(s)   : memory
    ## varnames    : wa_pu 
    ##               wa_pu 
    ##               wa_pu 
    ##               ...
    ## names       :  Ameri~ding), Ameri~ding), Ameri~ding),  Ameri~ding),  Ameri~ding), Ameri~full), ... 
    ## min values  : 0.0000000000,   0.0000000, 0.000000000, 0.0000000000, 0.0000000000, 0.000000000, ... 
    ## max values  : 0.0006324382,   0.0010287, 0.001647519, 0.0003089722, 0.0004855097, 0.000163235, ...

``` r
# plot the total importance scores
## note that gray cells are not selected by the prioritization
plot(
  rc[["total"]], main = "Importance scores",
  breaks = c(0, 1e-10, 1, 3, 6), axes = FALSE,
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
materials are available too (e.g., the [Massey University 2021
workshop](https://prioritizr.github.io/massey-workshop/) and the
[PacMara 2019
workshop](https://prioritizr.github.io/PacMara_workshop/)).

## Getting help

If you have any questions about the *prioritizr R* package or
suggestions for improving it, please [post an issue on the code
repository](https://github.com/prioritizr/prioritizr/issues/new).
