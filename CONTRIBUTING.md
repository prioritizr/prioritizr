
<!--- CONTRIBUTING.md is generated from CONTRIBUTING.Rmd. Please edit that file -->
Contributing to development of the *prioritizr R* package
=========================================================

We welcome contributions to the *prioritizr R* package. These contributions could be simple typo fixes, sentence rewrites to improve clarity, enhancing the vignettes to provide a greater understanding this package, or completely new functions to customize conservation planning problems. **No contribution is too small.**

The aim of this document is to provide a comprehensive guide for making contributions to the *prioritizr R* package. It also serves to document the internal organization and rationale behind the design of this package. Please note that it is beyond the scope of this document to teach *R* programming or package development practices. To learn more about these topics, we strongly recommend reading [*R for Data Science*](http://r4ds.had.co.nz/), [*Advanced R*](http://adv-r.had.co.nz/), and [*R Packages*](http://r-pkgs.had.co.nz/). Additionally, a substantial proportion of the code in this package is written in the *C++* programming language using the *Rcpp R* package. Therefore, individuals wishing to contribute new functionality (e.g. a function to add a new set of constraints to a conservation planning problem) may need to learn *C++* programming and the *Rcpp R* package. To learn more about *C++* programming and/or the *Rcpp R* package, we recommend reading [*Rcpp for everyone*](https://teuder.github.io/rcpp4everyone_en/). All of these are books are free, online, and absolutely fantastic resources.

If you have any questions about contributing to *prioritizr* that are not covered in this document, please [post an issue on the GitHub repository](https://github.com/prioritizr/prioritizr/issues/new).

### Style guide

This package uses American spelling for all English words. All code should use snake-case (i.e. under-scores and lower-case spelling; e.g. `add_relative_targets`), except for class names which use camel-case (i.e. no under-scores and upper-case letters; e.g. `ConservationProblem`). The `<-` operator should be used for object assignment. Spaces should surround all infix operators (e.g. `+`, `-`, etc.) and should be placed after commas. All indenting should use two-spaces. All lines should not exceed 80 characters in length, except for text in vignettes (and @aliases *roxygen2* tags because they must be on a single line). All code should escape character objects using double quotes (i.e. `"` symbols). Where possible, functions from other packages should not be imported and instead should be referred to using the `::` function (e.g. `Matrix::sparseMatrix` to access the `sparseMatrix` function from the *Matrix* package). All functions should use the [*assertthat* package](https://cran.r-project.org/web/packages/assertthat/index.html) to validate arguments. This package uses the [*testthat* package](https://cran.r-project.org/web/packages/testthat/index.html) for unit testing. All functions should have at least one unit test that verifies that it works and ideally several tests that verify that invalidate arguments return an error. All private functions that are not exported via the NAMESPACE should be located in the *R/internal.R* file. Although this package exports the pipe operator (`%>%`) for ease of use, for clarity, this operator should not be used anywhere in this package's source code (excepting documentation and unit tests).

This package uses the [*roxygen2* package](https://cran.r-project.org/web/packages/testthat/index.html) for generating documentation. Thus help files should be generated using *roxygen2* comments. Software and *R* package names should be written in *italics* in *markdown* documents and enclosed in `\pkg` tags in *Latex* files (e.g. ). When referring to parameter names in the documentation, they should be encapsulated by `\code` tags (e.g. `\code{x}` in help for *problem*). When referring to the names of columns in a data.frame object, please use the format `\code{"column_name"}`, where `column_name` is the name of the column (e.g. `\code{"mpg"}` to refer to a column in the `mtcars` object). All help files for functions should contain the following sections at a minimum: Description, Value, and Examples.

### Prototype class

The main class used to represent objects in *prioritizr* is the `pproto` class (defined in *R/pproto.R*). This class is inspired by the `ggproto` system used in the [*ggplot2* package](https://cran.r-project.org/web/packages/testthat/index.html) and uses environments to manage data (based on the `proto` class defined in the [*proto* package](https://cran.r-project.org/web/packages/proto/index.html)). It has several desirable properties: `pproto` objects can be modified in place, specific elements in a `pproto` object can be modified without *R* needing to copy all of the data stored in the `pproto` object (unlike `list` and *S3* objects in *R*), a `pproto` object can be copied without physically needing to copy all of raw data stored within it (specifically, when we copy a `pproto` object *R* just copies pointers to the data in the original `pproto` object). These properties are especially desirable when working with large data sets. However, they come at a cost. Notably, these "features" may be unnecessary for small data sets and so `pproto` objects may incur unnecessary overheads that mean manipulating them may take slightly longer than otherwise expected. Additionally, the raw syntax for working with `pproto` objects is object-oriented---though perhaps this may be argued as a feature and not a cost---and so some extra experience may be required to understand how to manipulate them.

### Conservation problems

The `ConservationProblem` class (defined in *R/ConservationProblem-proto.R*) is used to represent conservation planning objects. They are created by the `problem` function and should contain all the data and parameters needed to generate a mathematical formulation of the problem. They should also contain all the information needed to instantiate an interactive web application to manipulate them (using the [*shiny* package](https://cran.r-project.org/web/packages/shiny/index.html), though the functionality to achieve this is still being developed).

### Conservation modifiers

The `ConservationModifier` class (defined in *R/ConservationModifier-proto.R*) is used to instantiate objects that systematically alter the mathematical formulation of a conservation planning problem (i.e. a `ConservationProblem` object). All of the functions used for specifying objectives, constraints, penalties, decisions, solvers, and portfolios involve creating a new `ConservationModifier` object and appending it to a `ConservationProblem` object. To help standardize the behavior of different `ConservationModifier` objects, we have created specializations of the `ConservationModifier` class: `Objective` (defined in *R/Objective-proto.R*), `Constraint` (defined in *R/Constraint-proto.R*), `Penalty` (defined in *R/Penalty-proto.R*), `Decision` (defined in *R/Decision-proto.R*), `Solver` (defined in *R/Solver-proto.R*), and `Portfolio` (defined in *R/Portfolio-proto.R*). Every single `ConservationModifier` object contains an `apply` method that is used to modify an `OptimizationProblem` object to "apply" the mathematical formulation described by the object.

### Optimization problems

The `OptimizationProblem` class (defined in *R/OptimizationProblem-proto.R*) represents the precise mathematical formulation of a conservation planning problem. When a `ConservationProblem` object is "solved" (using the `solve` function), the `ConservationProblem` object is first "compiled" into an `OptimizationProblem` object (using the `compile` function) and the `OptimizationProblem` object is then solved (using the `solve` function). This distinction is important because if a contributor wants to add a new objective, constraint, penalty, or decision function to *prioritizr* they will need to write code that adds the mathematical formulation of the function to a `OptimizationProblem` object. Alas, this is not entirely trivial.

All `OptimizationProblem` objects are simply a wrapper to an external pointer pointer (`XPtr`) that points to a C++ `OPTIMIZATIONPROBLEM` class object (defined in *src/optimization\_problem.h*). We decided to implement the `OptimizationProblem` class in this manner because (1) storing objects as external pointers reduces memory consumption and (2) the process of adding in constraints requires iteration and loops which are slow in *R*. We elected not to use the [*ompr* package](https://cran.r-project.org/web/packages/testthat/index.html) because (1) benchmarks at the time indicated that this package would take too long to compile problems with many constraints and (2) it uses the R Optimization Infrastructure (ROI) which as yet solve problems using *Gurobi* (discounting repositories on GitHub which do not appear to be actively maintained). As a consequence, *Rcpp* code is needed to modify `OptimizationProblem` objects and users will need to write *Rcpp* functions to add new objectives, constraints, penalties to *prioritizr*.

The C++ `OPTIMIZATIONPROBLEM` class contains the standard data needed to formulate a mixed integer linear programming problem (i.e. model sense, problem objective , constraint matrix, constraint senses, right-hand-side values, variable decision types, and variable bounds). This class also contains additional data that pertain to conservation problems (i.e. number of features, planning units, and zones). If a potential contributor is unfamiliar with the standard representation of a mixed integer linear programming problem, we encourage them to read the documentation for the [*gurobi R* package](http://www.gurobi.com/documentation/8.0/refman/r_api_overview.html#r:problem). The fields *A\_i*, *A\_j*, and *A\_x* correspond the row, column, and cells values for the constraint matrix of the optimization problem (respectively). The other fields follow standard conventions. Note that the constraint matrix is, ultimately, constructed as a `Matrix::sparseMatrix` so row and column indices do not need to be sequential. Additionally, all *Rcpp* constraint and penalty functions should be independent. In other words, though it may be computationally efficient to reuse constraints and variables encoded in other functions, each *Rcpp* constraint and penalty function define its own constraints and variables. All conservation planning problems are defined following one of two standard mathematical formulations: the compressed and the expanded formulation.

The compressed formulation defines a problem which assumes that all instances of a conserved feature are used to achieve the target or count towards the benefit of a solution. Although the expanded formulation can provide identical solutions to the compressed formulation, the compressed formulation is provided because it is simpler and can be used with much larger sized problems. Currently, all constraints use the compressed formulation except for the `add_feature_contiguity_constraints` function. Under this formulation, the first set of decision variables (the first number of planning units × number of zones) always pertain to the state of the planning units. Thus in a problem containing 3 planning units and 2 zones, the first six variables indicate the allocation of: planning unit 1 in zone 1, planning unit 2 in zone 1, planning unit 3 in zone 1, planning unit 1 in zone 2, planning unit 2 in zone 2, planning unit 3 in zone 2. The first set of constraints (rows) always correspond to each target (noting that some objectives use "fake" targets to initialize the feature by planning unit data, referred to as *rij* data; see *R/compile.R*). These rows, which each correspond to a single target, contain the amount of each feature in each zone for which the target pertains. Thus rows for targets which pertain to a single zone will only contain feature abundance data for planning units (columns) in a single zone, and rows for targets which pertain to a single feature in multiple zones will contain feature abundance data for planning units (columns) in multiple zones.

To help illustrate the compressed formulation, consider the following problem:

``` r
# simulate data
pu <- data.frame(id = 1:3, cost_1 = 4:6, cost_2 = 7:9)
zone <- data.frame(id = 1:2, name = c("z1", "z2"))
feature <- data.frame(id = 1:2, name = c("f1", "f2"))
rij <- expand.grid(pu = 1:3, species = 1:2, zone = 1:2)
rij$amount <- seq_len(nrow(rij)) + nrow(rij)
targets <- matrix(1, nrow = 2, ncol = 2)

# create problem
p <- problem(pu, feature, rij, c("cost_1", "cost_2"), zone) %>%
     add_absolute_targets(targets)

# print problem
print(p)
```

    ## Conservation Problem
    ##   zones:          z1, z2 (2 zones)
    ##   planning units: data.frame (3 units)
    ##   cost:           min: 4, max: 9
    ##   features:       f1, f2 (2 features)
    ##   objective:      none
    ##   targets:        Absolute targets [targets (min: 1, max: 1)]
    ##   decisions:      default
    ##   constraints:    <none>
    ##   penalties:      <none>
    ##   portfolio:      default
    ##   solver:         default

The compressed formulation expresses the planning unit and feature data using the following constraint matrix. Here, each variable (column) corresponds to a different planning unit and a different zone allocation, each constraint (row) corresponds to a different target, and each cell corresponds to the amount of a each feature in each planning unit given a different zone (based on the *rij* data and the targets).

    ## 4 x 6 sparse Matrix of class "dgCMatrix"
    ##              pu1_z1 pu2_z1 pu3_z1 pu1_z2 pu2_z2 pu3_z2
    ## target_f1_z1     13     14     15      .      .      .
    ## target_f2_z1     16     17     18      .      .      .
    ## target_f1_z2      .      .      .     19     20     21
    ## target_f2_z2      .      .      .     22     23     24

The expanded formulation, on the other hand, defines a problem which can allow for some instances of conserved features to not be used for achieving the targets or maximizing the conservation benefit. This formulation is a generalized version of the compressed formulation. It contains additional variables (columns) and constraints (rows) for each combination of feature, planning unit, and zone that indicate if a given planning unit allocated to a specific zone is also allocated to conserve a given feature.

Given the previous problem, the expanded formulation expresses the planning unit and feature data in the constraint matrix as:

``` r
# generate targets
targets2 <- p$targets$output()

# create matrix
m <- matrix(NA, ncol = (p$number_of_zones() * p$number_of_planning_units()) +
                       (p$number_of_zones() * p$number_of_planning_units() *
                        p$number_of_features()),
            nrow = (p$number_of_zones() * p$number_of_planning_units() *
                    p$number_of_features()) +
                    (p$number_of_features() * p$number_of_zones()))

# add row names
rownames(m) <- c(paste0("pu", rep(seq_len(p$number_of_planning_units()),
                                            p$number_of_zones() *
                                            p$number_of_features()),
                          "_", rep(rep(p$feature_names(),
                                       each = p$number_of_planning_units()),
                                   p$number_of_zones()),
                          "_", rep(p$zone_names(),
                                   each = p$number_of_planning_units() *
                                          p$number_of_features())),
                 paste0("target_", rep(p$feature_names(), p$number_of_zones()),
                        "_", rep(p$zone_names(),
                                 each = p$number_of_features())))

# add column names
colnames(m) <- c(paste0("pu", rep(seq_len(p$number_of_planning_units()),
                                          p$number_of_zones()),
                        "_", rep(p$zone_names(),
                                 each = p$number_of_planning_units())),
                 paste0("pu", rep(seq_len(p$number_of_planning_units()),
                                          p$number_of_zones() *
                                          p$number_of_features()),
                        "_", rep(rep(p$feature_names(),
                                     each = p$number_of_planning_units()),
                                 p$number_of_zones()),
                        "_", rep(p$zone_names(),
                                 each = p$number_of_planning_units() *
                                        p$number_of_features())))

# add in indicator variables and constraints
curr_row <- 0
for (z in seq_len(p$number_of_zones())) {
  for (i in seq_len(p$number_of_features())) {
    for (j in seq_len(p$number_of_planning_units())) {
      curr_row <- curr_row + 1
      curr_col1 <- ((z - 1) * p$number_of_planning_units()) + j
      curr_col2 <- (p$number_of_planning_units() * p$number_of_zones()) +
                   ((z - 1) * p$number_of_features() *
                              p$number_of_planning_units()) +
                   ((i - 1) * p$number_of_planning_units()) + j
      m[curr_row, curr_col1] <- -1
      m[curr_row, curr_col2] <- 1
    }
  }
}

# add in targets
for (i in seq_len(nrow(targets2))) {
  # extract indices
  curr_row <- curr_row + 1
  curr_feature <- targets2$feature[i]
  curr_zone <- targets2$zone[i][[1]]
  curr_cols <- (p$number_of_planning_units() * p$number_of_zones()) +
               ((curr_zone - 1) * p$number_of_features() *
                                 p$number_of_planning_units()) +
               ((curr_feature - 1) * p$number_of_planning_units()) +
               seq_len(p$number_of_planning_units())
  curr_amount <- rij$amount[unlist(rij$zone) == curr_zone &
                            rij$species == curr_feature]
  # set matrix values
  m[curr_row, curr_cols] <- curr_amount
}

# convert to sparse matrix
m[is.na(m)] <- 0
m <- as(m, "sparseMatrix")

# print matrix
print(m)
```

    ## 16 x 18 sparse Matrix of class "dgCMatrix"

    ##    [[ suppressing 18 column names 'pu1_z1', 'pu2_z1', 'pu3_z1' ... ]]

    ##                                                                   
    ## pu1_f1_z1    -1  .  .  .  .  .  1  .  .  .  .  .  .  .  .  .  .  .
    ## pu2_f1_z1     . -1  .  .  .  .  .  1  .  .  .  .  .  .  .  .  .  .
    ## pu3_f1_z1     .  . -1  .  .  .  .  .  1  .  .  .  .  .  .  .  .  .
    ## pu1_f2_z1    -1  .  .  .  .  .  .  .  .  1  .  .  .  .  .  .  .  .
    ## pu2_f2_z1     . -1  .  .  .  .  .  .  .  .  1  .  .  .  .  .  .  .
    ## pu3_f2_z1     .  . -1  .  .  .  .  .  .  .  .  1  .  .  .  .  .  .
    ## pu1_f1_z2     .  .  . -1  .  .  .  .  .  .  .  .  1  .  .  .  .  .
    ## pu2_f1_z2     .  .  .  . -1  .  .  .  .  .  .  .  .  1  .  .  .  .
    ## pu3_f1_z2     .  .  .  .  . -1  .  .  .  .  .  .  .  .  1  .  .  .
    ## pu1_f2_z2     .  .  . -1  .  .  .  .  .  .  .  .  .  .  .  1  .  .
    ## pu2_f2_z2     .  .  .  . -1  .  .  .  .  .  .  .  .  .  .  .  1  .
    ## pu3_f2_z2     .  .  .  .  . -1  .  .  .  .  .  .  .  .  .  .  .  1
    ## target_f1_z1  .  .  .  .  .  . 13 14 15  .  .  .  .  .  .  .  .  .
    ## target_f2_z1  .  .  .  .  .  .  .  .  . 16 17 18  .  .  .  .  .  .
    ## target_f1_z2  .  .  .  .  .  .  .  .  .  .  .  . 19 20 21  .  .  .
    ## target_f2_z2  .  .  .  .  .  .  .  .  .  .  .  .  .  .  . 22 23 24
