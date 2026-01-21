# Add a *CPLEX* solver

Specify that the [*IBM
CPLEX*](https://www.ibm.com/products/ilog-cplex-optimization-studio/cplex-optimizer)
software should be used to solve a conservation planning problem (IBM
2017) . This function can also be used to customize the behavior of the
solver. It requires the cplexAPI package to be installed (see below for
installation instructions).

## Usage

``` r
add_cplex_solver(
  x,
  gap = 0.1,
  time_limit = .Machine$integer.max,
  presolve = TRUE,
  threads = 1,
  verbose = TRUE
)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- gap:

  `numeric` gap to optimality. This gap is relative and expresses the
  acceptable deviance from the optimal objective. For example, a value
  of 0.01 will result in the solver stopping when it has found a
  solution within 1% of optimality. Additionally, a value of 0 will
  result in the solver stopping when it has found an optimal solution.
  The default value is 0.1 (i.e., 10% from optimality).

- time_limit:

  `numeric` time limit (seconds) for generating solutions. The solver
  will return the current best solution when this time limit is
  exceeded. The default value is the largest integer value (i.e.,
  `.Machine$integer.max`), effectively meaning that solver will keep
  running until a solution within the optimality gap is found.

- presolve:

  `logical` attempt to simplify the problem before solving it? Defaults
  to `TRUE`.

- threads:

  `integer` number of threads to use for the optimization algorithm. The
  default value is 1.

- verbose:

  `logical` should information be printed while solving optimization
  problems? Defaults to `TRUE`.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the solver added to it.

## Details

[*IBM
CPLEX*](https://www.ibm.com/products/ilog-cplex-optimization-studio/cplex-optimizer)
is a commercial optimization software. It is faster than the available
open source solvers (e.g.,
[`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
and
[`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md).
Although formal benchmarks examining the performance of this solver for
conservation planning problems have yet to be completed, preliminary
analyses suggest that it performs slightly slower than the *Gurobi*
solver (i.e.,
[`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)).
We recommend using this solver if the *Gurobi* solver is not available.
Licenses are available for the *IBM CPLEX* software to academics at no
cost (see \<
https://www.ibm.com/products/ilog-cplex-optimization-studio/cplex-optimizer\>).

## Installation

The cplexAPI package is used to interface with *IBM CPLEX* software. To
install the package, the *IBM CPLEX* software must be installed (see
<https://www.ibm.com/products/ilog-cplex-optimization-studio/cplex-optimizer>).
Next, the `CPLEX_BIN` environmental variable must be set to specify the
file path for the *IBM CPLEX* software. For example, on a Linux system,
this variable can be specified by adding the following text to the
`~/.bashrc` file:

      export CPLEX_BIN="/opt/ibm/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/cplex"

Please Note that you may need to change the version number in the file
path (i.e., `"CPLEX_Studio128"`). After specifying the `CPLEX_BIN`
environmental variable, the cplexAPI package can be installed. Since the
cplexAPI package is not available on the the Comprehensive R Archive
Network (CRAN), it must be installed from [its GitHub
repository](https://github.com/cran/cplexAPI). To install the cplexAPI
package, please use the following code:

    if (!require(remotes)) install.packages("remotes")
    remotes::install_github("cran/cplexAPI")

For further details on installing this package, please consult the
[installation
instructions](https://github.com/cran/cplexAPI/blob/master/inst/INSTALL).

## References

IBM (2017) IBM ILOG CPLEX Optimization Studio CPLEX User's Manual.
Version 12 Release 8. IBM ILOG CPLEX Division, Incline Village, NV.

## See also

Other functions for adding solvers:
[`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md),
[`add_default_solver()`](https://prioritizr.net/reference/add_default_solver.md),
[`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md),
[`add_highs_solver()`](https://prioritizr.net/reference/add_highs_solver.md),
[`add_lsymphony_solver`](https://prioritizr.net/reference/add_lsymphony_solver.md),
[`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)

## Examples

``` r
# \dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create problem
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_cplex_solver(gap = 0.1, time_limit = 5, verbose = FALSE)

# generate solution
s <- solve(p)

# plot solution
plot(s, main = "solution", axes = FALSE)

# }
```
