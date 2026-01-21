# Add solvers

Specify the software and settings used to solve a conservation planning
problem. By default, the best available software currently installed on
the system will be used. For information on the performance of different
solvers, please see Hanson *et al.* (2025) and Schuster *et al.* (2020)
for benchmarks comparing run time and solution quality of these solvers.

## Details

The following functions can be used to add a solver to a conservation
planning [`problem()`](https://prioritizr.net/reference/problem.md).
Note that if multiple of these functions are added to a
[`problem()`](https://prioritizr.net/reference/problem.md), then only
the last function added will be used.

- [`add_default_solver()`](https://prioritizr.net/reference/add_default_solver.md):

  This solver uses the best software currently installed on the system.

- [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md):

  [*Gurobi*](https://www.gurobi.com/) is a state-of-the-art commercial
  optimization software with an R package interface. We recommend using
  this solver if at all possible. It is by far the fastest of the
  solvers available for generating prioritizations, however, it is not
  freely available. That said, licenses are available to academics at no
  cost. The gurobi package is distributed with the *Gurobi* software
  suite. This solver uses the gurobi package to solve problems.

- [`add_cplex_solver()`](https://prioritizr.net/reference/add_cplex_solver.md):

  [*IBM
  CPLEX*](https://www.ibm.com/products/ilog-cplex-optimization-studio/cplex-optimizer)
  is a commercial optimization software. It is faster than the open
  source solvers available for generating prioritizations, however, it
  is not freely available. Similar to the
  [*Gurobi*](https://www.gurobi.com/) software, licenses are available
  to academics at no cost. This solver uses the cplexAPI package to
  solve problems using *IBM CPLEX*.

- [`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md):

  [*CBC*](https://github.com/coin-or/Cbc) is an open-source mixed
  integer programming solver that is part of the Computational
  Infrastructure for Operations Research (COIN-OR) project. Preliminary
  benchmarks indicate that it is the fastest open source solver
  currently supported. We recommend using this solver if both *Gurobi*
  and *IBM CPLEX* are unavailable. It requires the rcbc package, which
  is currently only available on
  [GitHub](https://github.com/dirkschumacher/rcbc).

- [`add_highs_solver()`](https://prioritizr.net/reference/add_highs_solver.md):

  [*HiGHS*](https://highs.dev/) is an open source optimization software.
  Although this solver can have comparable performance to the *CBC*
  solver for particular problems and is generally faster than the
  *SYMPHONY* based solvers (see below), it sometimes can take much
  longer than the *CBC* solver for particular problems.

- [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md):

  [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) is an open-source
  mixed integer programming solver that is also part of the COIN-OR
  project. Although both *SYMPHONY* and *CBC* are part of the COIN-OR
  project, they are different software. The lpsymphony package provides
  an interface to the *SYMPHONY* software, and is distributed through
  [Bioconductor](https://doi.org/doi:10.18129/B9.bioc.lpsymphony). We
  recommend using this solver if the *CBC* and *HiGHS* solvers cannot be
  installed. This solver can use parallel processing to solve problems,
  so it is faster than Rsymphony package interface (see below).

- [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md):

  This solver provides an alternative interface to the
  [*SYMPHONY*](https://github.com/coin-or/SYMPHONY) solver using the
  Rsymphony package. It is not recommended to use this solver because it
  has the slowest performance.

## References

Hanson JO, Schuster R, Strimas-Mackey M, Morrell N, Edwards BPM, Arcese
P, Bennett JR, and Possingham HP (2025). Systematic conservation
prioritization with the prioritizr R package. *Conservation Biology*,
39: e14376.

Schuster R, Hanson JO, Strimas-Mackey M, and Bennett JR (2020). Exact
integer linear programming solvers outperform simulated annealing for
solving conservation planning problems. *PeerJ*, 8: e9258.

## See also

Other overviews:
[`constraints`](https://prioritizr.net/reference/constraints.md),
[`decisions`](https://prioritizr.net/reference/decisions.md),
[`importance`](https://prioritizr.net/reference/importance.md),
[`objectives`](https://prioritizr.net/reference/objectives.md),
[`penalties`](https://prioritizr.net/reference/penalties.md),
[`portfolios`](https://prioritizr.net/reference/portfolios.md),
[`summaries`](https://prioritizr.net/reference/summaries.md),
[`targets`](https://prioritizr.net/reference/targets.md)

## Examples

``` r
# \dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create basic problem
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_proportion_decisions()

# create vector to store plot names
n <- c()

# create empty list to store solutions
s <- c()

# if gurobi is installed: create problem with added gurobi solver
if (require("gurobi")) {
  p1 <- p %>% add_gurobi_solver(verbose = FALSE)
  n <- c(n, "gurobi")
  s <- c(s, solve(p1))
}
#> Loading required package: gurobi
#> Loading required package: slam

# if cplexAPI is installed: create problem with added CPLEX solver
if (require("cplexAPI")) {
  p2 <- p %>% add_cplex_solver(verbose = FALSE)
  n <- c(n, "CPLEX")
  s <- c(s, solve(p2))
}
#> Loading required package: cplexAPI

# if rcbc is installed: create problem with added CBC solver
if (require("rcbc")) {
  p3 <- p %>% add_cbc_solver(verbose = FALSE)
  n <- c(n, "CBC")
  s <- c(s, solve(p3))
}
#> Loading required package: rcbc

# if highs is installed: create problem with added HiGHs solver
if (require("highs")) {
  p4 <- p %>% add_highs_solver(verbose = FALSE)
  n <- c(n, "HiGHS")
  s <- c(s, solve(p4))
}
#> Loading required package: highs

# create problem with added rsymphony solver
if (require("Rsymphony")) {
  p5 <- p %>% add_rsymphony_solver(verbose = FALSE)
  n <- c(n, "Rsymphony")
  s <- c(s, solve(p5))
}
#> Loading required package: Rsymphony

# if lpsymphony is installed: create problem with added lpsymphony solver
if (require("lpsymphony")) {
  p6 <- p %>% add_lpsymphony_solver(verbose = FALSE)
  n <- c(n, "lpsymphony")
  s <- c(s, solve(p6))
}
#> Loading required package: lpsymphony

# plot solutions
names(s) <- n
plot(terra::rast(s), axes = FALSE)

# }
```
