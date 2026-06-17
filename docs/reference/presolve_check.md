# Presolve check

Check a conservation planning problem for potential issues before trying
to solve it. Specifically, problems are checked for (i) values that are
likely to result in "strange" solutions and (ii) values that are likely
to cause numerical instability issues and lead to unreasonably long run
times when solving it. Although these checks are provided to help
diagnose potential issues, please be aware that some detected issues may
be false positives. Please note that these checks will not be able to
verify if a problem has a feasible solution or not.

## Usage

``` r
presolve_check(x, warn = TRUE)

# S3 method for class 'ConservationProblem'
presolve_check(x, warn = TRUE)

# S3 method for class 'OptimizationProblem'
presolve_check(x, warn = TRUE)

# S3 method for class 'MultiConservationProblem'
presolve_check(x, warn = TRUE)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) or
  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  object.

- warn:

  `logical` value indicating if a warning should be thrown if the check
  fails? Defaults to `TRUE`.

## Value

A `logical` value indicating if all checks passed successfully.

## Details

This function checks for issues that are likely to result in "strange"
or "unrealistic" solutions. Specifically, it checks if (i) all planning
units are locked in, (ii) all planning units are locked out, and (iii)
all planning units have negative cost values (after applying penalties
if any were specified). Although such conservation planning problems are
mathematically valid, they are generally the result of a coding mistake
when building the problem (e.g., using an absurdly high penalty value or
using the wrong dataset to lock in planning units). Thus such issues, if
they are indeed issues and not false positives, can be fixed by
carefully checking the code, data, and parameters used to build the
conservation planning problem.

This function also checks for values that may lead to numerical
instability issues during optimization. Specifically, it checks if the
range of values in certain components of the optimization problem are
over a certain threshold (i.e., \\1 \times 10^9\\) or if the values
themselves exceed a certain threshold (i.e., \\1 \times 10^{10}\\). In
most cases, such issues will simply cause an exact algorithm solver to
take a very long time to generate a solution. In rare cases, such issues
can cause incorrect calculations which can lead to exact algorithm
solvers returning infeasible solutions (e.g., a solution to the minimum
set problem where not all targets are met) or solutions that exceed the
specified optimality gap (e.g., a suboptimal solution when a zero
optimality gap is specified).

What can you do if a conservation planning problem fails to pass these
checks? Well, this function will have thrown some warning messages
describing the source of the issue(s), so read them carefully. For
instance, a common issue is when a relatively large penalty value is
specified for boundary
([`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md))
or connectivity penalties
([`add_connectivity_penalties()`](https://prioritizr.net/reference/add_connectivity_penalties.md)).
This can be fixed by trying a smaller penalty value. In such cases, the
original penalty value supplied was so high that the optimal solution
would just have selected every single planning unit in the solution—and
this may not be especially helpful anyway (see below for example).
Another common issue is that the planning unit cost values are too
large. For example, if you express the costs of the planning units in
terms of USD then you might have some planning units that cost over one
billion dollars in large-scale planning exercises. This can be fixed by
rescaling the values so that they are smaller (e.g., multiplying the
values by a number smaller than one, or expressing them as a fraction of
the maximum cost). Let's consider another common issue, let's pretend
that you used habitat suitability models to predict the amount of
suitable habitat in each planning unit for each feature. If you
calculated the amount of suitable habitat in each planning unit in
square meters then this could lead to very large numbers. You could fix
this by converting the units from square meters to square kilometers or
thousands of square kilometers. Alternatively, you could calculate the
percentage of each planning unit that is occupied by suitable habitat,
which will yield values between 0 and 100.

But what can you do if you can't fix these issues by simply changing the
penalty values or rescaling data? You will need to apply some creative
thinking. Let's run through a couple of scenarios. Let's pretend that
you have a few planning units that cost a billion times more than any
other planning unit so you can't fix this by rescaling the cost values.
In this case, it's extremely unlikely that these planning units will be
selected in the optimal solution so just set the costs to zero and lock
them out. If this procedure yields a problem with no feasible solution,
because one (or several) of the planning units that you manually locked
out contains critical habitat for a feature, then find out which
planning unit(s) is causing this infeasibility and set its cost to zero.
After solving the problem, you will need to manually recalculate the
cost of the solutions but at least now you can be confident that you
have the optimal solution. Now let's pretend that you are using the
maximum number of targets met objective (i.e.,
[`add_max_n_targets_met_objective()`](https://prioritizr.net/reference/add_max_n_targets_met_objective.md))
and assigned some really high weights to the targets for some features
to ensure that their targets were met in the optimal solution. If you
set the weights for these features to one billion then you will probably
run into numerical instability issues. Instead, you can calculate
minimum weight needed to guarantee that these features will be
represented in the optimal solution and use this value instead of one
billion. This minimum weight value can be calculated as the sum of the
weight values for the other features and adding a small number to it
(e.g., 1). Finally, if you're running out of ideas for addressing
numerical stability issues, then you have one remaining option: you can
use the `numeric_focus` parameter of the
[`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
function to tell the solver to pay extra attention to numerical
instability issues. Although this option can help address numerical
instability issues, it often results in longer run times because the
solver will now dedicate additional computational processing to
addressing these issues. So, if you have problems that already take a
very long time time to solve, then might not be of much help.

## See also

See the Gurobi documentation for more information on numerical
instability issues
(<https://docs.gurobi.com/projects/optimizer/en/current/concepts/numericguide.html>).

## Examples

``` r
# set seed for reproducibility
set.seed(500)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create minimal problem with no issues
p1 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions()

# run presolve checks
# note that no warning is thrown which suggests that we should not
# encounter any numerical stability issues when trying to solve the problem
print(presolve_check(p1))
#> [1] TRUE

# create a minimal problem, containing cost values that are really
# high so that they could cause numerical instability issues when trying
# to solve it
sim_pu_raster2 <- sim_pu_raster
sim_pu_raster2[1] <- 1e+15
p2 <-
  problem(sim_pu_raster2, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions()

# run presolve checks
# note that a warning is thrown which suggests that we might encounter
# some issues, such as long solve time or suboptimal solutions, when
# trying to solve the problem
print(presolve_check(p2))
#> Warning: 
#> ── Presolve checks
#> ─────────────────────────────────────────────────────────────
#> 
#> ── Numerical issues ──
#> 
#> ℹ The following issues could stall optimization or produce incorrect solutions:
#> 
#> ✖ Planning units must not have cost values that are too high (> 1e6).
#> → Try re-scaling cost values (e.g., convert units from USD to millions of USD).
#> 
#> ── Results ──
#> 
#> ✖ Failed.
#> For more information, see `presolve_check()`.
#> [1] FALSE

# create a minimal problem with connectivity penalties values that have
# a really high penalty value that is likely to cause numerical instability
# issues when trying to solve the it
cm <- adjacency_matrix(sim_pu_raster)
p3 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_connectivity_penalties(1e+15, data = cm) %>%
  add_binary_decisions()

# run presolve checks
# note that a warning is thrown which suggests that we might encounter
# some numerical instability issues when trying to solve the problem
print(presolve_check(p3))
#> Warning: 
#> ── Presolve checks
#> ─────────────────────────────────────────────────────────────
#> 
#> ── Numerical issues ──
#> 
#> ℹ The following issues could stall optimization or produce incorrect solutions:
#> 
#> ✖ Multiplying the connectivity data by `penalty` must not produce values that
#>   are too high (> 1e6).
#> → Try using a smaller `penalty` in `add_connectivity_penalties()`.
#> 
#> ── Results ──
#> 
#> ✖ Failed.
#> For more information, see `presolve_check()`.
#> [1] FALSE

# let's forcibly solve the problem using Gurobi and tell it to
# be extra careful about numerical instability problems
s3 <-
   p3 %>%
   add_gurobi_solver(numeric_focus = TRUE) %>%
   solve(force = TRUE)
#> 
#> ── Presolve checks ─────────────────────────────────────────────────────────────
#> 
#> ── Numerical issues ──
#> 
#> ℹ The following issues could stall optimization or produce incorrect solutions:
#> 
#> ✖ Multiplying the connectivity data by `penalty` must not produce values that are too high (> 1e6).
#> → Try using a smaller `penalty` in `add_connectivity_penalties()`.
#> 
#> ── Results ──
#> 
#> ✖ Failed.
#> For more information, see `presolve_check()`.
#> Warning: `a` failed presolve check.
#> 
#> ── Optimization ────────────────────────────────────────────────────────────────
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.1
#> Set parameter NumericFocus to value 2
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Warning: Gurobi version mismatch between R 13.0.1 and C library 13.0.2
#> Gurobi Optimizer version 13.0.2 build v13.0.2rc1 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.1
#> NumericFocus  2
#> Presolve  2
#> Threads  1
#> 
#> Optimize a model with 293 rows, 234 columns and 1026 nonzeros (Min)
#> Model fingerprint: 0xecb065ff
#> Model has 234 linear objective coefficients
#> Variable types: 144 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-01, 1e+00]
#>   Objective range  [2e+02, 1e+15]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [3e+00, 8e+00]
#> Warning: Model contains large objective coefficients
#> 
#> Found heuristic solution: objective -1.44000e+17
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: -1.44e+17 
#> No other solutions better than -1.44e+17
#> 
#> Optimal solution found (tolerance 1.00e-01)
#> Best objective -1.440000000000e+17, best bound -1.440000000000e+17, gap 0.0000%

# plot solution
# we can see that all planning units were selected because the connectivity
# penalty is so high that cost becomes irrelevant, so we should try using
# a much lower penalty value
plot(s3, main = "solution", axes = FALSE)
```
