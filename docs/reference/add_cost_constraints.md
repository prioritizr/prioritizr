# Add cost constraints

Add constraints to a conservation planning problem to ensure that the
cost of selected planning units meets certain criteria.

## Usage

``` r
add_cost_constraints(x, threshold, sense)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- threshold:

  `numeric` value. This threshold value is also known as a
  "right-hand-side value" per integer programming terminology.

- sense:

  `character` value denoting the sense for the constraint. Acceptable
  values are: `">="`, `"<="`, or `"="`.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the constraints added to it.

## Details

This function adds constraints constraints that can be used to ensure
that the cost of solutions meet certain criteria (see Examples section
below for details). For example, these constraints can be used to
specify both minimum and maximum budget thresholds. Note that this
function provided as a convenient alternative for adding linear
constraints (per
[`add_linear_constraints()`](https://prioritizr.net/reference/add_linear_constraints.md))
to a [`problem()`](https://prioritizr.net/reference/problem.md).

## Mathematical formulation

The linear constraints are implemented using the following equation. Let
\\I\\ denote the set of planning units (indexed by \\i\\), \\Z\\ the set
of management zones (indexed by \\z\\), and \\X\_{iz}\\ the decision
variable for allocating planning unit \\i\\ to zone \\z\\ (e.g., with
binary values indicating if each planning unit is allocated or not).
Also, let \\D\_{iz}\\ denote the costs associated with planning units
\\i \in I\\ for zones \\z \in Z\\ (per `data`, if supplied as a `matrix`
object), \\\theta\\ denote the constraint sense (per `sense`), and \\t\\
denote the constraint threshold (per `threshold`).

\$\$ \sum\_{i}^{I} \sum\_{z}^{Z} (D\_{iz} \times X\_{iz}) \space \theta
\space t \$\$

## See also

Other functions for adding constraints:
[`add_contiguity_constraints()`](https://prioritizr.net/reference/add_contiguity_constraints.md),
[`add_feature_contiguity_constraints()`](https://prioritizr.net/reference/add_feature_contiguity_constraints.md),
[`add_linear_constraints()`](https://prioritizr.net/reference/add_linear_constraints.md),
[`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md),
[`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md),
[`add_mandatory_allocation_constraints()`](https://prioritizr.net/reference/add_mandatory_allocation_constraints.md),
[`add_manual_bounded_constraints()`](https://prioritizr.net/reference/add_manual_bounded_constraints.md),
[`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md),
[`add_neighbor_constraints()`](https://prioritizr.net/reference/add_neighbor_constraints.md)

## Examples

``` r
# \dontrun{
# set seed for reproducibility
set.seed(600)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create layer with 1s for all planning units
sim_ones_raster <- (sim_pu_raster * 0) + 1

# calculate budget based on 30% of the number of planning units
budget <- 0.3 * terra::global(sim_ones_raster, "sum", na.rm = TRUE)[[1]]

# here we will formulate a multi-objective optimization problem
# that (i) minimizes targets shortfalls for feature representation,
# (ii) minimizes the cost of the solution, and (iii)
# ensures that (approximately) 30% of the study area is selected
# (i.e., by specifying a budget for the upper threshold and a linear
# constraint for the lower threshold on the number of selected planning
# units). to explore trade-offs between the objectives,
# we will use the reference point approach to generate 10 different solutions
mp <-
  multi_problem(
    obj1 =
      problem(sim_ones_raster, sim_features) %>%
      add_min_shortfall_objective(budget = budget) %>%
      add_relative_targets(0.4) %>%
      add_cost_constraints(sense = ">=", threshold = budget * 0.95) %>%
      add_binary_decisions(),
    obj2 =
      problem(sim_pu_raster, sim_pu_raster) %>%
      add_min_penalties_objective() %>%
      # note a value of 1 is here because only the costs minimized
      add_cost_penalties(1) %>%
      add_binary_decisions()
  ) %>%
  add_ref_point_approach(
    weights = approach_weights_matrix(n_problems = 2, n_values = 10)
  ) %>%
  add_default_solver(gap = 0.01)

# generate solutions
ms <- solve(mp)
#> 
#> ── Optimization ────────────────────────────────────────────────────────────────
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> 
#> Optimize a model with 8 rows, 95 columns and 636 nonzeros (Min)
#> Model fingerprint: 0xb5135a04
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-01, 3e+01]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [1e+00, 3e+01]
#> 
#> Found heuristic solution: objective 1.7589414
#> Presolve removed 1 rows and 0 columns
#> Presolve time: 0.00s
#> Presolved: 7 rows, 95 columns, 635 nonzeros
#> Variable types: 5 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 6 rows, 96 columns, 546 nonzeros
#> 
#> 
#> Root relaxation: objective 1.468163e+00, 19 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       1.4681633    1.46816  0.00%     -    0s
#> 
#> Explored 1 nodes (19 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 1.46816 1.75894 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 1.468163338508e+00, best bound 1.468163338508e+00, gap 0.0000%
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> 
#> Optimize a model with 8 rows, 95 columns and 636 nonzeros (Min)
#> Model fingerprint: 0x8a6777f9
#> Model has 90 linear objective coefficients
#> Variable types: 5 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-01, 3e+01]
#>   Objective range  [2e+02, 2e+02]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [1e+00, 3e+01]
#> 
#> Found heuristic solution: objective 5350.9034882
#> Presolve removed 8 rows and 95 columns
#> Presolve time: 0.00s
#> Presolve: All rows and columns removed
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 5132.13 5350.9 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.132129470825e+03, best bound 5.132129470825e+03, gap 0.0000%
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> 
#> Optimize a model with 8 rows, 95 columns and 636 nonzeros (Max)
#> Model fingerprint: 0x473eb6b4
#> Model has 5 linear objective coefficients
#> Variable types: 5 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-01, 3e+01]
#>   Objective range  [1e+00, 1e+00]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [1e+00, 3e+01]
#> 
#> Found heuristic solution: objective 5.0000000
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 5 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.000000000000e+00, best bound 5.000000000000e+00, gap 0.0000%
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> 
#> Optimize a model with 8 rows, 95 columns and 636 nonzeros (Max)
#> Model fingerprint: 0x74b96a45
#> Model has 90 linear objective coefficients
#> Variable types: 5 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-01, 3e+01]
#>   Objective range  [2e+02, 2e+02]
#>   Bounds range     [1e+00, 1e+00]
#>   RHS range        [1e+00, 3e+01]
#> 
#> Found heuristic solution: objective 5566.7673340
#> Presolve removed 8 rows and 95 columns
#> Presolve time: 0.00s
#> Presolve: All rows and columns removed
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 5727 5566.77 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.727002456665e+03, best bound 5.727002456665e+03, gap 0.0000%
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x09a2bed4
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-03, 2e+02]
#>   Objective range  [2e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Found heuristic solution: objective 0.0000000
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 2
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 98 columns, 643 nonzeros
#> 
#> 
#> Root relaxation: objective 7.308018e-02, 23 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0730802    0.07308  0.00%     -    0s
#> 
#> Explored 1 nodes (23 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0730802 2 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 7.308017605902e-02, best bound 7.308017605902e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 736 nonzeros (Min)
#> Model fingerprint: 0x1848c070
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-01, 2e+02]
#>   Objective range  [3e-01, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 4 rows and 2 columns
#> Presolve time: 0.00s
#> Presolved: 8 rows, 96 columns, 641 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0728911
#> 
#> Presolve time: 0.00s
#> Presolved: 8 rows, 96 columns, 641 nonzeros
#> Variable types: 6 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 7 rows, 97 columns, 552 nonzeros
#> 
#> 
#> Root relaxation: objective 0.000000e+00, 21 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0000000    0.00000  0.00%     -    0s
#> 
#> Explored 1 nodes (21 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0 0.0728911 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 736 nonzeros (Min)
#> Model fingerprint: 0x369eacc1
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-03, 2e+02]
#>   Objective range  [2e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 9 rows and 7 columns
#> Presolve time: 0.00s
#> Presolved: 3 rows, 91 columns, 271 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.00018907
#> 
#> Presolve time: 0.00s
#> Presolved: 3 rows, 91 columns, 271 nonzeros
#> Variable types: 1 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 2 rows, 91 columns, 181 nonzeros
#> 
#> 
#> Root relaxation: objective 0.000000e+00, 10 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0000000    0.00000  0.00%     -    0s
#> 
#> Explored 1 nodes (10 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0 0.00018907 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Generating solutions ■■                                 4% | ETA: 30s
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xea39cf05
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-04, 2e+02]
#>   Objective range  [2e-04, 1e+00]
#>   Bounds range     [2e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.222222
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 98 columns, 643 nonzeros
#> 
#> 
#> Root relaxation: objective 1.615859e-02, 22 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0161586    0.01616  0.00%     -    0s
#> 
#> Explored 1 nodes (22 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0161586 0.222222 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 1.615859426043e-02, best bound 1.615859426043e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xc7e1b9e5
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-04, 2e+02]
#>   Objective range  [2e-04, 1e+00]
#>   Bounds range     [3e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0240665
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 18 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.02407    0.02402  0.17%     -    0s
#> 
#> Explored 1 nodes (18 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0240665 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 2.406648477963e-02, best bound 2.402469855534e-02, gap 0.1736%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xdc653dee
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-04, 2e+02]
#>   Objective range  [2e-04, 1e+00]
#>   Bounds range     [4e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0319744
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 3.165162e-02, 24 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0316516    0.03165  0.00%     -    0s
#> 
#> Explored 1 nodes (24 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0316516 0.0319744 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 3.165161510904e-02, best bound 3.165161510904e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x2809ad9c
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-04, 2e+02]
#>   Objective range  [2e-04, 1e+00]
#>   Bounds range     [6e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0388593
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 98 columns, 643 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 21 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.03886    0.03886  0.00%     -    0s
#> 
#> Explored 1 nodes (21 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0388593 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 3.885926624775e-02, best bound 3.885926624775e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x53de072d
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-04, 2e+02]
#>   Objective range  [2e-04, 1e+00]
#>   Bounds range     [7e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0460669
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 98 columns, 643 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 20 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.04607    0.04607  0.00%     -    0s
#> 
#> Explored 1 nodes (20 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0460669 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.606691738646e-02, best bound 4.606691738646e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xdc33ed77
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-04, 2e+02]
#>   Objective range  [2e-04, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0532746
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 98 columns, 643 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 20 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.05327    0.05299  0.53%     -    0s
#> 
#> Explored 1 nodes (20 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0532746 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.327456852518e-02, best bound 5.299339428507e-02, gap 0.5278%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x17594b33
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-04, 2e+02]
#>   Objective range  [2e-04, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0604822
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 98 columns, 643 nonzeros
#> 
#> 
#> Root relaxation: objective 5.940966e-02, 21 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0594097    0.05941  0.00%     -    0s
#> 
#> Explored 1 nodes (21 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0594097 0.0604822 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.940966361619e-02, best bound 5.940966361619e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Generating solutions ■■■■■                             13% | ETA: 27s
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xf253405a
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-04, 2e+02]
#>   Objective range  [2e-04, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.111111
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 98 columns, 643 nonzeros
#> 
#> 
#> Root relaxation: objective 6.542348e-02, 21 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0654235    0.06542  0.00%     -    0s
#> 
#> Explored 1 nodes (21 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0654235 0.111111 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 6.542348370835e-02, best bound 6.542348370835e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x68de52f1
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [4e-04, 2e+02]
#>   Objective range  [4e-04, 1e+00]
#>   Bounds range     [2e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.00859352
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 8.141027e-03, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0081410    0.00814  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.00814103 0.00859352 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 8.141027320979e-03, best bound 8.141027320979e-03, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x21c00dfa
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [4e-04, 2e+02]
#>   Objective range  [4e-04, 1e+00]
#>   Bounds range     [3e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.04 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0244093
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 18 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.02441    0.02434  0.29%     -    0s
#> 
#> Explored 1 nodes (18 simplex iterations) in 0.04 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0244093 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 2.440929800166e-02, best bound 2.433905092747e-02, gap 0.2878%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.04 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x3dc5d05a
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [4e-04, 2e+02]
#>   Objective range  [4e-04, 1e+00]
#>   Bounds range     [4e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0344726
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 3.231719e-02, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0323172    0.03232  0.00%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0323172 0.0344726 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 3.231718852086e-02, best bound 3.231718852086e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x66eedbd6
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [4e-04, 2e+02]
#>   Objective range  [4e-04, 1e+00]
#>   Bounds range     [6e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0416803
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 4.022091e-02, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0402209    0.04022  0.00%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0402209 0.0416803 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.022091345728e-02, best bound 4.022091345728e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x26db5ca0
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [4e-04, 2e+02]
#>   Objective range  [4e-04, 1e+00]
#>   Bounds range     [7e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0488879
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 4.801761e-02, 22 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0480176    0.04802  0.00%     -    0s
#> 
#> Explored 1 nodes (22 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0480176 0.0488879 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.801761496001e-02, best bound 4.801761496001e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xb6b724a1
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [4e-04, 2e+02]
#>   Objective range  [4e-04, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0560956
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 22 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.05610    0.05568  0.75%     -    0s
#> 
#> Explored 1 nodes (22 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0560956 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.609557907938e-02, best bound 5.567726920553e-02, gap 0.7457%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Generating solutions ■■■■■■■■                          23% | ETA: 24s
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xf2d6d051
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [4e-04, 2e+02]
#>   Objective range  [4e-04, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.069996
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 6.330323e-02, 23 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0633032    0.06330  0.00%     -    0s
#> 
#> Explored 1 nodes (23 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0633032 0.069996 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 6.330323021809e-02, best bound 6.330323021809e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x9e3e6ed4
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [4e-04, 2e+02]
#>   Objective range  [4e-04, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.077679
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 98 columns, 643 nonzeros
#> 
#> 
#> Root relaxation: objective 7.051088e-02, 20 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0705109    0.07051  0.00%     -    0s
#> 
#> Explored 1 nodes (20 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0705109 0.077679 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 7.051088135680e-02, best bound 7.051088135680e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x57a2d0c7
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [6e-04, 2e+02]
#>   Objective range  [6e-04, 1e+00]
#>   Bounds range     [3e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.00816204
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.00816    0.00816  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.00816204 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 8.162035079845e-03, best bound 8.162035079845e-03, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x8d8da6fc
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [6e-04, 2e+02]
#>   Objective range  [6e-04, 1e+00]
#>   Bounds range     [3e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0168442
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 1.626105e-02, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0162610    0.01626  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.016261 0.0168442 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 1.626104688309e-02, best bound 1.626104688309e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x896ed8ba
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [6e-04, 2e+02]
#>   Objective range  [6e-04, 1e+00]
#>   Bounds range     [4e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.03266
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 26 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.03266    0.03246  0.62%     -    0s
#> 
#> Explored 1 nodes (26 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.03266 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 3.266000174290e-02, best bound 3.245907048959e-02, gap 0.6152%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xf217215e
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [6e-04, 2e+02]
#>   Objective range  [6e-04, 1e+00]
#>   Bounds range     [6e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0407602
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 26 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.04076    0.04056  0.50%     -    0s
#> 
#> Explored 1 nodes (26 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0407602 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.076016105244e-02, best bound 4.055808229284e-02, gap 0.4958%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xc5bed6e8
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [6e-04, 2e+02]
#>   Objective range  [6e-04, 1e+00]
#>   Bounds range     [7e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0490475
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 4.847578e-02, 24 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0484758    0.04848  0.00%     -    0s
#> 
#> Explored 1 nodes (24 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0484758 0.0490475 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.847578278129e-02, best bound 4.847578278129e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Generating solutions ■■■■■■■■■■■                       32% | ETA: 21s
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xacd32e1e
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [6e-04, 2e+02]
#>   Objective range  [6e-04, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0589166
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 5.638367e-02, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0563837    0.05638  0.00%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0563837 0.0589166 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.638367330049e-02, best bound 5.638367330049e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x9630eb42
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [6e-04, 2e+02]
#>   Objective range  [6e-04, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0661242
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 6.424561e-02, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0642456    0.06425  0.00%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0642456 0.0661242 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 6.424561201262e-02, best bound 6.424561201262e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xb9b59093
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [6e-04, 2e+02]
#>   Objective range  [6e-04, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0733319
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 7.202642e-02, 22 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0720264    0.07203  0.00%     -    0s
#> 
#> Explored 1 nodes (22 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0720264 0.0733319 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 7.202642244001e-02, best bound 7.202642244001e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x4ef601db
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [7e-04, 2e+02]
#>   Objective range  [7e-04, 1e+00]
#>   Bounds range     [4e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.00818304
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.00818    0.00818  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.00818304 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 8.183042838710e-03, best bound 8.183042838710e-03, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xdf1dddf6
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [7e-04, 2e+02]
#>   Objective range  [7e-04, 1e+00]
#>   Bounds range     [4e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0162821
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.01628    0.01628  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0162821 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 1.628205464196e-02, best bound 1.628205464196e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xae92bdb0
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [7e-04, 2e+02]
#>   Objective range  [7e-04, 1e+00]
#>   Bounds range     [4e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0250949
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 2.438107e-02, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0243811    0.02438  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0243811 0.0250949 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 2.438106644521e-02, best bound 2.438106644521e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x7b36203a
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [7e-04, 2e+02]
#>   Objective range  [7e-04, 1e+00]
#>   Bounds range     [6e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0412994
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 4.057909e-02, 26 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0405791    0.04058  0.00%     -    0s
#> 
#> Explored 1 nodes (26 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0405791 0.0412994 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.057909005170e-02, best bound 4.057909005170e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xb6d97a42
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [7e-04, 2e+02]
#>   Objective range  [7e-04, 1e+00]
#>   Bounds range     [7e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0488186
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.04882    0.04868  0.29%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0488186 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.881859600333e-02, best bound 4.867810185495e-02, gap 0.2878%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Generating solutions ■■■■■■■■■■■■■■                    43% | ETA: 17s
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x082dd611
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [7e-04, 2e+02]
#>   Objective range  [7e-04, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0567265
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.05673    0.05673  0.00%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0567265 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.672648652252e-02, best bound 5.672648652252e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xdcf83eab
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [7e-04, 2e+02]
#>   Objective range  [7e-04, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0647849
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 24 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.06478    0.06463  0.23%     -    0s
#> 
#> Explored 1 nodes (24 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0647849 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 6.478485960778e-02, best bound 6.463437704172e-02, gap 0.2323%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x435d906c
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [7e-04, 2e+02]
#>   Objective range  [7e-04, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0730563
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 24 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.07306    0.07254  0.70%     -    0s
#> 
#> Explored 1 nodes (24 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0730563 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 7.305626718347e-02, best bound 7.254226756091e-02, gap 0.7036%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x1aa7ecd8
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-04, 2e+02]
#>   Objective range  [9e-04, 1e+00]
#>   Bounds range     [6e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.00820405
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.00820    0.00819  0.16%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.00820405 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 8.204050597576e-03, best bound 8.190695351708e-03, gap 0.1628%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xf61ffc02
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-04, 2e+02]
#>   Objective range  [9e-04, 1e+00]
#>   Bounds range     [6e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0163031
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.01630    0.01630  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0163031 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 1.630306240082e-02, best bound 1.630306240082e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x1534afb9
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-04, 2e+02]
#>   Objective range  [9e-04, 1e+00]
#>   Bounds range     [6e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0244021
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.02440    0.02440  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0244021 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 2.440207420407e-02, best bound 2.440207420407e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x8a3074c3
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-04, 2e+02]
#>   Objective range  [9e-04, 1e+00]
#>   Bounds range     [6e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.04 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0333456
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 3.250109e-02, 28 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0325011    0.03250  0.00%     -    0s
#> 
#> Explored 1 nodes (28 simplex iterations) in 0.04 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0325011 0.0333456 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 3.250108600732e-02, best bound 3.250108600732e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.04 seconds (0.00 work units), solution count 2
#> 
#> Generating solutions ■■■■■■■■■■■■■■■■■                 52% | ETA: 15s
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xa4a394df
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-04, 2e+02]
#>   Objective range  [9e-04, 1e+00]
#>   Bounds range     [7e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0491614
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.04916    0.04870  0.94%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0491614 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.916140922536e-02, best bound 4.869910961381e-02, gap 0.9404%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x2842dfd9
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-04, 2e+02]
#>   Objective range  [9e-04, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0570693
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.05707    0.05680  0.48%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0570693 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.706929974456e-02, best bound 5.679812141706e-02, gap 0.4752%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x434cecb5
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-04, 2e+02]
#>   Objective range  [9e-04, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0653241
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.06532    0.06490  0.65%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0653241 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 6.532410720294e-02, best bound 6.489713322031e-02, gap 0.6536%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x95cd3a9c
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [9e-04, 2e+02]
#>   Objective range  [9e-04, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0740861
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 7.288508e-02, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0728851    0.07289  0.00%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0728851 0.0740861 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 7.288508078295e-02, best bound 7.288508078295e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xb437f0eb
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [7e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.00822506
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.00823    0.00819  0.42%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.00822506 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 8.225058356441e-03, best bound 8.190695351708e-03, gap 0.4178%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x4560618e
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [7e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0163241
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.01632    0.01632  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0163241 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 1.632407015969e-02, best bound 1.632407015969e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x0175084c
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [7e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0244231
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.02442    0.02442  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0244231 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 2.442308196294e-02, best bound 2.442308196294e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xf10ae4e0
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [7e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0325221
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 28 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.03252    0.03252  0.00%     -    0s
#> 
#> Explored 1 nodes (28 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0325221 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 3.252209376618e-02, best bound 3.252209376618e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Generating solutions ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 11s
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xaed517bb
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [7e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0406211
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 28 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.04062    0.04062  0.00%     -    0s
#> 
#> Explored 1 nodes (28 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0406211 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.062110556943e-02, best bound 4.062110556943e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x65c544fd
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0574121
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 5.681913e-02, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0568191    0.05682  0.00%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0568191 0.0574121 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.681912917593e-02, best bound 5.681912917593e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x1c43513b
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0658634
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 6.491814e-02, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0649181    0.06492  0.00%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0649181 0.0658634 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 6.491814097918e-02, best bound 6.491814097918e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xf079e400
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0732279
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.07323    0.07302  0.29%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0732279 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 7.322789400499e-02, best bound 7.301715278242e-02, gap 0.2878%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x9effa30c
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.00824607
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.00825    0.00819  0.67%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.00824607 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 8.246066115307e-03, best bound 8.190695351708e-03, gap 0.6715%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x35b7e97f
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0163451
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.01635    0.01635  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0163451 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 1.634507791855e-02, best bound 1.634507791855e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xad0f70b6
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0244441
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.02444    0.02444  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0244441 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 2.444408972180e-02, best bound 2.444408972180e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Generating solutions ■■■■■■■■■■■■■■■■■■■■■■■           72% | ETA:  8s
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x457aea0e
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0325431
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 28 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.03254    0.03254  0.00%     -    0s
#> 
#> Explored 1 nodes (28 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0325431 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 3.254310152505e-02, best bound 3.254310152505e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xf02be918
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0406421
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 28 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.04064    0.04064  0.00%     -    0s
#> 
#> Explored 1 nodes (28 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0406421 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.064211332830e-02, best bound 4.064211332830e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xc9ccca00
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [8e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.049847
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 4.874113e-02, 27 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0487411    0.04874  0.00%     -    0s
#> 
#> Explored 1 nodes (27 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0487411 0.049847 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.874112513155e-02, best bound 4.874112513155e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x729a9401
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0649391
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.06494    0.06494  0.00%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0649391 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 6.493914873804e-02, best bound 6.493914873804e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xf5fffb79
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0735707
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.07357    0.07304  0.72%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0735707 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 7.357070722702e-02, best bound 7.303816054129e-02, gap 0.7239%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x2b01ca21
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.00826707
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: interrupted, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0          -    0         0.00827    0.00819  0.92%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.00826707 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 8.267073874173e-03, best bound 8.190695351708e-03, gap 0.9239%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xe5fed5b7
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0163661
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.01637    0.01637  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0163661 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 1.636608567742e-02, best bound 1.636608567742e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xbe281ba2
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0244651
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.02447    0.02447  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0244651 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 2.446509748067e-02, best bound 2.446509748067e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Generating solutions ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  5s
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x2bbfacce
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0325641
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 28 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.03256    0.03256  0.00%     -    0s
#> 
#> Explored 1 nodes (28 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0325641 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 3.256410928392e-02, best bound 3.256410928392e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x2c6d7e5c
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0406631
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 28 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.04066    0.04066  0.00%     -    0s
#> 
#> Explored 1 nodes (28 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0406631 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.066312108716e-02, best bound 4.066312108716e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xe77124f1
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0487621
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 27 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.04876    0.04876  0.00%     -    0s
#> 
#> Explored 1 nodes (27 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0487621 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.876213289041e-02, best bound 4.876213289041e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x14f8ee9e
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [9e-01, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0568611
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 27 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.05686    0.05686  0.00%     -    0s
#> 
#> Explored 1 nodes (27 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0568611 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.686114469366e-02, best bound 5.686114469366e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x60355da3
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [1e-03, 2e+02]
#>   Objective range  [1e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0730592
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 25 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.07306    0.07306  0.00%     -    0s
#> 
#> Explored 1 nodes (25 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0730592 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 7.305916830016e-02, best bound 7.305916830016e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x1c2b65b7
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-03, 2e+02]
#>   Objective range  [2e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.00828808
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: objective 8.190695e-03, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#> *    0     0               0       0.0081907    0.00819  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 2: 0.0081907 0.00828808 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 8.190695351708e-03, best bound 8.190695351708e-03, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 2
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x125c7b10
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-03, 2e+02]
#>   Objective range  [2e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0163871
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.01639    0.01639  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0163871 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 1.638709343629e-02, best bound 1.638709343629e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Generating solutions ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  2s
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x54040085
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-03, 2e+02]
#>   Objective range  [2e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0244861
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 17 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.02449    0.02449  0.00%     -    0s
#> 
#> Explored 1 nodes (17 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0244861 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 2.448610523953e-02, best bound 2.448610523953e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xcea79c5e
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-03, 2e+02]
#>   Objective range  [2e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0325851
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 28 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.03259    0.03259  0.00%     -    0s
#> 
#> Explored 1 nodes (28 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0325851 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 3.258511704278e-02, best bound 3.258511704278e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0x8e345fa3
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-03, 2e+02]
#>   Objective range  [2e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0406841
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 28 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.04068    0.04068  0.00%     -    0s
#> 
#> Explored 1 nodes (28 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0406841 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.068412884603e-02, best bound 4.068412884603e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xf318c4e6
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-03, 2e+02]
#>   Objective range  [2e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0487831
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 27 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.04878    0.04878  0.00%     -    0s
#> 
#> Explored 1 nodes (27 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0487831 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 4.878314064928e-02, best bound 4.878314064928e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xd111fc7d
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-03, 2e+02]
#>   Objective range  [2e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0568822
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 27 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.05688    0.05688  0.00%     -    0s
#> 
#> Explored 1 nodes (27 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0568822 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 5.688215245253e-02, best bound 5.688215245253e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Set parameter TimeLimit to value 2147483647
#> Set parameter MIPGap to value 0.01
#> Set parameter Presolve to value 2
#> Set parameter Threads to value 1
#> Set parameter MultiObjPre to value 2
#> Academic license - for non-commercial use only - expires 2027-04-14
#> Gurobi Optimizer version 13.0.1 build v13.0.1rc0 (linux64 - "Ubuntu 24.04.2 LTS")
#> 
#> CPU model: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, instruction set [SSE2|AVX|AVX2|AVX512]
#> Thread count: 4 physical cores, 8 logical processors, using up to 1 threads
#> 
#> Non-default parameters:
#> TimeLimit  2147483647
#> MIPGap  0.01
#> Presolve  2
#> Threads  1
#> MultiObjPre  2
#> 
#> Optimize a model with 12 rows, 98 columns and 737 nonzeros (Min)
#> Model fingerprint: 0xc7d08624
#> Model has 1 linear objective coefficients
#> Variable types: 8 continuous, 90 integer (90 binary)
#> Coefficient statistics:
#>   Matrix range     [2e-03, 2e+02]
#>   Objective range  [2e-03, 1e+00]
#>   Bounds range     [1e+00, 6e+02]
#>   RHS range        [1e+00, 5e+03]
#> 
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: starting optimization (min) with 2 objectives... 
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: applying initial presolve...
#> ---------------------------------------------------------------------------
#> 
#> Presolve removed 3 rows and 1 columns
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 1 (objective_1) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0
#> 
#> 
#> Explored 0 nodes (0 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 0.000000000000e+00, best bound 0.000000000000e+00, gap 0.0000%
#> ---------------------------------------------------------------------------
#> 
#> Multi-objectives: optimize objective 2 (objective_2) ...
#> ---------------------------------------------------------------------------
#> 
#> Loaded user MIP start with objective 0.0649812
#> 
#> Presolve time: 0.00s
#> Presolved: 9 rows, 97 columns, 732 nonzeros
#> Variable types: 7 continuous, 90 integer (90 binary)
#> Root relaxation presolve removed 1 rows and 0 columns
#> Root relaxation presolved: 8 rows, 97 columns, 642 nonzeros
#> 
#> 
#> Root relaxation: cutoff, 27 iterations, 0.00 seconds (0.00 work units)
#> 
#>     Nodes    |    Current Node    |     Objective Bounds      |     Work
#>  Expl Unexpl |  Obj  Depth IntInf | Incumbent    BestBd   Gap | It/Node Time
#> 
#>      0     0     cutoff    0         0.06498    0.06498  0.00%     -    0s
#> 
#> Explored 1 nodes (27 simplex iterations) in 0.00 seconds (0.00 work units)
#> Thread count was 1 (of 8 available processors)
#> 
#> Solution count 1: 0.0649812 
#> 
#> Optimal solution found (tolerance 1.00e-02)
#> Best objective 6.498116425577e-02, best bound 6.498116425577e-02, gap 0.0000%
#> 
#> ---------------------------------------------------------------------------
#> Multi-objectives: solved in 0.00 seconds (0.00 work units), solution count 1
#> 
#> Generating solutions ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s

# plot the solutions
plot(terra::rast(ms), axes = FALSE)


# extract objective values for the solutions
obj_matrix <- attributes(ms)$objective

# preview the objective values
head(obj_matrix)
#>                obj1     obj2
#> solution_1 1.725603 5132.242
#> solution_2 1.468163 5598.032
#> solution_3 5.000000 5132.129
#> solution_4 1.719528 5133.965
#> solution_5 1.719528 5133.965
#> solution_6 1.697270 5147.233

# plot the objectives values to visualize trade-offs
# (note that smaller values are better for both objectives)
plot(
  obj_matrix,
  main = "Trade-offs between objectives",
  xlab = "Species representation (shortfall)",
  ylab = "Solution cost"
)

# }
```
