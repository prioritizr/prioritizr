# Write problem

Save the mathematical formulation for a conservation planning problem to
a file for mixed integer programming solvers. Note that this function
requires either the Rsymphony or gurobi package to be installed.

## Usage

``` r
write_problem(x, path, solver = NULL)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- path:

  `character` file path to save the problem formulation. In particular,
  `path` should have a `".lp"` or `.mps"` file extension to specify
  whether the problem formulation should be saved in the
  [LP](https://docs.gurobi.com/current/#refman/lp_format.html) or
  [MPS](https://docs.gurobi.com/current/#refman/mps_format.html) format
  (respectively). If using the Gurobi solver (i.e.,
  `solver = "gurobi"`), the problem can be saved in a compressed file
  format (e.g., `".mps.gx"`) to reduce file size (see
  `?gurobi::gurobi_write()` for details).

- solver:

  `character` name of optimization solver to write the problem to disk.
  Available options are: `"rsymphony" `, `"gurobi"`, or `NULL`. Note
  that using the Gurobi solver is much faster, because the Rsymphony
  solver requires attempting to solve the problem before it can be
  written. Defaults to `NULL`, such that the best available solver is
  used.

## Value

An invisible `TRUE` indicating success.

## Examples

``` r
# set seed for reproducibility
set.seed(500)

# load data
sim_pu_polygons <- get_sim_pu_polygons()
sim_features <- get_sim_features()

# subset data to extract first four planning units
sim_pu_polygons <- sim_pu_polygons[1:4, ]

# create minimal problem
p <-
  problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions()

# specify file path to save problem formulation
path <- file.path(tempdir(), "model.lp")
print(path)
#> [1] "/tmp/RtmpZRtJhY/model.lp"

# save problem to file
## note that either the gurobi or Rsymphony package needs to be installed
write_problem(p, path)
#> Set parameter Username
#> Set parameter LicenseID to value 2806834
#> Academic license - for non-commercial use only - expires 2027-04-14

# print model file
cat(readLines(path), sep = "\n")
#> \ Model R
#> \ LP format - for model browsing. Use MPS format to capture full model detail.
#> \ Signature: 0x54d8652510f6dfed
#> Minimize
#>   215.8638399028077 C0 + 212.7823480801063 C1 + 207.4962437102063 C2
#>    + 208.9321699486367 C3
#> Subject To
#>  R0: 0.7150548100471497 C0 + 0.6990428566932678 C1 + 0.6859316825866699 C2
#>    + 0.6783192753791809 C3 >= 0.2778348624706268
#>  R1: 0.2900900840759277 C0 + 0.3052216172218323 C1 + 0.3266035616397858 C2
#>    + 0.3510976731777191 C3 >= 0.1273012936115265
#>  R2: 0.8178213238716125 C0 + 0.8064534068107605 C1 + 0.7852441072463989 C2
#>    + 0.7541319727897644 C3 >= 0.3163650810718537
#>  R3: 0.219966322183609 C0 + 0.2713800370693207 C1 + 0.3296246528625488 C2
#>    + 0.3900085389614105 C3 >= 0.1210979551076889
#>  R4: 0.4533809423446655 C0 + 0.4413638710975647 C1 + 0.4343546628952026 C2
#>    + 0.4355628192424774 C3 >= 0.176466229557991
#> Bounds
#> Binaries
#>  C0 C1 C2 C3
#> End
```
