# *Marxan* conservation problem

Create a conservation planning
[`problem()`](https://prioritizr.net/reference/problem.md) following the
mathematical formulations used in *Marxan* (detailed in Beyer *et al.*
2016). Note that these problems are solved using exact algorithms and
not simulated annealing (i.e., the *Marxan* software). Please note that
the vroom package is required to import *Marxan* data files.

## Usage

``` r
marxan_problem(x, ...)

# Default S3 method
marxan_problem(x, ...)

# S3 method for class 'data.frame'
marxan_problem(x, spec, puvspr, bound = NULL, blm = 0, symmetric = TRUE, ...)

# S3 method for class 'character'
marxan_problem(x, ...)
```

## Arguments

- x:

  `character` file path for a *Marxan* input file (typically called
  `"input.dat"`), or `data.frame` containing planning unit data
  (typically called `"pu.dat"`). If `x` is a `data.frame`, then each row
  corresponds to a different planning unit, and it must have the
  following columns.

  id

  :   `integer` unique identifier for each planning unit. These
      identifiers are used by `puvspr`.

  cost

  :   `numeric` cost of each planning unit.

  status

  :   `integer` indicating if each planning unit should not be locked in
      the solution (0) or if it should be locked in (2) or locked
      out (3) of the solution. Although *Marxan* allows planning units
      to be selected in the initial solution (using values of 1), these
      values have no effect here. This column is optional.

- ...:

  not used.

- spec:

  `data.frame` containing information on the features. In particular,
  `spec` must follow the conventions used by *Marxan* for the species
  data file (conventionally called `"spec.dat"`). Each row corresponds
  to a different feature and each column corresponds to different
  information about the features. It must contain the columns listed
  below. Note that `spec` must contain at least one column named
  `"prop"` or `"amount"` – **but not columns with both of these names**
  – to specify the target for each feature.

  id

  :   `integer` unique identifier for each feature These identifiers are
      used by `puvspr`.

  name

  :   `character` name for each feature.

  prop

  :   `numeric` relative target for each feature (optional).

  amount

  :   `numeric` absolute target for each feature (optional).

- puvspr:

  `data.frame` containing information on the amount of each feature in
  each planning unit. In particular, `puvspr` must follow the
  conventions used in the *Marxan* input data file (conventionally
  called `"puvspr.dat"`). It must contain the following columns.

  pu

  :   `integer` planning unit identifier.

  species

  :   `integer` feature identifier.

  amount

  :   `numeric` amount of the feature in the planning unit.

- bound:

  `NULL` object indicating that no boundary data is required for the
  conservation planning problem, or a `data.frame` containing
  information on the planning units' boundaries. If `bound` is a
  `data.frame` object, then it must follow the conventions used in the
  *Marxan* input data file (conventionally called `"bound.dat"`). In
  particular, it must contain the following columns.

  id1

  :   `integer` planning unit identifier.

  id2

  :   `integer` planning unit identifier.

  boundary

  :   `numeric` length of shared boundary between the planning units
      identified in the previous two columns.

- blm:

  `numeric` boundary length modifier value. Note that `blm` will only
  have an affect if `bound` is a `data.frame` object. Defaults to 0.

- symmetric:

  `logical` value indicating if `bound` describes symmetric
  relationships between planning units? If `bound` has asymmetric
  connectivity data, then use `symmetric = FALSE`. Defaults to `TRUE`.

## Value

A [`problem()`](https://prioritizr.net/reference/problem.md) object.

## Details

This function is provided as a convenient interface for solving *Marxan*
problems using the prioritizr package. Note that this function does not
support all of the functionality provided by the *Marxan* software. In
particular, only the following parameters supported: `"INPUTDIR"`,
`"SPECNAME`", `"PUNAME"`,
`"PUVSPRNAME", `"BOUNDNAME"`, `"BLM"`, and `"ASYMMETRICCONNECTIVITY"`. Additionally, for the species data (per `spec`), only the `"id"`, `"name"`, `"prop"`, and `"amount"\`
columns are considered.

## Notes

In previous versions, this function could not accommodate asymmetric
connectivity data. It has now been updated to handle asymmetric
connectivity data.

## References

Ball IR, Possingham HP, and Watts M (2009) *Marxan and relatives:
Software for spatial conservation prioritisation* in Spatial
conservation prioritisation: Quantitative methods and computational
tools. Eds Moilanen A, Wilson KA, and Possingham HP. Oxford University
Press, Oxford, UK.

Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
conservation planning problems with integer linear programming.
*Ecological Modelling*, 228: 14–22.

Serra N, Kockel A, Game ET, Grantham H, Possingham HP, and McGowan J
(2020) Marxan User Manual: For Marxan version 2.43 and above. The Nature
Conservancy (TNC), Arlington, Virginia, United States and Pacific Marine
Analysis and Research Association (PacMARA), Victoria, British Columbia,
Canada.

## See also

For more information on the correct format for for *Marxan* input data,
see the [official *Marxan* website](https://marxansolutions.org), Ball
*et al.* (2009), and Serra *et al.* (2020).

## Examples

``` r
# create Marxan problem using Marxan input file
# (note this example requires the vroom package to be installed)
input_file <- system.file("extdata/marxan/input.dat", package = "prioritizr")
p1 <-
  marxan_problem(input_file) %>%
  add_default_solver(verbose = FALSE)

# solve problem
s1 <- solve(p1)

# print solution
head(s1)
#> # A tibble: 6 × 8
#>      id    cost status     xloc      yloc locked_in locked_out solution_1
#>   <dbl>   <dbl>  <dbl>    <dbl>     <dbl> <lgl>     <lgl>           <dbl>
#> 1     3      0       0 1116623. -4493479. FALSE     FALSE               0
#> 2    30   7527.      3 1110623. -4496943. FALSE     TRUE                0
#> 3    56  37349.      0 1092623. -4500408. FALSE     FALSE               0
#> 4    58  16959.      0 1116623. -4500408. FALSE     FALSE               0
#> 5    84  34220.      0 1098623. -4503872. FALSE     FALSE               0
#> 6    85 178908.      0 1110623. -4503872. FALSE     FALSE               0

# create Marxan problem using data.frames that have been loaded into R
# (note this example also requires the vroom package to be installed)
## load in planning unit data
pu_path <- system.file("extdata/marxan/input/pu.dat", package = "prioritizr")
pu_dat <- vroom::vroom(pu_path)
#> Rows: 1751 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> dbl (5): id, cost, status, xloc, yloc
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
head(pu_dat)
#> # A tibble: 6 × 5
#>      id    cost status     xloc      yloc
#>   <dbl>   <dbl>  <dbl>    <dbl>     <dbl>
#> 1     3      0       0 1116623. -4493479.
#> 2    30   7527.      3 1110623. -4496943.
#> 3    56  37349.      0 1092623. -4500408.
#> 4    58  16959.      0 1116623. -4500408.
#> 5    84  34220.      0 1098623. -4503872.
#> 6    85 178908.      0 1110623. -4503872.

## load in feature data
spec_path <- system.file(
  "extdata/marxan/input/spec.dat", package = "prioritizr"
)
spec_dat <- vroom::vroom(spec_path)
#> Rows: 17 Columns: 4
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (1): name
#> dbl (3): id, prop, spf
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
head(spec_dat)
#> # A tibble: 6 × 4
#>      id  prop   spf name  
#>   <dbl> <dbl> <dbl> <chr> 
#> 1    10   0.3     1 bird1 
#> 2    11   0.3     1 nvis2 
#> 3    12   0.3     1 nvis8 
#> 4    13   0.3     1 nvis9 
#> 5    14   0.3     1 nvis14
#> 6    15   0.3     1 nvis20

## load in planning unit vs feature data
puvspr_path <- system.file(
  "extdata/marxan/input/puvspr.dat", package = "prioritizr"
)
puvspr_dat <- vroom::vroom(puvspr_path)
#> Rows: 4662 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> dbl (3): species, pu, amount
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
head(puvspr_dat)
#> # A tibble: 6 × 3
#>   species    pu amount
#>     <dbl> <dbl>  <dbl>
#> 1      26    56 120.  
#> 2      26    58  45.2 
#> 3      26    84  68.0 
#> 4      26    85   9.74
#> 5      26    86   7.80
#> 6      26   111 478.  

## load in the boundary data
bound_path <- system.file(
  "extdata/marxan/input/bound.dat", package = "prioritizr"
)
bound_dat <- vroom::vroom(bound_path)
#> Rows: 5256 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> dbl (3): id1, id2, boundary
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
head(bound_dat)
#> # A tibble: 6 × 3
#>     id1   id2 boundary
#>   <dbl> <dbl>    <dbl>
#> 1     3     3    16000
#> 2     3    30     4000
#> 3     3    58     4000
#> 4    30    30    12000
#> 5    30    58     4000
#> 6    30    85     4000

# create problem without the boundary data
p2 <-
  marxan_problem(pu_dat, spec_dat, puvspr_dat) %>%
  add_default_solver(verbose = FALSE)

# solve problem
s2 <- solve(p2)

# print solution
head(s2)
#> # A tibble: 6 × 8
#>      id    cost status     xloc      yloc locked_in locked_out solution_1
#>   <dbl>   <dbl>  <dbl>    <dbl>     <dbl> <lgl>     <lgl>           <dbl>
#> 1     3      0       0 1116623. -4493479. FALSE     FALSE               0
#> 2    30   7527.      3 1110623. -4496943. FALSE     TRUE                0
#> 3    56  37349.      0 1092623. -4500408. FALSE     FALSE               1
#> 4    58  16959.      0 1116623. -4500408. FALSE     FALSE               0
#> 5    84  34220.      0 1098623. -4503872. FALSE     FALSE               0
#> 6    85 178908.      0 1110623. -4503872. FALSE     FALSE               0

# create problem with the boundary data and a boundary length modifier
# set to 5
p3 <-
  marxan_problem(pu_dat, spec_dat, puvspr_dat, bound_dat, blm = 5) %>%
  add_default_solver(verbose = FALSE)

# solve problem
s3 <- solve(p3)

# print solution
head(s3)
#> # A tibble: 6 × 8
#>      id    cost status     xloc      yloc locked_in locked_out solution_1
#>   <dbl>   <dbl>  <dbl>    <dbl>     <dbl> <lgl>     <lgl>           <dbl>
#> 1     3      0       0 1116623. -4493479. FALSE     FALSE               0
#> 2    30   7527.      3 1110623. -4496943. FALSE     TRUE                0
#> 3    56  37349.      0 1092623. -4500408. FALSE     FALSE               0
#> 4    58  16959.      0 1116623. -4500408. FALSE     FALSE               0
#> 5    84  34220.      0 1098623. -4503872. FALSE     FALSE               0
#> 6    85 178908.      0 1110623. -4503872. FALSE     FALSE               0
```
