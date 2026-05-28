# Optimization problem

Create a new optimization problem.

## Usage

``` r
optimization_problem(x = NULL)
```

## Arguments

- x:

  A `NULL` or `list` object. If `x` is a `NULL`, then an empty
  optimization problem is created. Alternately, if a `x` is a `list`
  then a fully formulated optimization problem is created. See Details
  for more information on the `list` format. Defaults to `NULL`.

## Value

An
[OptimizationProblem](https://prioritizr.net/reference/OptimizationProblem-class.md)
object.

## Details

The argument to `x` can be a `NULL` or a `list` object. If `x` is a
`list`, then it must have the following elements.

- modelsense:

  `character` model sense.

- number_of_features:

  `integer` number of features in problem.

- number_of_planning_units:

  `integer` number of planning units.

- A_i:

  `integer` row indices for constraint matrix.

- A_j:

  `integer` column indices for constraint matrix.

- A_x:

  `numeric` values for constraint matrix.

- obj:

  `numeric` objective function values.

- lb:

  `numeric` lower bound for decision values.

- ub:

  `numeric` upper bound for decision values.

- rhs:

  `numeric` right-hand side values.

- sense:

  `numeric` constraint senses.

- vtype:

  `character` variable types. These are used to specify that the
  decision variables are binary (`"B"`), continuous (`"C"`), or
  semi-continuous (`"S"`).

- row_ids:

  `character` identifiers for the rows in the constraint matrix.

- col_ids:

  `character` identifiers for the columns in the constraint matrix.

## Examples

``` r
# create new empty object
x1 <- optimization_problem()

# print new empty object
print(x1)
#> An optimization problem (<OptimizationProblem>)
#> • model sense: missing
#> • dimensions:  0, 0, 0 (rows, columns, cells)
#> • variables:   none

# create list with optimization problem
l <- list(
  modelsense = "min",
  number_of_features = 2,
  number_of_planning_units = 3,
  number_of_zones = 1,
  A_i = c(0L, 1L, 0L, 1L, 0L, 1L),
  A_j = c(0L, 0L, 1L, 1L, 2L, 2L),
  A_x = c(2, 10, 1, 10, 1, 10),
  obj = c(1, 2, 2),
  lb = c(0, 1, 0),
  ub = c(0, 1, 1),
  rhs = c(2, 10),
  compressed_formulation = TRUE,
  sense = c(">=", ">="),
  vtype = c("B", "B", "B"),
  row_ids = c("spp_target", "spp_target"),
  col_ids = c("pu", "pu", "pu")
)

# create fully formulated object based on lists
x2 <- optimization_problem(l)

# print fully formulated object
print(x2)
#> An optimization problem (<OptimizationProblem>)
#> • model sense: min
#> • dimensions:  2, 3, 6 (rows, columns, cells)
#> • variables:   3 (B)
```
