# Optimization problem methods

These functions are used to query and update a
[`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md).

## Usage

``` r
# S4 method for class 'OptimizationProblem'
nrow(x)

# S4 method for class 'OptimizationProblem'
ncol(x)

# S4 method for class 'OptimizationProblem'
ncell(x)

modelsense(x)

# S4 method for class 'OptimizationProblem'
modelsense(x)

vtype(x)

# S4 method for class 'OptimizationProblem'
vtype(x)

obj(x)

# S4 method for class 'OptimizationProblem'
obj(x)

A(x)

# S4 method for class 'OptimizationProblem'
A(x)

rhs(x)

# S4 method for class 'OptimizationProblem'
rhs(x)

sense(x)

# S4 method for class 'OptimizationProblem'
sense(x)

lb(x)

# S4 method for class 'OptimizationProblem'
lb(x)

ub(x)

# S4 method for class 'OptimizationProblem'
ub(x)

col_ids(x)

# S4 method for class 'OptimizationProblem'
col_ids(x)

row_ids(x)

# S4 method for class 'OptimizationProblem'
row_ids(x)

compressed_formulation(x)

# S4 method for class 'OptimizationProblem'
compressed_formulation(x)

set_obj(x, obj)

# S4 method for class 'OptimizationProblem,ANY'
set_obj(x, obj)

set_lb(x, lb)

# S4 method for class 'OptimizationProblem,ANY'
set_lb(x, lb)

set_ub(x, ub)

# S4 method for class 'OptimizationProblem,ANY'
set_ub(x, ub)

append_linear_constraints(x, rhs, sense, A, row_ids)

# S4 method for class 'OptimizationProblem,ANY,ANY,ANY,ANY'
append_linear_constraints(x, rhs, sense, A, row_ids)

remove_last_linear_constraint(x)

# S4 method for class 'OptimizationProblem'
remove_last_linear_constraint(x)
```

## Arguments

- x:

  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  object.

- obj:

  `numeric` vector containing a new linear coefficient for each decision
  variable in the problem.

- lb:

  `numeric` vector containing a new lower bound for each decision
  variable in the problem.

- ub:

  `numeric` vector containing a new upper bound for each decision
  variable in the problem.

- rhs:

  `numeric` vector with the right-hand-side values for new constraints.

- sense:

  `character` vector with senses for new constraints (i.e., `">="`,
  `"<="`, or "`=`" values).

- A:

  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  matrix with coefficients for new constraints.

- row_ids:

  `character` vector with identifiers for new constraints.

## Value

A
[`Matrix::dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html),
`numeric` vector, `numeric` vector, or scalar `integer` depending on the
method used.

## Details

The following functions are used to query data.

- `nrow(x)`:

  `integer` number of rows (constraints).

- `ncol(x)`:

  `integer` number of columns (decision variables).

- `ncell(x)`:

  `integer` number of cells.

- `modelsense(x)`:

  `character` describing if the problem is to be maximized (`"max"`) or
  minimized (`"min"`).

- `vtype(x)`:

  `character` describing the type of each decision variable: binary
  (`"B"`), semi-continuous (`"S"`), or continuous (`"C"`)

- `obj(x)`:

  `numeric` vector specifying the objective function.

- `A(x)`:

  [`Matrix::dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
  matrix object defining the problem matrix.

- `rhs(x)`:

  `numeric` vector with right-hand-side linear constraints

- `sense(x)`:

  `character` vector with the senses of the linear constraints (`"<="`,
  `">="`, `"="`).

- `lb(x)`:

  `numeric` lower bound for each decision variable. Missing data values
  (`NA`) indicate no lower bound for a given variable.

- `ub(x)`:

  `numeric` upper bounds for each decision variable. Missing data values
  (`NA`) indicate no upper bound for a given variable.

- `number_of_planning_units(x)`:

  `integer` number of planning units in the problem.

- `number_of_features(x)`:

  `integer` number of features the problem.

The following functions are used to update data. Note that these
functions return an invisible `TRUE` indicating success.

- `set_obj(x, obj)`:

  override the objective in the problem. Here, `obj` is a `numeric`
  vector containing a new linear coefficient for each decision variable
  in the problem.

- `set_lb(x, lb)`:

  override the variable lower bounds in the problem. Here, `lb` is a
  `numeric` vector containing a new lower bound.for each decision
  variable in the problem.

- `set_ub(x, ub)`:

  override the variable upper bounds in the problem. Here, `ub` is a
  `numeric` vector containing a new upper bound.for each decision
  variable in the problem.

- `remove_last_linear_constraint()`:

  remove the last linear constraint added to a problem.

- `append_linear_constraints(x, A, sense, rhs, row_ids)`:

  add an additional linear constraints to a problem. Here, `A` is a
  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  matrix, `sense` is a `character` vector with constraint senses (i.e.,
  `">="`, `"<="`, or "`=`" values), `rhs` is a `numeric` vector with the
  right-hand-side values, and `row_ids` is a `character` vector with
  identifiers.
