# Optimization problem class

This class is used to represent an optimization problem. It stores the
information needed to generate a solution using an exact algorithm
solver. Most users should use
[`compile()`](https://prioritizr.net/reference/compile.md) to generate
new optimization problem objects, and the functions distributed with the
package to interact with them (e.g.,
[`base::as.list()`](https://rdrr.io/r/base/list.html)). **Only experts
should use the fields and methods for this class directly.**

## See also

Other classes:
[`ConservationModifier-class`](https://prioritizr.net/reference/ConservationModifier-class.md),
[`ConservationProblem-class`](https://prioritizr.net/reference/ConservationProblem-class.md),
[`Constraint-class`](https://prioritizr.net/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.net/reference/Decision-class.md),
[`Objective-class`](https://prioritizr.net/reference/Objective-class.md),
[`Penalty-class`](https://prioritizr.net/reference/Penalty-class.md),
[`Portfolio-class`](https://prioritizr.net/reference/Portfolio-class.md),
[`Solver-class`](https://prioritizr.net/reference/Solver-class.md),
[`Target-class`](https://prioritizr.net/reference/Target-class.md),
[`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Public fields

- `ptr`:

  A `Rcpp::Xptr` external pointer. Create a new optimization problem
  object.

## Methods

### Public methods

- [`OptimizationProblem$new()`](#method-OptimizationProblem-new)

- [`OptimizationProblem$print()`](#method-OptimizationProblem-print)

- [`OptimizationProblem$show()`](#method-OptimizationProblem-show)

- [`OptimizationProblem$ncol()`](#method-OptimizationProblem-ncol)

- [`OptimizationProblem$nrow()`](#method-OptimizationProblem-nrow)

- [`OptimizationProblem$ncell()`](#method-OptimizationProblem-ncell)

- [`OptimizationProblem$modelsense()`](#method-OptimizationProblem-modelsense)

- [`OptimizationProblem$vtype()`](#method-OptimizationProblem-vtype)

- [`OptimizationProblem$obj()`](#method-OptimizationProblem-obj)

- [`OptimizationProblem$A()`](#method-OptimizationProblem-A)

- [`OptimizationProblem$rhs()`](#method-OptimizationProblem-rhs)

- [`OptimizationProblem$sense()`](#method-OptimizationProblem-sense)

- [`OptimizationProblem$lb()`](#method-OptimizationProblem-lb)

- [`OptimizationProblem$ub()`](#method-OptimizationProblem-ub)

- [`OptimizationProblem$number_of_features()`](#method-OptimizationProblem-number_of_features)

- [`OptimizationProblem$number_of_planning_units()`](#method-OptimizationProblem-number_of_planning_units)

- [`OptimizationProblem$number_of_zones()`](#method-OptimizationProblem-number_of_zones)

- [`OptimizationProblem$col_ids()`](#method-OptimizationProblem-col_ids)

- [`OptimizationProblem$row_ids()`](#method-OptimizationProblem-row_ids)

- [`OptimizationProblem$compressed_formulation()`](#method-OptimizationProblem-compressed_formulation)

- [`OptimizationProblem$shuffle_columns()`](#method-OptimizationProblem-shuffle_columns)

- [`OptimizationProblem$copy()`](#method-OptimizationProblem-copy)

- [`OptimizationProblem$set_obj()`](#method-OptimizationProblem-set_obj)

- [`OptimizationProblem$set_lb()`](#method-OptimizationProblem-set_lb)

- [`OptimizationProblem$set_ub()`](#method-OptimizationProblem-set_ub)

- [`OptimizationProblem$remove_last_linear_constraint()`](#method-OptimizationProblem-remove_last_linear_constraint)

- [`OptimizationProblem$append_linear_constraints()`](#method-OptimizationProblem-append_linear_constraints)

- [`OptimizationProblem$clone()`](#method-OptimizationProblem-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

#### Usage

    OptimizationProblem$new(ptr)

#### Arguments

- `ptr`:

  `Rcpp::Xptr` external pointer.

#### Returns

A new `OptimizationProblem` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print concise information about the object.

#### Usage

    OptimizationProblem$print()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method [`show()`](https://prioritizr.net/reference/show.md)

Print concise information about the object.

#### Usage

    OptimizationProblem$show()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method [`ncol()`](https://rspatial.github.io/terra/reference/dimensions.html)

Obtain the number of columns in the problem formulation.

#### Usage

    OptimizationProblem$ncol()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method [`nrow()`](https://rspatial.github.io/terra/reference/dimensions.html)

Obtain the number of rows in the problem formulation.

#### Usage

    OptimizationProblem$nrow()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method [`ncell()`](https://rspatial.github.io/terra/reference/dimensions.html)

Obtain the number of cells in the problem formulation.

#### Usage

    OptimizationProblem$ncell()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method [`modelsense()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Obtain the model sense.

#### Usage

    OptimizationProblem$modelsense()

#### Returns

A `character` value.

------------------------------------------------------------------------

### Method [`vtype()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Obtain the decision variable types.

#### Usage

    OptimizationProblem$vtype()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### Method [`obj()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Obtain the objective function.

#### Usage

    OptimizationProblem$obj()

#### Returns

A `numeric` vector.

------------------------------------------------------------------------

### Method [`A()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Obtain the constraint matrix.

#### Usage

    OptimizationProblem$A()

#### Returns

A
[`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
object.

------------------------------------------------------------------------

### Method [`rhs()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Obtain the right-hand-side constraint values.

#### Usage

    OptimizationProblem$rhs()

#### Returns

A `numeric` vector.

------------------------------------------------------------------------

### Method [`sense()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Obtain the constraint senses.

#### Usage

    OptimizationProblem$sense()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### Method [`lb()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Obtain the lower bounds for the decision variables.

#### Usage

    OptimizationProblem$lb()

#### Returns

A `numeric` vector.

------------------------------------------------------------------------

### Method [`ub()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Obtain the upper bounds for the decision variables.

#### Usage

    OptimizationProblem$ub()

#### Returns

A `numeric` vector.

------------------------------------------------------------------------

### Method [`number_of_features()`](https://prioritizr.net/reference/number_of_features.md)

Obtain the number of features.

#### Usage

    OptimizationProblem$number_of_features()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method [`number_of_planning_units()`](https://prioritizr.net/reference/number_of_planning_units.md)

Obtain the number of planning units.

#### Usage

    OptimizationProblem$number_of_planning_units()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method [`number_of_zones()`](https://prioritizr.net/reference/number_of_zones.md)

Obtain the number of zones.

#### Usage

    OptimizationProblem$number_of_zones()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method [`col_ids()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Obtain the identifiers for the columns.

#### Usage

    OptimizationProblem$col_ids()

#### Returns

A `character` value.

------------------------------------------------------------------------

### Method [`row_ids()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Obtain the identifiers for the rows.

#### Usage

    OptimizationProblem$row_ids()

#### Returns

A `character` value.

------------------------------------------------------------------------

### Method [`compressed_formulation()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Is the problem formulation compressed?

#### Usage

    OptimizationProblem$compressed_formulation()

#### Returns

A `logical` value.

------------------------------------------------------------------------

### Method `shuffle_columns()`

Shuffle the order of the columns in the optimization problem.

#### Usage

    OptimizationProblem$shuffle_columns(order)

#### Arguments

- `order`:

  `integer` vector with new order.

#### Returns

An `integer` vector with indices to un-shuffle the problem.

------------------------------------------------------------------------

### Method `copy()`

Create a copy of the optimization problem.

#### Usage

    OptimizationProblem$copy()

#### Returns

A new `OptimizationProblem` object .

------------------------------------------------------------------------

### Method [`set_obj()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Set objective coefficients for the decision variables in the
optimization problem.

#### Usage

    OptimizationProblem$set_obj(obj)

#### Arguments

- `obj`:

  `numeric` vector.

#### Returns

An invisible `TRUE` indicating success.

------------------------------------------------------------------------

### Method [`set_lb()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Set lower bounds for the decision variables in the optimization problem.

#### Usage

    OptimizationProblem$set_lb(lb)

#### Arguments

- `lb`:

  `numeric` vector.

#### Returns

An invisible `TRUE` indicating success.

------------------------------------------------------------------------

### Method [`set_ub()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Set upper bounds for the decision variables in the optimization problem.

#### Usage

    OptimizationProblem$set_ub(ub)

#### Arguments

- `ub`:

  `numeric` vector.

#### Returns

An invisible `TRUE` indicating success.

------------------------------------------------------------------------

### Method [`remove_last_linear_constraint()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Remove last linear constraint added to a problem.

#### Usage

    OptimizationProblem$remove_last_linear_constraint()

#### Returns

An invisible `TRUE` indicating success.

------------------------------------------------------------------------

### Method [`append_linear_constraints()`](https://prioritizr.net/reference/OptimizationProblem-methods.md)

Append linear constraints to the optimization problem.

#### Usage

    OptimizationProblem$append_linear_constraints(rhs, sense, A, row_ids)

#### Arguments

- `rhs`:

  `numeric` vector with right-hand-side values.

- `sense`:

  `character` vector with constraint sense values (i.e., `"<="`, `">="`,
  or `"="`).

- `A`:

  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  with constraint coefficients.

- `row_ids`:

  `character` vector with identifier for constraints.

#### Returns

An invisible `TRUE` indicating success.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OptimizationProblem$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
