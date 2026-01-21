# Solver class

This class is used to represent solvers for optimization. **Only experts
should use the fields and methods for this class directly.**

## See also

Other classes:
[`ConservationModifier-class`](https://prioritizr.net/reference/ConservationModifier-class.md),
[`ConservationProblem-class`](https://prioritizr.net/reference/ConservationProblem-class.md),
[`Constraint-class`](https://prioritizr.net/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.net/reference/Decision-class.md),
[`Objective-class`](https://prioritizr.net/reference/Objective-class.md),
[`OptimizationProblem-class`](https://prioritizr.net/reference/OptimizationProblem-class.md),
[`Penalty-class`](https://prioritizr.net/reference/Penalty-class.md),
[`Portfolio-class`](https://prioritizr.net/reference/Portfolio-class.md),
[`Target-class`](https://prioritizr.net/reference/Target-class.md),
[`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Super class

[`prioritizr::ConservationModifier`](https://prioritizr.net/reference/ConservationModifier-class.md)
-\> `Solver`

## Methods

### Public methods

- [`Solver$run()`](#method-Solver-run)

- [`Solver$calculate()`](#method-Solver-calculate)

- [`Solver$set_variable_ub()`](#method-Solver-set_variable_ub)

- [`Solver$set_variable_lb()`](#method-Solver-set_variable_lb)

- [`Solver$set_constraint_rhs()`](#method-Solver-set_constraint_rhs)

- [`Solver$set_start_solution()`](#method-Solver-set_start_solution)

- [`Solver$remove_start_solution()`](#method-Solver-remove_start_solution)

- [`Solver$solve()`](#method-Solver-solve)

- [`Solver$clone()`](#method-Solver-clone)

Inherited methods

- [`prioritizr::ConservationModifier$get_data()`](https://prioritizr.net/reference/ConservationModifier.html#method-get_data)
- [`prioritizr::ConservationModifier$get_internal()`](https://prioritizr.net/reference/ConservationModifier.html#method-get_internal)
- [`prioritizr::ConservationModifier$print()`](https://prioritizr.net/reference/ConservationModifier.html#method-print)
- [`prioritizr::ConservationModifier$repr()`](https://prioritizr.net/reference/ConservationModifier.html#method-repr)
- [`prioritizr::ConservationModifier$set_data()`](https://prioritizr.net/reference/ConservationModifier.html#method-set_data)
- [`prioritizr::ConservationModifier$set_internal()`](https://prioritizr.net/reference/ConservationModifier.html#method-set_internal)
- [`prioritizr::ConservationModifier$show()`](https://prioritizr.net/reference/ConservationModifier.html#method-show)

------------------------------------------------------------------------

### Method `run()`

Run the solver to generate a solution.

#### Usage

    Solver$run()

#### Returns

`list` of solutions.

------------------------------------------------------------------------

### Method `calculate()`

Perform computations that need to be completed before applying the
object.

#### Usage

    Solver$calculate(...)

#### Arguments

- `...`:

  Additional arguments.

- `x`:

  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  object.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `set_variable_ub()`

Set the upper bound for a decision variable.

#### Usage

    Solver$set_variable_ub(index, value)

#### Arguments

- `index`:

  `integer` value indicating the index of the decision variable.

- `value`:

  `numeric` new bound value.

#### Details

Note that this method should only be run after `$calculate()`. It can be
used to overwrite values after ingesting an
[`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
object. It is designed to be used in
[portfolios](https://prioritizr.net/reference/portfolios.md) and
[importance](https://prioritizr.net/reference/importance.md) functions.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `set_variable_lb()`

Set the lower bound for a decision variable.

#### Usage

    Solver$set_variable_lb(index, value)

#### Arguments

- `index`:

  `integer` value indicating the index of the decision variable.

- `value`:

  `numeric` new bound value.

#### Details

Note that this method should only be run after `$calculate()`. It can be
used to overwrite values after ingesting an
[`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
object. It is designed to be used in
[portfolios](https://prioritizr.net/reference/portfolios.md) and
[importance](https://prioritizr.net/reference/importance.md) functions.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `set_constraint_rhs()`

Set the right-hand-side coefficient bound for a constraint.

#### Usage

    Solver$set_constraint_rhs(index, value)

#### Arguments

- `index`:

  `integer` value indicating the index of the decision variable.

- `value`:

  `numeric` new value.

#### Details

Note that this method should only be run after `$calculate()`. It can be
used to overwrite values after ingesting an
[`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
object. It is designed to be used in
[portfolios](https://prioritizr.net/reference/portfolios.md) and
[importance](https://prioritizr.net/reference/importance.md) functions.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `set_start_solution()`

Set the starting solution.

#### Usage

    Solver$set_start_solution(value, warn = TRUE)

#### Arguments

- `value`:

  `numeric` vector.

- `warn`:

  `logical` indicating if a warning should be displayed if the solver
  does not support starting solutions.

#### Details

This method is designed used in
[portfolios](https://prioritizr.net/reference/portfolios.md) and
[importance](https://prioritizr.net/reference/importance.md) functions.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `remove_start_solution()`

Remove the starting solution.

#### Usage

    Solver$remove_start_solution()

#### Details

This method is designed used in
[portfolios](https://prioritizr.net/reference/portfolios.md) and
[importance](https://prioritizr.net/reference/importance.md) functions.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method [`solve()`](https://prioritizr.net/reference/solve.md)

Solve an optimization problem.

#### Usage

    Solver$solve(x, ...)

#### Arguments

- `x`:

  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  object.

- `...`:

  Additional arguments passed to the `calculate()` method.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Solver$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
