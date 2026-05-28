# Solver class

This class is used to represent solvers for optimization. **Only experts
should use the fields and methods for this class directly.**

## See also

Other classes:
[`ConservationModifier-class`](https://prioritizr.net/reference/ConservationModifier-class.md),
[`ConservationProblem-class`](https://prioritizr.net/reference/ConservationProblem-class.md),
[`Constraint-class`](https://prioritizr.net/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.net/reference/Decision-class.md),
[`MultiObjApproach-class`](https://prioritizr.net/reference/MultiObjApproach-class.md),
[`MultiObjConservationProblem-class`](https://prioritizr.net/reference/MultiObjConservationProblem-class.md),
[`Objective-class`](https://prioritizr.net/reference/Objective-class.md),
[`OptimizationProblem-class`](https://prioritizr.net/reference/OptimizationProblem-class.md),
[`Penalty-class`](https://prioritizr.net/reference/Penalty-class.md),
[`Portfolio-class`](https://prioritizr.net/reference/Portfolio-class.md),
[`Target-class`](https://prioritizr.net/reference/Target-class.md),
[`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Super class

[`ConservationModifier`](https://prioritizr.net/reference/ConservationModifier-class.md)
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

- [`Solver$default_solve_multiobj()`](#method-Solver-default_solve_multiobj)

- [`Solver$solve_multiobj()`](#method-Solver-solve_multiobj)

- [`Solver$clone()`](#method-Solver-clone)

Inherited methods

- [`ConservationModifier$get_data()`](https://prioritizr.net/reference/ConservationModifier.html#method-get_data)
- [`ConservationModifier$get_internal()`](https://prioritizr.net/reference/ConservationModifier.html#method-get_internal)
- [`ConservationModifier$print()`](https://prioritizr.net/reference/ConservationModifier.html#method-print)
- [`ConservationModifier$repr()`](https://prioritizr.net/reference/ConservationModifier.html#method-repr)
- [`ConservationModifier$set_data()`](https://prioritizr.net/reference/ConservationModifier.html#method-set_data)
- [`ConservationModifier$set_internal()`](https://prioritizr.net/reference/ConservationModifier.html#method-set_internal)
- [`ConservationModifier$show()`](https://prioritizr.net/reference/ConservationModifier.html#method-show)

------------------------------------------------------------------------

### `Solver$run()`

Run the solver to generate a solution.

#### Usage

    Solver$run()

#### Returns

`list` of solutions.

------------------------------------------------------------------------

### `Solver$calculate()`

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

### `Solver$set_variable_ub()`

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

### `Solver$set_variable_lb()`

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

### `Solver$set_constraint_rhs()`

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

### `Solver$set_start_solution()`

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

### `Solver$remove_start_solution()`

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

### `Solver$solve()`

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

A `list` object with the solution and additional information.

------------------------------------------------------------------------

### `Solver$default_solve_multiobj()`

Solve a multi-objective optimization problem using a hierarchical
multi-objective optimization approach. Broadly speaking, this approach
involves using multiple optimization procedures to solve objectives
following a hierarchical (lexicographic) ordering, wherein those
associated with a higher priority order are solved before those with a
lower priority order. When implementing this approach, constraints are
added after generating a given solution to ensure that subsequent
solutions for lower priority objectives have adequate performance
according to higher priority objectives.

#### Usage

    Solver$default_solve_multiobj(x, priority, rel_tol, ...)

#### Arguments

- `x`:

  `list` object with multi-objective optimization problem. Arguments
  must contain the following elements: (`"opt"`)
  [`OptimizationProblem`](https://prioritizr.net/reference/OptimizationProblem-class.md)
  object; (`"modelsense"`) `character` vector containing the model sense
  values for each objective; and (`"obj"`) numeric\` matrix containing
  the coefficients for each of the objectives, wherein rows correspond
  to different objectives, columns to different decision variables and
  row names can be optionally specify names for the objectives.

- `priority`:

  `numeric` vector with values indicating the priority for each
  objective. Greater values denote greater priority, and so objectives
  associated with greater values are optimized earlier in the
  multi-objective process.

- `rel_tol`:

  `numeric` vector with relative tolerance values for each constraint.
  Greater values denote a greater degree of sub-optimality.

- `...`:

  Additional arguments passed to the `calculate()` method.

#### Returns

A `list` object with the solution and additional information.

------------------------------------------------------------------------

### `Solver$solve_multiobj()`

Solve a multi-objective optimization problem using a hierarchical
multi-objective optimization approach. Broadly speaking, this approach
involves using multiple optimization procedures to solve objectives
following a hierarchical (lexicographic) ordering, wherein those
associated with a higher priority order are solved before those with a
lower priority order. When implementing this approach, constraints are
added after generating a given solution to ensure that subsequent
solutions for lower priority objectives have adequate performance
according to higher priority objectives.

#### Usage

    Solver$solve_multiobj(x, priority, rel_tol, ...)

#### Arguments

- `x`:

  `list` object with multi-objective optimization problem. Arguments
  must contain the following elements: (`"opt"`)
  [`OptimizationProblem`](https://prioritizr.net/reference/OptimizationProblem-class.md)
  object; (`"modelsense"`) `character` vector containing the model sense
  values for each objective; and (`"obj"`) numeric\` matrix containing
  the coefficients for each of the objectives, wherein rows correspond
  to different objectives, columns to different decision variables and
  row names can be optionally specify names for the objectives.

- `priority`:

  `numeric` vector with values indicating the priority for each
  objective. Greater values denote greater priority, and so objectives
  associated with greater values are optimized earlier in the
  multi-objective process.

- `rel_tol`:

  `numeric` vector with relative tolerance values for each constraint.
  Greater values denote a greater degree of sub-optimality.

- `...`:

  Additional arguments passed to the `calculate()` method.

#### Returns

A `list` object with the solution and additional information.

------------------------------------------------------------------------

### `Solver$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Solver$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
