# Portfolio class

This class is used to represent portfolios used in optimization. **Only
experts should use the fields and methods for this class directly.**

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
[`Solver-class`](https://prioritizr.net/reference/Solver-class.md),
[`Target-class`](https://prioritizr.net/reference/Target-class.md),
[`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Super class

[`prioritizr::ConservationModifier`](https://prioritizr.net/reference/ConservationModifier-class.md)
-\> `Portfolio`

## Methods

### Public methods

- [`Portfolio$run()`](#method-Portfolio-run)

- [`Portfolio$clone()`](#method-Portfolio-clone)

Inherited methods

- [`prioritizr::ConservationModifier$calculate()`](https://prioritizr.net/reference/ConservationModifier.html#method-calculate)
- [`prioritizr::ConservationModifier$get_data()`](https://prioritizr.net/reference/ConservationModifier.html#method-get_data)
- [`prioritizr::ConservationModifier$get_internal()`](https://prioritizr.net/reference/ConservationModifier.html#method-get_internal)
- [`prioritizr::ConservationModifier$print()`](https://prioritizr.net/reference/ConservationModifier.html#method-print)
- [`prioritizr::ConservationModifier$repr()`](https://prioritizr.net/reference/ConservationModifier.html#method-repr)
- [`prioritizr::ConservationModifier$set_data()`](https://prioritizr.net/reference/ConservationModifier.html#method-set_data)
- [`prioritizr::ConservationModifier$set_internal()`](https://prioritizr.net/reference/ConservationModifier.html#method-set_internal)
- [`prioritizr::ConservationModifier$show()`](https://prioritizr.net/reference/ConservationModifier.html#method-show)

------------------------------------------------------------------------

### Method `run()`

Run the portfolio to generate solutions.

#### Usage

    Portfolio$run(x, solver)

#### Arguments

- `x`:

  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  object.

- `solver`:

  [`Solver`](https://prioritizr.net/reference/Solver-class.md) object.

#### Returns

`list` of solutions.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Portfolio$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
