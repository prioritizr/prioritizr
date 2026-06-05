# Penalty class

This class is used to represent penalties used in optimization. **Only
experts should use the fields and methods for this class directly.**

## See also

Other classes:
[`ConservationModifier-class`](https://prioritizr.net/reference/ConservationModifier-class.md),
[`ConservationProblem-class`](https://prioritizr.net/reference/ConservationProblem-class.md),
[`Constraint-class`](https://prioritizr.net/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.net/reference/Decision-class.md),
[`MultiConservationProblem-class`](https://prioritizr.net/reference/MultiConservationProblem-class.md),
[`MultiObjApproach-class`](https://prioritizr.net/reference/MultiObjApproach-class.md),
[`Objective-class`](https://prioritizr.net/reference/Objective-class.md),
[`OptimizationProblem-class`](https://prioritizr.net/reference/OptimizationProblem-class.md),
[`Portfolio-class`](https://prioritizr.net/reference/Portfolio-class.md),
[`Solver-class`](https://prioritizr.net/reference/Solver-class.md),
[`Target-class`](https://prioritizr.net/reference/Target-class.md),
[`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Super class

[`ConservationModifier`](https://prioritizr.net/reference/ConservationModifier-class.md)
-\> `Penalty`

## Methods

### Public methods

- [`Penalty$apply()`](#method-Penalty-apply)

- [`Penalty$clone()`](#method-Penalty-clone)

Inherited methods

- [`ConservationModifier$calculate()`](https://prioritizr.net/reference/ConservationModifier.html#method-calculate)
- [`ConservationModifier$get_data()`](https://prioritizr.net/reference/ConservationModifier.html#method-get_data)
- [`ConservationModifier$get_internal()`](https://prioritizr.net/reference/ConservationModifier.html#method-get_internal)
- [`ConservationModifier$print()`](https://prioritizr.net/reference/ConservationModifier.html#method-print)
- [`ConservationModifier$repr()`](https://prioritizr.net/reference/ConservationModifier.html#method-repr)
- [`ConservationModifier$set_data()`](https://prioritizr.net/reference/ConservationModifier.html#method-set_data)
- [`ConservationModifier$set_internal()`](https://prioritizr.net/reference/ConservationModifier.html#method-set_internal)
- [`ConservationModifier$show()`](https://prioritizr.net/reference/ConservationModifier.html#method-show)

------------------------------------------------------------------------

### `Penalty$apply()`

Update an optimization problem formulation.

#### Usage

    Penalty$apply(x)

#### Arguments

- `x`:

  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  object.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `Penalty$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Penalty$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
