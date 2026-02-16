# Objective class

This class is used to represent the objective function used in
optimization. **Only experts should use the fields and methods for this
class directly.**

## See also

Other classes:
[`ConservationModifier-class`](https://prioritizr.net/reference/ConservationModifier-class.md),
[`ConservationProblem-class`](https://prioritizr.net/reference/ConservationProblem-class.md),
[`Constraint-class`](https://prioritizr.net/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.net/reference/Decision-class.md),
[`MultiObjApproach-class`](https://prioritizr.net/reference/MultiObjApproach-class.md),
[`MultiObjConservationProblem-class`](https://prioritizr.net/reference/MultiObjConservationProblem-class.md),
[`OptimizationProblem-class`](https://prioritizr.net/reference/OptimizationProblem-class.md),
[`Penalty-class`](https://prioritizr.net/reference/Penalty-class.md),
[`Portfolio-class`](https://prioritizr.net/reference/Portfolio-class.md),
[`Solver-class`](https://prioritizr.net/reference/Solver-class.md),
[`Target-class`](https://prioritizr.net/reference/Target-class.md),
[`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Super class

[`prioritizr::ConservationModifier`](https://prioritizr.net/reference/ConservationModifier-class.md)
-\> `Objective`

## Public fields

- `has_targets`:

  `logical` value indicating if the objective supports targets.

- `has_weights`:

  `logical` value indicating if the objective supports feature weights.

## Methods

### Public methods

- [`Objective$default_weights()`](#method-Objective-default_weights)

- [`Objective$apply()`](#method-Objective-apply)

- [`Objective$clone()`](#method-Objective-clone)

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

### Method `default_weights()`

Specify default value for the feature weights.

#### Usage

    Objective$default_weights()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### Method [`apply()`](https://rdrr.io/r/base/apply.html)

Update an optimization problem formulation.

#### Usage

    Objective$apply(x)

#### Arguments

- `x`:

  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  object.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Objective$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
