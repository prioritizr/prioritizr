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
[`MultiConservationProblem-class`](https://prioritizr.net/reference/MultiConservationProblem-class.md),
[`MultiObjApproach-class`](https://prioritizr.net/reference/MultiObjApproach-class.md),
[`OptimizationProblem-class`](https://prioritizr.net/reference/OptimizationProblem-class.md),
[`Penalty-class`](https://prioritizr.net/reference/Penalty-class.md),
[`Portfolio-class`](https://prioritizr.net/reference/Portfolio-class.md),
[`Solver-class`](https://prioritizr.net/reference/Solver-class.md),
[`Target-class`](https://prioritizr.net/reference/Target-class.md),
[`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Super class

[`ConservationModifier`](https://prioritizr.net/reference/ConservationModifier-class.md)
-\> `Objective`

## Public fields

- `has_targets`:

  `logical` value indicating if the objective supports targets. Values
  indicate that (`TRUE`) targets must be specified, (`NA`) targets are
  optional, and (`FALSE`) must are not used at all.

- `has_weights`:

  `logical` value indicating if the objective supports feature weights.

## Methods

### Public methods

- [`Objective$default_weights()`](#method-Objective-default_weights)

- [`Objective$apply()`](#method-Objective-apply)

- [`Objective$clone()`](#method-Objective-clone)

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

### `Objective$default_weights()`

Specify default value for the feature weights.

#### Usage

    Objective$default_weights()

#### Returns

A `numeric` value.

------------------------------------------------------------------------

### `Objective$apply()`

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

### `Objective$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Objective$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
