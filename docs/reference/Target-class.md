# Target class

This class is used to represent targets for optimization. **Only experts
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
[`Solver-class`](https://prioritizr.net/reference/Solver-class.md),
[`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Super class

[`prioritizr::ConservationModifier`](https://prioritizr.net/reference/ConservationModifier-class.md)
-\> `Target`

## Methods

### Public methods

- [`Target$output()`](#method-Target-output)

- [`Target$clone()`](#method-Target-clone)

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

### Method `output()`

Output the targets.

#### Usage

    Target$output()

#### Returns

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Target$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
