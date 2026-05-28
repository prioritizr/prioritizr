# Conservation problem modifier class

This super-class is used to construct
[`Objective`](https://prioritizr.net/reference/Objective-class.md)
[`Penalty`](https://prioritizr.net/reference/Penalty-class.md),
[`Target`](https://prioritizr.net/reference/Target-class.md),
[`Constraint`](https://prioritizr.net/reference/Constraint-class.md),
[`Portfolio`](https://prioritizr.net/reference/Portfolio-class.md),
[`Solver`](https://prioritizr.net/reference/Solver-class.md), and
[`Decision`](https://prioritizr.net/reference/Decision-class.md)
objects. **Only experts should use the fields and methods for this class
directly.**

## See also

Other classes:
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
[`Target-class`](https://prioritizr.net/reference/Target-class.md),
[`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Public fields

- `name`:

  `character` value.

- `data`:

  `list` containing data.

- `internal`:

  `list` containing internal computed values.

- `compressed_formulation`:

  `logical` value indicating if the object is compatible with a
  compressed formulation.

## Methods

### Public methods

- [`ConservationModifier$print()`](#method-ConservationModifier-print)

- [`ConservationModifier$show()`](#method-ConservationModifier-show)

- [`ConservationModifier$repr()`](#method-ConservationModifier-repr)

- [`ConservationModifier$calculate()`](#method-ConservationModifier-calculate)

- [`ConservationModifier$get_data()`](#method-ConservationModifier-get_data)

- [`ConservationModifier$set_data()`](#method-ConservationModifier-set_data)

- [`ConservationModifier$get_internal()`](#method-ConservationModifier-get_internal)

- [`ConservationModifier$set_internal()`](#method-ConservationModifier-set_internal)

- [`ConservationModifier$clone()`](#method-ConservationModifier-clone)

------------------------------------------------------------------------

### `ConservationModifier$print()`

Print information about the object.

#### Usage

    ConservationModifier$print()

#### Returns

None.

------------------------------------------------------------------------

### `ConservationModifier$show()`

Print information about the object.

#### Usage

    ConservationModifier$show()

#### Returns

None.

------------------------------------------------------------------------

### `ConservationModifier$repr()`

Generate a character representation of the object.

#### Usage

    ConservationModifier$repr(compact = TRUE)

#### Arguments

- `compact`:

  `logical` value indicating if the output value should be compact?
  Defaults to `FALSE`.

#### Returns

A `character` value.

------------------------------------------------------------------------

### `ConservationModifier$calculate()`

Perform computations that need to be completed before applying the
object.

#### Usage

    ConservationModifier$calculate(x, y)

#### Arguments

- `x`:

  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  object.

- `y`:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `ConservationModifier$get_data()`

Get values stored in the `data` field.

#### Usage

    ConservationModifier$get_data(x)

#### Arguments

- `x`:

  `character` name of data.

#### Returns

An object. If the `data` field does not contain an object associated
with `x`, then a
[`new_waiver()`](https://prioritizr.net/reference/new_waiver.md) object
is returned.

------------------------------------------------------------------------

### `ConservationModifier$set_data()`

Set values stored in the `data` field. Note that this method will
overwrite existing data.

#### Usage

    ConservationModifier$set_data(x, value)

#### Arguments

- `x`:

  `character` name of data.

- `value`:

  Object to store.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `ConservationModifier$get_internal()`

Get values stored in the `internal` field.

#### Usage

    ConservationModifier$get_internal(x)

#### Arguments

- `x`:

  `character` name of data.

#### Returns

An object. If the `internal` field does not contain an object associated
with `x`, then a
[`new_waiver()`](https://prioritizr.net/reference/new_waiver.md) object
is returned.

------------------------------------------------------------------------

### `ConservationModifier$set_internal()`

Set values stored in the `internal` field. Note that this method will
overwrite existing data.

#### Usage

    ConservationModifier$set_internal(x, value)

#### Arguments

- `x`:

  `character` name of data.

- `value`:

  Object to store.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `ConservationModifier$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConservationModifier$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
