# Target setting method class

This class is used to represent methods for setting targets. **Only
experts should use the fields and methods for this class directly.**

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
[`Solver-class`](https://prioritizr.net/reference/Solver-class.md),
[`Target-class`](https://prioritizr.net/reference/Target-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Public fields

- `name`:

  `character` value with name of method.

- `type`:

  `character` value denoting the target type.

- `fun`:

  `function` for calculating targets.

- `args`:

  `list` containing arguments.

- `frame`:

  defused \`call for generating error messages.

## Methods

### Public methods

- [`TargetMethod$new()`](#method-TargetMethod-new)

- [`TargetMethod$print()`](#method-TargetMethod-print)

- [`TargetMethod$calculate_targets()`](#method-TargetMethod-calculate_targets)

- [`TargetMethod$calculate_targets_km2()`](#method-TargetMethod-calculate_targets_km2)

- [`TargetMethod$calculate_relative_targets()`](#method-TargetMethod-calculate_relative_targets)

- [`TargetMethod$calculate_absolute_targets()`](#method-TargetMethod-calculate_absolute_targets)

- [`TargetMethod$clone()`](#method-TargetMethod-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initialize new object.

#### Usage

    TargetMethod$new(name, type, fun, args, frame)

#### Arguments

- `name`:

  `character` value with name of method.

- `type`:

  `character` value denoting the target type. Available options include
  `"relative"` and `"absolute"`.

- `fun`:

  `function` for calculating targets.

- `args`:

  `list` containing arguments.

- `frame`:

  defused \`call for generating error messages.

#### Returns

A new `Method` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    TargetMethod$print(...)

#### Arguments

- `...`:

  not used.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `calculate_targets()`

Calculate targets expressed in the type of units defined for the method
(per `$type`).

#### Usage

    TargetMethod$calculate_targets(x, features, call = NULL)

#### Arguments

- `x`:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- `features`:

  `integer` feature indices.

- `call`:

  `NULL` or calling environment.

#### Returns

A `numeric` vector with target values.

------------------------------------------------------------------------

### Method `calculate_targets_km2()`

Calculate targets as km².

#### Usage

    TargetMethod$calculate_targets_km2(x, features, call = NULL)

#### Arguments

- `x`:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- `features`:

  `integer` feature indices.

- `call`:

  `NULL` or calling environment.

#### Returns

A `numeric` vector with target values expressed in km².

------------------------------------------------------------------------

### Method `calculate_relative_targets()`

Calculate targets as km².

#### Usage

    TargetMethod$calculate_relative_targets(x, features, call = NULL)

#### Arguments

- `x`:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- `features`:

  `integer` feature indices.

- `call`:

  `NULL` or calling environment.

#### Returns

A `numeric` vector with target values expressed as relative units.

------------------------------------------------------------------------

### Method `calculate_absolute_targets()`

Calculate targets expressed as absolute units.

#### Usage

    TargetMethod$calculate_absolute_targets(x, features, call = NULL)

#### Arguments

- `x`:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- `features`:

  `integer` feature indices.

- `call`:

  `NULL` or calling environment.

#### Returns

A `numeric` vector with target values expressed as absolute units.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TargetMethod$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
