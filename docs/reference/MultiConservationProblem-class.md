# Multi-objective conservation problem class

This class is used to represent multi-objective conservation planning
problems. It stores the data (e.g., planning units, and features) and
mathematical formulation (e.g., the objective, constraints, and other
design criteria) needed to generate prioritizations. Most users should
use
[`multi_problem()`](https://prioritizr.net/reference/multi_problem.md)
to generate new multi-objective conservation problem objects, and the
functions distributed with the package to interact with them (e.g.,
[`number_of_features()`](https://prioritizr.net/reference/number_of_features.md),
[`number_of_planning_units()`](https://prioritizr.net/reference/number_of_planning_units.md)).
**Only experts should use the fields and methods for this class
directly.**

## See also

Other classes:
[`ConservationModifier-class`](https://prioritizr.net/reference/ConservationModifier-class.md),
[`ConservationProblem-class`](https://prioritizr.net/reference/ConservationProblem-class.md),
[`Constraint-class`](https://prioritizr.net/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.net/reference/Decision-class.md),
[`MultiObjApproach-class`](https://prioritizr.net/reference/MultiObjApproach-class.md),
[`Objective-class`](https://prioritizr.net/reference/Objective-class.md),
[`OptimizationProblem-class`](https://prioritizr.net/reference/OptimizationProblem-class.md),
[`Penalty-class`](https://prioritizr.net/reference/Penalty-class.md),
[`Portfolio-class`](https://prioritizr.net/reference/Portfolio-class.md),
[`Solver-class`](https://prioritizr.net/reference/Solver-class.md),
[`Target-class`](https://prioritizr.net/reference/Target-class.md),
[`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Public fields

- `problems`:

  `list` containing
  [`ConservationProblem`](https://prioritizr.net/reference/ConservationProblem-class.md)
  objects.

- `defaults`:

  `list` indicating if other fields contain defaults.

- `approach`:

  [`MultiObjApproach`](https://prioritizr.net/reference/MultiObjApproach-class.md)
  object for specifying the multi-objective optimization approach.

- `solver`:

  [`Solver`](https://prioritizr.net/reference/Solver-class.md) object
  specifying the solver for generating solutions.

## Methods

### Public methods

- [`MultiConservationProblem$new()`](#method-MultiConservationProblem-initialize)

- [`MultiConservationProblem$summary()`](#method-MultiConservationProblem-summary)

- [`MultiConservationProblem$print()`](#method-MultiConservationProblem-print)

- [`MultiConservationProblem$show()`](#method-MultiConservationProblem-show)

- [`MultiConservationProblem$repr()`](#method-MultiConservationProblem-repr)

- [`MultiConservationProblem$number_of_planning_units()`](#method-MultiConservationProblem-number_of_planning_units)

- [`MultiConservationProblem$is_ids_equivalent_to_indices()`](#method-MultiConservationProblem-is_ids_equivalent_to_indices)

- [`MultiConservationProblem$planning_unit_indices()`](#method-MultiConservationProblem-planning_unit_indices)

- [`MultiConservationProblem$total_unit_ids()`](#method-MultiConservationProblem-total_unit_ids)

- [`MultiConservationProblem$convert_total_unit_ids_to_indices()`](#method-MultiConservationProblem-convert_total_unit_ids_to_indices)

- [`MultiConservationProblem$planning_unit_indices_with_finite_costs()`](#method-MultiConservationProblem-planning_unit_indices_with_finite_costs)

- [`MultiConservationProblem$number_of_total_units()`](#method-MultiConservationProblem-number_of_total_units)

- [`MultiConservationProblem$planning_unit_class()`](#method-MultiConservationProblem-planning_unit_class)

- [`MultiConservationProblem$number_of_features()`](#method-MultiConservationProblem-number_of_features)

- [`MultiConservationProblem$feature_names()`](#method-MultiConservationProblem-feature_names)

- [`MultiConservationProblem$number_of_problems()`](#method-MultiConservationProblem-number_of_problems)

- [`MultiConservationProblem$problem_names()`](#method-MultiConservationProblem-problem_names)

- [`MultiConservationProblem$number_of_zones()`](#method-MultiConservationProblem-number_of_zones)

- [`MultiConservationProblem$zone_names()`](#method-MultiConservationProblem-zone_names)

- [`MultiConservationProblem$add_approach()`](#method-MultiConservationProblem-add_approach)

- [`MultiConservationProblem$add_solver()`](#method-MultiConservationProblem-add_solver)

- [`MultiConservationProblem$clone()`](#method-MultiConservationProblem-clone)

------------------------------------------------------------------------

### `MultiConservationProblem$new()`

Create a new multi-objective conservation problem object.

#### Usage

    MultiConservationProblem$new(problems)

#### Arguments

- `problems`:

  `list` containing
  [`ConservationProblem`](https://prioritizr.net/reference/ConservationProblem-class.md)
  objects.

#### Returns

A new `MultiConservationProblem` object.

------------------------------------------------------------------------

### `MultiConservationProblem$summary()`

Print extended information about the object.

#### Usage

    MultiConservationProblem$summary()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `MultiConservationProblem$print()`

Print concise information about the object.

#### Usage

    MultiConservationProblem$print()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `MultiConservationProblem$show()`

Display concise information about the object.

#### Usage

    MultiConservationProblem$show()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `MultiConservationProblem$repr()`

Generate a character representation of the object.

#### Usage

    MultiConservationProblem$repr()

#### Returns

A `character` value.

------------------------------------------------------------------------

### `MultiConservationProblem$number_of_planning_units()`

Obtain the number of planning units. The planning units correspond to
elements in the cost data (e.g., indices, rows, geometries, cells) that
have finite values in at least one zone. In other words, planning unit
are elements in the cost data that do not have missing (`NA`) values in
every zone.

#### Usage

    MultiConservationProblem$number_of_planning_units()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `MultiConservationProblem$is_ids_equivalent_to_indices()`

Check if planning unit identifiers are equivalent to the planning unit
indices? Only `FALSE` if the planning units are `data.frame` format.

#### Usage

    MultiConservationProblem$is_ids_equivalent_to_indices()

#### Returns

A `logical` value.

------------------------------------------------------------------------

### `MultiConservationProblem$planning_unit_indices()`

Obtain the planning unit indices.

#### Usage

    MultiConservationProblem$planning_unit_indices()

#### Returns

An `integer` vector.

------------------------------------------------------------------------

### `MultiConservationProblem$total_unit_ids()`

Obtain the total unit identifiers.

#### Usage

    MultiConservationProblem$total_unit_ids()

#### Returns

An `integer` vector.

------------------------------------------------------------------------

### `MultiConservationProblem$convert_total_unit_ids_to_indices()`

Convert total unit identifiers to indices.

#### Usage

    MultiConservationProblem$convert_total_unit_ids_to_indices(ids)

#### Arguments

- `ids`:

  `integer` vector with planning unit identifiers.

#### Returns

An `integer` vector.

------------------------------------------------------------------------

### `MultiConservationProblem$planning_unit_indices_with_finite_costs()`

Obtain the planning unit indices that are associated with finite cost
values.

#### Usage

    MultiConservationProblem$planning_unit_indices_with_finite_costs()

#### Returns

A `list` of `integer` vectors. Each `list` element corresponds to a
different zone.

------------------------------------------------------------------------

### `MultiConservationProblem$number_of_total_units()`

Obtain the number of total units. The total units include all elements
in the cost data (e.g., indices, rows, geometries, cells), including
those with missing (`NA`) values.

#### Usage

    MultiConservationProblem$number_of_total_units()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `MultiConservationProblem$planning_unit_class()`

Get planning unit class.

#### Usage

    MultiConservationProblem$planning_unit_class()

#### Returns

A `character` value.

------------------------------------------------------------------------

### `MultiConservationProblem$number_of_features()`

Obtain the number of features.

#### Usage

    MultiConservationProblem$number_of_features()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `MultiConservationProblem$feature_names()`

Obtain the names of the features.

#### Usage

    MultiConservationProblem$feature_names()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### `MultiConservationProblem$number_of_problems()`

Obtain the number of problems.

#### Usage

    MultiConservationProblem$number_of_problems()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `MultiConservationProblem$problem_names()`

Obtain the names of the problems.

#### Usage

    MultiConservationProblem$problem_names()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### `MultiConservationProblem$number_of_zones()`

Obtain the number of zones.

#### Usage

    MultiConservationProblem$number_of_zones()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `MultiConservationProblem$zone_names()`

Obtain the zone names.

#### Usage

    MultiConservationProblem$zone_names()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### `MultiConservationProblem$add_approach()`

Create a new object with an approach added to the problem formulation.

#### Usage

    MultiConservationProblem$add_approach(x)

#### Arguments

- `x`:

  [MultiObjApproach](https://prioritizr.net/reference/MultiObjApproach-class.md)
  object.

#### Returns

An updated `MultiConservationProblem` object.

------------------------------------------------------------------------

### `MultiConservationProblem$add_solver()`

Create a new object with a solver added to the problem formulation.

#### Usage

    MultiConservationProblem$add_solver(x)

#### Arguments

- `x`:

  [Solver](https://prioritizr.net/reference/Solver-class.md) object.

#### Returns

An updated `MultiConservationProblem` object.

------------------------------------------------------------------------

### `MultiConservationProblem$clone()`

The objects of this class are cloneable with this method.

#### Usage

    MultiConservationProblem$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
