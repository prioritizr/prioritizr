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
  object for specifying the multi-objective optimization appraoch.

- `solver`:

  [`Solver`](https://prioritizr.net/reference/Solver-class.md) object
  specifying the solver for generating solutions.

## Methods

### Public methods

- [`MultiObjConservationProblem$new()`](#method-MultiObjConservationProblem-initialize)

- [`MultiObjConservationProblem$summary()`](#method-MultiObjConservationProblem-summary)

- [`MultiObjConservationProblem$print()`](#method-MultiObjConservationProblem-print)

- [`MultiObjConservationProblem$show()`](#method-MultiObjConservationProblem-show)

- [`MultiObjConservationProblem$repr()`](#method-MultiObjConservationProblem-repr)

- [`MultiObjConservationProblem$number_of_planning_units()`](#method-MultiObjConservationProblem-number_of_planning_units)

- [`MultiObjConservationProblem$is_ids_equivalent_to_indices()`](#method-MultiObjConservationProblem-is_ids_equivalent_to_indices)

- [`MultiObjConservationProblem$planning_unit_indices()`](#method-MultiObjConservationProblem-planning_unit_indices)

- [`MultiObjConservationProblem$total_unit_ids()`](#method-MultiObjConservationProblem-total_unit_ids)

- [`MultiObjConservationProblem$convert_total_unit_ids_to_indices()`](#method-MultiObjConservationProblem-convert_total_unit_ids_to_indices)

- [`MultiObjConservationProblem$planning_unit_indices_with_finite_costs()`](#method-MultiObjConservationProblem-planning_unit_indices_with_finite_costs)

- [`MultiObjConservationProblem$number_of_total_units()`](#method-MultiObjConservationProblem-number_of_total_units)

- [`MultiObjConservationProblem$planning_unit_class()`](#method-MultiObjConservationProblem-planning_unit_class)

- [`MultiObjConservationProblem$number_of_features()`](#method-MultiObjConservationProblem-number_of_features)

- [`MultiObjConservationProblem$feature_names()`](#method-MultiObjConservationProblem-feature_names)

- [`MultiObjConservationProblem$number_of_problems()`](#method-MultiObjConservationProblem-number_of_problems)

- [`MultiObjConservationProblem$problem_names()`](#method-MultiObjConservationProblem-problem_names)

- [`MultiObjConservationProblem$number_of_zones()`](#method-MultiObjConservationProblem-number_of_zones)

- [`MultiObjConservationProblem$zone_names()`](#method-MultiObjConservationProblem-zone_names)

- [`MultiObjConservationProblem$add_approach()`](#method-MultiObjConservationProblem-add_approach)

- [`MultiObjConservationProblem$add_solver()`](#method-MultiObjConservationProblem-add_solver)

- [`MultiObjConservationProblem$clone()`](#method-MultiObjConservationProblem-clone)

------------------------------------------------------------------------

### `MultiObjConservationProblem$new()`

Create a new multi-objective conservation problem object.

#### Usage

    MultiObjConservationProblem$new(problems)

#### Arguments

- `problems`:

  `list` containing
  [`ConservationProblem`](https://prioritizr.net/reference/ConservationProblem-class.md)
  objects.

#### Returns

A new `MultiObjConservationProblem` object.

------------------------------------------------------------------------

### `MultiObjConservationProblem$summary()`

Print extended information about the object.

#### Usage

    MultiObjConservationProblem$summary()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `MultiObjConservationProblem$print()`

Print concise information about the object.

#### Usage

    MultiObjConservationProblem$print()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `MultiObjConservationProblem$show()`

Display concise information about the object.

#### Usage

    MultiObjConservationProblem$show()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### `MultiObjConservationProblem$repr()`

Generate a character representation of the object.

#### Usage

    MultiObjConservationProblem$repr()

#### Returns

A `character` value.

------------------------------------------------------------------------

### `MultiObjConservationProblem$number_of_planning_units()`

Obtain the number of planning units. The planning units correspond to
elements in the cost data (e.g., indices, rows, geometries, cells) that
have finite values in at least one zone. In other words, planning unit
are elements in the cost data that do not have missing (`NA`) values in
every zone.

#### Usage

    MultiObjConservationProblem$number_of_planning_units()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `MultiObjConservationProblem$is_ids_equivalent_to_indices()`

Check if planning unit identifiers are equivalent to the planning unit
indices? Only `FALSE` if the planning units are `data.frame` format.

#### Usage

    MultiObjConservationProblem$is_ids_equivalent_to_indices()

#### Returns

A `logical` value.

------------------------------------------------------------------------

### `MultiObjConservationProblem$planning_unit_indices()`

Obtain the planning unit indices.

#### Usage

    MultiObjConservationProblem$planning_unit_indices()

#### Returns

An `integer` vector.

------------------------------------------------------------------------

### `MultiObjConservationProblem$total_unit_ids()`

Obtain the total unit identifiers.

#### Usage

    MultiObjConservationProblem$total_unit_ids()

#### Returns

An `integer` vector.

------------------------------------------------------------------------

### `MultiObjConservationProblem$convert_total_unit_ids_to_indices()`

Convert total unit identifiers to indices.

#### Usage

    MultiObjConservationProblem$convert_total_unit_ids_to_indices(ids)

#### Arguments

- `ids`:

  `integer` vector with planning unit identifiers.

#### Returns

An `integer` vector.

------------------------------------------------------------------------

### `MultiObjConservationProblem$planning_unit_indices_with_finite_costs()`

Obtain the planning unit indices that are associated with finite cost
values.

#### Usage

    MultiObjConservationProblem$planning_unit_indices_with_finite_costs()

#### Returns

A `list` of `integer` vectors. Each `list` element corresponds to a
different zone.

------------------------------------------------------------------------

### `MultiObjConservationProblem$number_of_total_units()`

Obtain the number of total units. The total units include all elements
in the cost data (e.g., indices, rows, geometries, cells), including
those with missing (`NA`) values.

#### Usage

    MultiObjConservationProblem$number_of_total_units()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `MultiObjConservationProblem$planning_unit_class()`

Get planning unit class.

#### Usage

    MultiObjConservationProblem$planning_unit_class()

#### Returns

A `character` value.

------------------------------------------------------------------------

### `MultiObjConservationProblem$number_of_features()`

Obtain the number of features.

#### Usage

    MultiObjConservationProblem$number_of_features()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `MultiObjConservationProblem$feature_names()`

Obtain the names of the features.

#### Usage

    MultiObjConservationProblem$feature_names()

#### Returns

A `list` of `character` vectors.

------------------------------------------------------------------------

### `MultiObjConservationProblem$number_of_problems()`

Obtain the number of problems.

#### Usage

    MultiObjConservationProblem$number_of_problems()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `MultiObjConservationProblem$problem_names()`

Obtain the names of the problems.

#### Usage

    MultiObjConservationProblem$problem_names()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### `MultiObjConservationProblem$number_of_zones()`

Obtain the number of zones.

#### Usage

    MultiObjConservationProblem$number_of_zones()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### `MultiObjConservationProblem$zone_names()`

Obtain the zone names.

#### Usage

    MultiObjConservationProblem$zone_names()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### `MultiObjConservationProblem$add_approach()`

Create a new object with an approach added to the problem formulation.

#### Usage

    MultiObjConservationProblem$add_approach(x)

#### Arguments

- `x`:

  [MultiObjApproach](https://prioritizr.net/reference/MultiObjApproach-class.md)
  object.

#### Returns

An updated `MultiObjConservationProblem` object.

------------------------------------------------------------------------

### `MultiObjConservationProblem$add_solver()`

Create a new object with a solver added to the problem formulation.

#### Usage

    MultiObjConservationProblem$add_solver(x)

#### Arguments

- `x`:

  [Solver](https://prioritizr.net/reference/Solver-class.md) object.

#### Returns

An updated `MultiObjConservationProblem` object.

------------------------------------------------------------------------

### `MultiObjConservationProblem$clone()`

The objects of this class are cloneable with this method.

#### Usage

    MultiObjConservationProblem$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
