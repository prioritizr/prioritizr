# Conservation problem class

This class is used to represent conservation planning problems. It
stores the data (e.g., planning units, and features) and mathematical
formulation (e.g., the objective, constraints, and other design
criteria) needed to generate prioritizations. Most users should use
[`problem()`](https://prioritizr.net/reference/problem.md) to generate
new conservation problem objects, and the functions distributed with the
package to interact with them (e.g.,
[`number_of_features()`](https://prioritizr.net/reference/number_of_features.md),
[`number_of_planning_units()`](https://prioritizr.net/reference/number_of_planning_units.md)).
**Only experts should use the fields and methods for this class
directly.**

## See also

Other classes:
[`ConservationModifier-class`](https://prioritizr.net/reference/ConservationModifier-class.md),
[`Constraint-class`](https://prioritizr.net/reference/Constraint-class.md),
[`Decision-class`](https://prioritizr.net/reference/Decision-class.md),
[`Objective-class`](https://prioritizr.net/reference/Objective-class.md),
[`OptimizationProblem-class`](https://prioritizr.net/reference/OptimizationProblem-class.md),
[`Penalty-class`](https://prioritizr.net/reference/Penalty-class.md),
[`Portfolio-class`](https://prioritizr.net/reference/Portfolio-class.md),
[`Solver-class`](https://prioritizr.net/reference/Solver-class.md),
[`Target-class`](https://prioritizr.net/reference/Target-class.md),
[`TargetMethod-class`](https://prioritizr.net/reference/TargetMethod-class.md),
[`Weight-class`](https://prioritizr.net/reference/Weight-class.md)

## Public fields

- `data`:

  `list` containing data (e.g., planning units, costs).

- `defaults`:

  `list` indicating if other fields contain defaults.

- `objective`:

  [`Objective`](https://prioritizr.net/reference/Objective-class.md)
  object specifying the objective function for the problem formulation.

- `decisions`:

  [`Decision`](https://prioritizr.net/reference/Decision-class.md)
  object specifying the decision types for the problem formulation.

- `targets`:

  [`Target`](https://prioritizr.net/reference/Target-class.md) object
  specifying the representation targets for the problem formulation.

- `weights`:

  [`Weight`](https://prioritizr.net/reference/Weight-class.md) object
  specifying the feature weights for the problem formulation.

- `constraints`:

  `list` containing
  [`Constraint`](https://prioritizr.net/reference/Constraint-class.md)
  objects that specify constraints for the problem formulation.

- `penalties`:

  `list` containing
  [`Penalty`](https://prioritizr.net/reference/Penalty-class.md) objects
  that specify penalties for the problem formulation.

- `portfolio`:

  [`Portfolio`](https://prioritizr.net/reference/Portfolio-class.md)
  object specifying the approach for generating multiple solutions.

- `solver`:

  [`Solver`](https://prioritizr.net/reference/Solver-class.md) object
  specifying the solver for generating solutions.

## Methods

### Public methods

- [`ConservationProblem$new()`](#method-ConservationProblem-new)

- [`ConservationProblem$summary()`](#method-ConservationProblem-summary)

- [`ConservationProblem$print()`](#method-ConservationProblem-print)

- [`ConservationProblem$show()`](#method-ConservationProblem-show)

- [`ConservationProblem$repr()`](#method-ConservationProblem-repr)

- [`ConservationProblem$get_data()`](#method-ConservationProblem-get_data)

- [`ConservationProblem$set_data()`](#method-ConservationProblem-set_data)

- [`ConservationProblem$number_of_planning_units()`](#method-ConservationProblem-number_of_planning_units)

- [`ConservationProblem$is_ids_equivalent_to_indices()`](#method-ConservationProblem-is_ids_equivalent_to_indices)

- [`ConservationProblem$planning_unit_indices()`](#method-ConservationProblem-planning_unit_indices)

- [`ConservationProblem$total_unit_ids()`](#method-ConservationProblem-total_unit_ids)

- [`ConservationProblem$convert_total_unit_ids_to_indices()`](#method-ConservationProblem-convert_total_unit_ids_to_indices)

- [`ConservationProblem$planning_unit_indices_with_finite_costs()`](#method-ConservationProblem-planning_unit_indices_with_finite_costs)

- [`ConservationProblem$set_planning_unit_indices_with_finite_costs()`](#method-ConservationProblem-set_planning_unit_indices_with_finite_costs)

- [`ConservationProblem$number_of_total_units()`](#method-ConservationProblem-number_of_total_units)

- [`ConservationProblem$planning_unit_costs()`](#method-ConservationProblem-planning_unit_costs)

- [`ConservationProblem$planning_unit_class()`](#method-ConservationProblem-planning_unit_class)

- [`ConservationProblem$set_planning_unit_costs()`](#method-ConservationProblem-set_planning_unit_costs)

- [`ConservationProblem$number_of_features()`](#method-ConservationProblem-number_of_features)

- [`ConservationProblem$feature_names()`](#method-ConservationProblem-feature_names)

- [`ConservationProblem$feature_abundances_in_planning_units()`](#method-ConservationProblem-feature_abundances_in_planning_units)

- [`ConservationProblem$set_feature_abundances_in_planning_units()`](#method-ConservationProblem-set_feature_abundances_in_planning_units)

- [`ConservationProblem$feature_positive_abundances_in_planning_units()`](#method-ConservationProblem-feature_positive_abundances_in_planning_units)

- [`ConservationProblem$set_feature_positive_abundances_in_planning_units()`](#method-ConservationProblem-set_feature_positive_abundances_in_planning_units)

- [`ConservationProblem$feature_abundances_in_total_units()`](#method-ConservationProblem-feature_abundances_in_total_units)

- [`ConservationProblem$feature_units()`](#method-ConservationProblem-feature_units)

- [`ConservationProblem$feature_abundances_km2_in_total_units()`](#method-ConservationProblem-feature_abundances_km2_in_total_units)

- [`ConservationProblem$set_feature_abundances_km2_in_total_units()`](#method-ConservationProblem-set_feature_abundances_km2_in_total_units)

- [`ConservationProblem$feature_targets()`](#method-ConservationProblem-feature_targets)

- [`ConservationProblem$feature_weights()`](#method-ConservationProblem-feature_weights)

- [`ConservationProblem$has_negative_feature_data()`](#method-ConservationProblem-has_negative_feature_data)

- [`ConservationProblem$number_of_zones()`](#method-ConservationProblem-number_of_zones)

- [`ConservationProblem$zone_names()`](#method-ConservationProblem-zone_names)

- [`ConservationProblem$add_portfolio()`](#method-ConservationProblem-add_portfolio)

- [`ConservationProblem$add_solver()`](#method-ConservationProblem-add_solver)

- [`ConservationProblem$add_targets()`](#method-ConservationProblem-add_targets)

- [`ConservationProblem$add_weights()`](#method-ConservationProblem-add_weights)

- [`ConservationProblem$add_objective()`](#method-ConservationProblem-add_objective)

- [`ConservationProblem$add_decisions()`](#method-ConservationProblem-add_decisions)

- [`ConservationProblem$add_constraint()`](#method-ConservationProblem-add_constraint)

- [`ConservationProblem$add_penalty()`](#method-ConservationProblem-add_penalty)

- [`ConservationProblem$remove_all_penalties()`](#method-ConservationProblem-remove_all_penalties)

- [`ConservationProblem$clone()`](#method-ConservationProblem-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new conservation problem object.

#### Usage

    ConservationProblem$new(data = list())

#### Arguments

- `data`:

  `list` containing data

#### Returns

A new `ConservationProblem` object.

------------------------------------------------------------------------

### Method [`summary()`](https://rspatial.github.io/terra/reference/summary.html)

Print extended information about the object.

#### Usage

    ConservationProblem$summary()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print concise information about the object.

#### Usage

    ConservationProblem$print()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method [`show()`](https://prioritizr.net/reference/show.md)

Display concise information about the object.

#### Usage

    ConservationProblem$show()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `repr()`

Generate a character representation of the object.

#### Usage

    ConservationProblem$repr()

#### Returns

A `character` value.

------------------------------------------------------------------------

### Method `get_data()`

Get values stored in the `data` field.

#### Usage

    ConservationProblem$get_data(x)

#### Arguments

- `x`:

  `character` name of data.

#### Returns

An object. If the `data` field does not contain an object associated
with the argument to `x`, then a
[`new_waiver()`](https://prioritizr.net/reference/new_waiver.md) object
is returned.

------------------------------------------------------------------------

### Method `set_data()`

Set values stored in the `data` field. Note that this method will
overwrite existing data.

#### Usage

    ConservationProblem$set_data(x, value)

#### Arguments

- `x`:

  `character` name of data.

- `value`:

  Object to store.

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method [`number_of_planning_units()`](https://prioritizr.net/reference/number_of_planning_units.md)

Obtain the number of planning units. The planning units correspond to
elements in the cost data (e.g., indices, rows, geometries, cells) that
have finite values in at least one zone. In other words, planning unit
are elements in the cost data that do not have missing (`NA`) values in
every zone.

#### Usage

    ConservationProblem$number_of_planning_units()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### Method `is_ids_equivalent_to_indices()`

Check if planning unit identifiers are equivalent to the planning unit
indices? Only `FALSE` if the planning units are `data.frame` format.

#### Usage

    ConservationProblem$is_ids_equivalent_to_indices()

#### Returns

A `logical` value.

------------------------------------------------------------------------

### Method `planning_unit_indices()`

Obtain the planning unit indices.

#### Usage

    ConservationProblem$planning_unit_indices()

#### Returns

An `integer` vector.

------------------------------------------------------------------------

### Method `total_unit_ids()`

Obtain the total unit identifiers.

#### Usage

    ConservationProblem$total_unit_ids()

#### Returns

An `integer` vector.

------------------------------------------------------------------------

### Method `convert_total_unit_ids_to_indices()`

Convert total unit identifiers to indices.

#### Usage

    ConservationProblem$convert_total_unit_ids_to_indices(ids)

#### Arguments

- `ids`:

  `integer` vector with planning unit identifiers.

#### Returns

An `integer` vector.

------------------------------------------------------------------------

### Method `planning_unit_indices_with_finite_costs()`

Obtain the planning unit indices that are associated with finite cost
values.

#### Usage

    ConservationProblem$planning_unit_indices_with_finite_costs()

#### Returns

A `list` of `integer` vectors. Each `list` element corresponds to a
different zone.

------------------------------------------------------------------------

### Method `set_planning_unit_indices_with_finite_costs()`

Perform calculations to cache the planning unit indices that are
associated with finite cost values.

#### Usage

    ConservationProblem$set_planning_unit_indices_with_finite_costs()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method [`number_of_total_units()`](https://prioritizr.net/reference/number_of_total_units.md)

Obtain the number of total units. The total units include all elements
in the cost data (e.g., indices, rows, geometries, cells), including
those with missing (`NA`) values.

#### Usage

    ConservationProblem$number_of_total_units()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### Method `planning_unit_costs()`

Obtain the planning unit costs.

#### Usage

    ConservationProblem$planning_unit_costs()

#### Returns

A `numeric` matrix.

------------------------------------------------------------------------

### Method `planning_unit_class()`

Get planning unit class.

#### Usage

    ConservationProblem$planning_unit_class()

#### Returns

A `character` value.

------------------------------------------------------------------------

### Method `set_planning_unit_costs()`

Perform calculations to cache the planning unit costs.

#### Usage

    ConservationProblem$set_planning_unit_costs()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method [`number_of_features()`](https://prioritizr.net/reference/number_of_features.md)

Obtain the number of features.

#### Usage

    ConservationProblem$number_of_features()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### Method [`feature_names()`](https://prioritizr.net/reference/feature_names.md)

Obtain the names of the features.

#### Usage

    ConservationProblem$feature_names()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### Method `feature_abundances_in_planning_units()`

Obtain the abundance of the features in the planning units.

#### Usage

    ConservationProblem$feature_abundances_in_planning_units()

#### Returns

A `numeric` matrix. Each column corresponds to a different zone and each
row corresponds to a different feature.

------------------------------------------------------------------------

### Method `set_feature_abundances_in_planning_units()`

Perform calculations to cache the abundance of the features in the
planning units.

#### Usage

    ConservationProblem$set_feature_abundances_in_planning_units()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `feature_positive_abundances_in_planning_units()`

Obtain the positive abundance of the features in the planning units.
Note that this method, unlike `feature_abundances_in_planning_units`,

#### Usage

    ConservationProblem$feature_positive_abundances_in_planning_units()

#### Returns

A `numeric` matrix. Each column corresponds to a different zone and each
row corresponds to a different feature.

------------------------------------------------------------------------

### Method `set_feature_positive_abundances_in_planning_units()`

Perform calculations to cache the positive abundance of the features in
the planning units.

#### Usage

    ConservationProblem$set_feature_positive_abundances_in_planning_units()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `feature_abundances_in_total_units()`

Obtain the abundance of the features in the total units.

#### Usage

    ConservationProblem$feature_abundances_in_total_units()

#### Returns

A `numeric` matrix. Each column corresponds to a different zone and each
row corresponds to a different feature.

------------------------------------------------------------------------

### Method `feature_units()`

Obtain the units of the features.

#### Usage

    ConservationProblem$feature_units()

#### Returns

A `character` value. Each element corresponds to a different feature.

------------------------------------------------------------------------

### Method `feature_abundances_km2_in_total_units()`

Obtain the abundance of the features in area-based units of km².

#### Usage

    ConservationProblem$feature_abundances_km2_in_total_units()

#### Details

Note that if a feature has missing (`NA`) units then missing values are
returned.

#### Returns

A `numeric` matrix. Each column corresponds to a different zone and each
row corresponds to a different feature.

------------------------------------------------------------------------

### Method `set_feature_abundances_km2_in_total_units()`

Perform calculations to cache the abundance of the features in
area-based units of km².

#### Usage

    ConservationProblem$set_feature_abundances_km2_in_total_units()

#### Returns

Invisible `TRUE`.

------------------------------------------------------------------------

### Method `feature_targets()`

Obtain the representation targets for the features.

#### Usage

    ConservationProblem$feature_targets()

#### Returns

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame.

------------------------------------------------------------------------

### Method `feature_weights()`

Obtain the weights for the features.

#### Usage

    ConservationProblem$feature_weights()

#### Returns

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame.

------------------------------------------------------------------------

### Method `has_negative_feature_data()`

See if the feature data contain any negative values.

#### Usage

    ConservationProblem$has_negative_feature_data()

#### Returns

A `logical` value.

------------------------------------------------------------------------

### Method [`number_of_zones()`](https://prioritizr.net/reference/number_of_zones.md)

Obtain the number of zones.

#### Usage

    ConservationProblem$number_of_zones()

#### Returns

An `integer` value.

------------------------------------------------------------------------

### Method [`zone_names()`](https://prioritizr.net/reference/zone_names.md)

Obtain the zone names.

#### Usage

    ConservationProblem$zone_names()

#### Returns

A `character` vector.

------------------------------------------------------------------------

### Method `add_portfolio()`

Create a new object with a portfolio added to the problem formulation.

#### Usage

    ConservationProblem$add_portfolio(x)

#### Arguments

- `x`:

  [Portfolio](https://prioritizr.net/reference/Portfolio-class.md)
  object.

#### Returns

An updated `ConservationProblem` object.

------------------------------------------------------------------------

### Method `add_solver()`

Create a new object with a solver added to the problem formulation.

#### Usage

    ConservationProblem$add_solver(x)

#### Arguments

- `x`:

  [Solver](https://prioritizr.net/reference/Solver-class.md) object.

#### Returns

An updated `ConservationProblem` object.

------------------------------------------------------------------------

### Method `add_targets()`

Create a new object with targets added to the problem formulation.

#### Usage

    ConservationProblem$add_targets(x)

#### Arguments

- `x`:

  [Target](https://prioritizr.net/reference/Target-class.md) object.

#### Returns

An updated `ConservationProblem` object.

------------------------------------------------------------------------

### Method `add_weights()`

Create a new object with weights added to the problem formulation.

#### Usage

    ConservationProblem$add_weights(x)

#### Arguments

- `x`:

  [Weight](https://prioritizr.net/reference/Weight-class.md) object.

#### Returns

An updated `ConservationProblem` object.

------------------------------------------------------------------------

### Method `add_objective()`

Create a new object with an objective added to the problem formulation.

#### Usage

    ConservationProblem$add_objective(x)

#### Arguments

- `x`:

  [Objective](https://prioritizr.net/reference/Objective-class.md)
  object.

#### Returns

An updated `ConservationProblem` object.

------------------------------------------------------------------------

### Method `add_decisions()`

Create a new object with decisions added to the problem formulation.

#### Usage

    ConservationProblem$add_decisions(x)

#### Arguments

- `x`:

  [Decision](https://prioritizr.net/reference/Decision-class.md) object.

#### Returns

An updated `ConservationProblem` object.

------------------------------------------------------------------------

### Method `add_constraint()`

Create a new object with a constraint added to the problem formulation.

#### Usage

    ConservationProblem$add_constraint(x)

#### Arguments

- `x`:

  [Constraint](https://prioritizr.net/reference/Constraint-class.md)
  object.

#### Returns

An updated `ConservationProblem` object.

------------------------------------------------------------------------

### Method `add_penalty()`

Create a new object with a penalty added to the problem formulation.

#### Usage

    ConservationProblem$add_penalty(x)

#### Arguments

- `x`:

  [Penalty](https://prioritizr.net/reference/Penalty-class.md) object.

#### Returns

An updated `ConservationProblem` object.

------------------------------------------------------------------------

### Method `remove_all_penalties()`

Create a new object without any penalties.

#### Usage

    ConservationProblem$remove_all_penalties(retain = NULL)

#### Arguments

- `retain`:

  `character` vector of classes to retain. Defaults to `NULL` such that
  all penalties are excluded.

#### Returns

An updated `ConservationProblem` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConservationProblem$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
