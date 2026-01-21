# Changelog

## prioritizr 8.1.0.2

- Update publication record.

## prioritizr 8.1.0.1

### Minor improvements and bug fixes

- Update
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
  to be compatible with Gurobi version 13
  ([\#389](https://github.com/prioritizr/prioritizr/issues/389)).
- Standardize procedures for sanitizing solver outputs to provide more
  consistent behavior from different solvers.

### Documentation

- Update README with video and materials for the *prioritizr* workshop
  as part of the Statistical Methods Webinar series by the Ecological
  Forecasting Initiative and ESA Statistical Ecology Section.
- Update publication record.

## prioritizr 8.1.0

CRAN release: 2025-11-10

### Notice

- CRAN release.

### New features

- New
  [`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md)
  function for adding targets to a conservation planning problem based
  on a target setting method
  ([\#377](https://github.com/prioritizr/prioritizr/issues/377)). In
  particular, the following functions can be used in conjunction with
  this function to specify target setting methods:
  [`spec_absolute_targets()`](https://prioritizr.net/reference/spec_absolute_targets.md),
  [`spec_area_targets()`](https://prioritizr.net/reference/spec_area_targets.md),
  [`spec_interp_absolute_targets()`](https://prioritizr.net/reference/spec_interp_absolute_targets.md),
  [`spec_interp_area_targets()`](https://prioritizr.net/reference/spec_interp_area_targets.md)
  [`spec_jung_targets()`](https://prioritizr.net/reference/spec_jung_targets.md),
  [`spec_max_targets()`](https://prioritizr.net/reference/spec_max_targets.md),
  [`spec_min_targets()`](https://prioritizr.net/reference/spec_min_targets.md),
  [`spec_polak_targets()`](https://prioritizr.net/reference/spec_polak_targets.md),
  [`spec_pop_size_targets()`](https://prioritizr.net/reference/spec_pop_size_targets.md),
  [`spec_relative_targets()`](https://prioritizr.net/reference/spec_relative_targets.md),
  [`spec_rl_ecosystem_targets()`](https://prioritizr.net/reference/spec_rl_ecosystem_targets.md),
  [`spec_rl_species_targets()`](https://prioritizr.net/reference/spec_rl_species_targets.md),
  [`spec_rodrigues_targets()`](https://prioritizr.net/reference/spec_rodrigues_targets.md),
  [`spec_rule_targets()`](https://prioritizr.net/reference/spec_rule_targets.md),
  [`spec_ward_targets()`](https://prioritizr.net/reference/spec_ward_targets.md),
  [`spec_watson_targets()`](https://prioritizr.net/reference/spec_watson_targets.md),
  and
  [`spec_wilson_targets()`](https://prioritizr.net/reference/spec_wilson_targets.md).
- New
  [`add_group_targets()`](https://prioritizr.net/reference/add_group_targets.md)
  function for adding targets to a conservation planning problem based
  on feature groups. This function is provided as a convenient
  alternative to the
  [`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md)
  function. With this function, features can be organized into groups
  and then have their targets calculated based on the method specified
  for their group.
- New
  [`linear_interpolation()`](https://prioritizr.net/reference/linear_interpolation.md)
  function for linearly interpolating values.
- New [`as_km2()`](https://prioritizr.net/reference/as_km2.md) and
  [`as_per_km2()`](https://prioritizr.net/reference/as_per_km2.md)
  functions to help with area-based calcultions.
- Many of the internal functions used for parameter and data validation
  can now be used by other packages that depend on the *prioritizr*
  package (e.g., `assert()`, `as_Matrix()`, `all_binary()`,
  `all_positive()`). The idea here is that people developing packages
  that build on the *prioritizr* package can use these functions to
  streamline their developmental efforts, while helping to avoid reverse
  dependency issues. To use these functions in your own package, you can
  make a local copy of the desired *prioritizr* functions in your
  package (i.e., a process known as code vendoring). In particular, you
  can use the
  [`usethis::use_standalone()`](https://usethis.r-lib.org/reference/use_standalone.html)
  function to automatically make a copy of *prioritizr* functions from
  the *prioritizr* online code repository. For example,
  `usethis::use_standalone("prioritizr/prioritizr", file = "standalone-cli.R")`
  can be used to make a copy of the `standalone-cli.R` file in the
  *prioritizr* source code. Note that all files in the *prioritizr* code
  repository that begin with `"standalone-"` can be copied with the
  [`usethis::use_standalone()`](https://usethis.r-lib.org/reference/use_standalone.html)
  function.

### Major changes

- The
  [`add_loglinear_targets()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function has been deprecated. For similar functionality, see the new
  [`spec_interp_absolute_targets()`](https://prioritizr.net/reference/spec_interp_absolute_targets.md)
  function.
- The
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  function will now catch issues where the same planning unit (or
  planning units) has been both locked in and locked out
  ([\#386](https://github.com/prioritizr/prioritizr/issues/386)). Thanks
  to Jason Everett ([@jaseeverett](https://github.com/jaseeverett)) for
  the suggestion.
- The
  [`add_feature_weights()`](https://prioritizr.net/reference/add_feature_weights.md)
  function can only be used once with a
  [`problem()`](https://prioritizr.net/reference/problem.md), and
  attempting to add multiple weights will over-write previously
  specified weights (similar to how targets are handled).

### Minor improvements and bug fixes

- Update [`print()`](https://rdrr.io/r/base/print.html) method for
  [`problem()`](https://prioritizr.net/reference/problem.md) objects to
  display a more useful number of digits for floating point numbers.
- Update
  [`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md)
  so that an alternative formulation can be used for the optimization
  problem
  ([\#369](https://github.com/prioritizr/prioritizr/issues/369)). This
  alternative formulation may be useful when conservation planning
  problems are taking a long time to solve. Note that the default
  behavior of the function is to use the same formulation as in previous
  versions of the package.
- Update [`solve()`](https://prioritizr.net/reference/solve.md) function
  to provide information on the objective bound. This represents the
  best estimate of the optimal objective value during optimization.
  Given a solution `x`, this information can be accessed using
  `attr(x, "objbound")`. Note that this is only supported for the Gurobi
  solver.
- Update `ConservationProblem` class so that overwriting problem
  components will yield a more concise warning message.
- Update [`compile()`](https://prioritizr.net/reference/compile.md)
  function to throw more informative warnings when a
  [`problem()`](https://prioritizr.net/reference/problem.md) have an
  objective that does not support weights or targets.
- Update
  [`category_layer()`](https://prioritizr.net/reference/category_layer.md)
  and
  [`category_vector()`](https://prioritizr.net/reference/category_vector.md)
  to work with continuous values
  ([\#381](https://github.com/prioritizr/prioritizr/issues/381)). In
  cases, where a given pixel has multiple non-zero values, it will be
  allocated to the category with the greatest value. Thanks to Martin
  Jung ([@Martin-Jung](https://github.com/Martin-Jung)) for the
  suggestion.
- Fix bug in internal `get_crs()` function when using
  [`raster::raster()`](https://rdrr.io/pkg/raster/man/raster.html) or
  [`raster::stack()`](https://rdrr.io/pkg/raster/man/stack.html)
  objects.
- Fix bug in
  [`add_relative_targets()`](https://prioritizr.net/reference/add_relative_targets.md)
  that produced an incoherent error message.
- Fix bug in
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  that produced poorly formatted error message.
- Speed up internal validation of
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  raster data. In particular, the minimum and maximum values of rasters
  are now computed with `terra::minmax(x, compute = TRUE)`, instead of
  `terra::global(x, "range", na.rm = TRUE)`.
- Fix bug in
  [`intersecting_units()`](https://prioritizr.net/reference/intersecting_units.md)
  function that caused it to throw an incorrect error message when used
  with an
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  object containing geometry collection data
  ([\#379](https://github.com/prioritizr/prioritizr/issues/379)). Thanks
  to Alan Jackson ([@alankjackson](https://github.com/alankjackson)) for
  bug report.

### Documentation

- Update [`?targets`](https://prioritizr.net/reference/targets.md) to
  provide a comprehensive overview of the target functions.
- Update
  [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md)
  function documentation with better example.
- Fix incorrect text in Management Zones vignette
  ([\#382](https://github.com/prioritizr/prioritizr/issues/382)). Thanks
  to Anthony Richardson ([@ric325](https://github.com/ric325)) for bug
  report.
- Update publication record.

## prioritizr 8.0.6.8

### Minor improvements and bug fixes

- Update
  [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md)
  function to reduce run time.
- Update [`solve()`](https://prioritizr.net/reference/solve.md) function
  and the importance functions to ensure consistency their in output
  formats. Note that these changes do not alter their outputs.
- Update
  [`eval_ferrier_importance()`](https://prioritizr.net/reference/eval_ferrier_importance.md)
  function to better provide error messages with improved formatting.
- Update internal `any_nonNA()`, `any_nonzero()`, and `all_binary()`
  functions for processing raster data.
- Update internal `any_nonzero()` and `any_nonNA()` functions to provide
  better error messages.

## prioritizr 8.0.6.7

### New features

- New
  [`calibrate_cohon_penalty()`](https://prioritizr.net/reference/calibrate_cohon_penalty.md)
  function for automatically identifying a suitable penalty value for
  the penalties functions
  ([\#175](https://github.com/prioritizr/prioritizr/issues/175)). It is
  designed to work with any objective function and any of the penalty
  functions available in the package.
- New
  [`add_neighbor_penalties()`](https://prioritizr.net/reference/add_neighbor_penalties.md)
  function to reduce spatial fragmentation. This function is especially
  useful when working with large-scale problems or open source solvers.
- Update
  [`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md),
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md),
  and
  [`add_highs_solver()`](https://prioritizr.net/reference/add_highs_solver.md),
  functions with a new `control` parameter that can be used to manually
  specify additional parameters for customizing the optimization process
  ([\#354](https://github.com/prioritizr/prioritizr/issues/354)).

### Minor improvements and bug fixes

- Update problem formulation for
  [`add_connectivity_penalties()`](https://prioritizr.net/reference/add_connectivity_penalties.md),
  [`add_asym_connectivity_penalties()`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md),
  and
  [`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md)
  to slightly improve solve times. In particular, instead of using
  binary variables to model the product of the planning unit decision
  variables, continuous variables are now used. The documentation for
  these functions has also been updated to mention this information.
  Thanks to Bistra Dilkina for the suggestion.
- Update
  [`add_neighbor_constraints()`](https://prioritizr.net/reference/add_neighbor_constraints.md)
  function so that setting `clamp = TRUE` is more likely to resolve
  infeasibility issues. In particular, setting `clamp = TRUE` will (i)
  limit the minimum number of neighbors for a given planning unit based
  on the locked out constraints of neighboring planning units and (ii)
  not apply this constraint to any locked in or locked out planning
  units.
- Update
  [`add_min_shortfall_objective()`](https://prioritizr.net/reference/add_min_shortfall_objective.md)
  and
  [`add_min_largest_shortfall_objective()`](https://prioritizr.net/reference/add_min_largest_shortfall_objective.md)
  functions to employ a slightly different problem formulation that –
  despite being functionally identical to the previous formulation – has
  better performance for large-scale problems
  ([\#357](https://github.com/prioritizr/prioritizr/issues/357)). Thanks
  to Aboozar Mohammadi ([@AboozarM](https://github.com/AboozarM)) for
  the suggestion.
- Update
  [`write_problem()`](https://prioritizr.net/reference/write_problem.md)
  function to support all the file formats supported by the Gurobi
  solver (per
  [`gurobi::gurobi_write()`](https://rdrr.io/pkg/gurobi/man/gurobi_write.html)).
  Of particular note, this means that problems can now be saved in
  compressed file file format (e.g., `.mps.gz`).
- Update
  [`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md)
  function so that the `presolve` parameter can be used to specify the
  intensity of the presolve process. Similar to
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md),
  the `presolve` parameter is now specified as an integer value. The
  default value is now 2, which specifies the most intensive level of
  presolve. For backwards compatibility, a value of `TRUE` is treated as
  a value of 1.
- Update `eval_target_coverage_amount()` so that the relative shortfall
  for each target is now calculated by dividing the absolute shortfall
  by the absolute target. This change is to ensure consistency with the
  minimum shortfall objective.
- Fix bug in internal `repr.list()` function that displayed duplicate
  class names.
- Fix bug in
  [`adjacency_matrix()`](https://prioritizr.net/reference/adjacency_matrix.md),
  [`compile()`](https://prioritizr.net/reference/compile.md), and
  [`zone_names()`](https://prioritizr.net/reference/zone_names.md)
  functions that caused an unhelpful error message when calling the
  function without any arguments.
- Update unit tests for
  [`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md),
  [`add_connectivity_penalties()`](https://prioritizr.net/reference/add_connectivity_penalties.md),
  and
  [`add_asym_connectivity_penalties()`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md)
  to reduce run time.
- Fix bug in unit tests for
  [`add_asym_connectivity_penalties()`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md),
  and
  [`eval_rank_importance()`](https://prioritizr.net/reference/eval_rank_importance.md)
  functions. Note that these bugs do not affect the correctness of the
  functions as implemented in the package.
- Classes are now exported to make it easier for reverse dependencies to
  add their own objectives, constraints, penalties, targets, and
  solvers.

### Documentation updates

- Fix mistake in
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
  function documentation for the `numeric_focus` parameter.
- Fix typo in equation for
  [`add_max_utility_objective()`](https://prioritizr.net/reference/add_max_utility_objective.md)
  ([\#373](https://github.com/prioritizr/prioritizr/issues/373)). Thanks
  to Anthony Richardson ([@ric325](https://github.com/ric325)) for bug
  report.
- Update Calibrating trade-offs vignette with new
  [`calibrate_cohon_penalty()`](https://prioritizr.net/reference/calibrate_cohon_penalty.md)
  function.
- Update package overview vignette with new
  [`add_neighbor_penalties()`](https://prioritizr.net/reference/add_neighbor_penalties.md)
  function.
- Update solver benchmarks vignette to remove unnecessary package
  dependencies.
- Update publication record.

## prioritizr 8.0.6.6

### Minor improvements and bug fixes

- Fix bug in
  [`eval_rank_importance()`](https://prioritizr.net/reference/eval_rank_importance.md)
  function that could lead to incorrect importance values when
  considering proportion or semi-continuous decision types (i.e.,
  problems with
  [`add_proportion_decisions()`](https://prioritizr.net/reference/add_proportion_decisions.md)
  or
  [`add_semicontinuous_decisions()`](https://prioritizr.net/reference/add_semicontinuous_decisions.md)).

### Documentation updates

- Update publication record.

## prioritizr 8.0.6.5

### Minor improvements and bug fixes

- Fix bug in
  [`eval_rank_importance()`](https://prioritizr.net/reference/eval_rank_importance.md)
  function that caused a superfluous warning to be thrown when locked
  constraints (i.e,
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md),
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md),
  or
  [`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md)).

## prioritizr 8.0.6.4

### Minor improvements and bug fixes

- Update
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md),
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md),
  [`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md),
  and
  [`add_manual_bounded_constraints()`](https://prioritizr.net/reference/add_manual_bounded_constraints.md)
  functions so that planning units can be locked based on their planning
  unit identifier values when specifying `data.frame` planning units
  ([\#359](https://github.com/prioritizr/prioritizr/issues/359)). These
  functions have also been updated to provide more informative error
  messages when invalid data are specified. Thanks to Martin Jung
  ([@Martin-Jung](https://github.com/Martin-Jung)) for bug report.
- Fix
  [`eval_rank_importance()`](https://prioritizr.net/reference/eval_rank_importance.md)
  to better account for proportion-type and semi-continuous decision
  types ([\#367](https://github.com/prioritizr/prioritizr/issues/367)).
  Thanks to Martin Jung ([@Martin-Jung](https://github.com/Martin-Jung))
  for bug report.
- Fix bug in [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rspatial.github.io/terra/reference/summary.html)
  functions for
  [`problem()`](https://prioritizr.net/reference/problem.md) objects
  that caused the functions to incorrectly show the classes of the
  planning unit data that inherit from multiple classes. For example,
  this means that
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  planning units will now be shown as having `"sf"` data, rather than
  `"sftbl_dftbldata.frame"` data. Similarly,
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  planning units will now be shown as `tbl_df` instead of
  `tbldfdata.frame`.
- Update internal `all_finite()` function to perform faster for
  `character` vector arguments.
- Update dependencies so that the *slam* package is now an optional
  dependency. This is because the *slam* package is only required when
  using
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  and
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md).
- Update
  [`marxan_problem()`](https://prioritizr.net/reference/marxan_problem.md)
  to provide better validation of input data and more informative error
  messages. This update also involves replacing the *data.table* package
  with the *vroom* package.
- Thanks to Sandra Neubert
  ([@sandra-neubert](https://github.com/sandra-neubert)) for code
  review.

### Documentation updates

- Update
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md),
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md),
  [`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md),
  and
  [`add_manual_bounded_constraints()`](https://prioritizr.net/reference/add_manual_bounded_constraints.md)
  documentation to provide more detail on specifying which planning
  units should be constrained
  ([\#359](https://github.com/prioritizr/prioritizr/issues/359)). Thanks
  to Martin Jung ([@Martin-Jung](https://github.com/Martin-Jung)) for
  bug report.
- Update README to thank Theodoros Ploumis
  ([@theodorosploumis](https://github.com/theodorosploumis)) for the
  logo.
- Update documentation for
  [`eval_rank_importance()`](https://prioritizr.net/reference/eval_rank_importance.md).
- Standardize terminology for referring to “cells” in raster data.
  Previously, some parts of the documentation referred to them as
  pixels.
- Update publication record.

## prioritizr 8.0.6.3

### Minor improvements and bug fixes

- Fix bug in
  [`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)
  that caused segmentation faults when invalid arguments to `data` were
  specified
  ([\#363](https://github.com/prioritizr/prioritizr/issues/363)).

### Documentation updates

- Update publication record.

## prioritizr 8.0.6.2

### Minor improvements and bug fixes

- Update internal functions (i.e., `all_match_of`, and `is_match_of()`)
  for validating arguments to be compatible with `character` vectors
  produced using the *glue* package
  ([\#360](https://github.com/prioritizr/prioritizr/issues/360)). Thanks
  to Dan Wismer ([@DanWismer](https://github.com/DanWismer)) for bug
  report.

### Documentation updates

- Update
  [`eval_feature_representation_summary()`](https://prioritizr.net/reference/eval_feature_representation_summary.md)
  documentation to improve description of the output data frame
  ([\#355](https://github.com/prioritizr/prioritizr/issues/355)). Thanks
  to Sam Bradshaw
  ([@sam-bradshaw-wcmc](https://github.com/sam-bradshaw-wcmc)) for bug
  report.

## prioritizr 8.0.6.1

### Minor improvements and bug fixes

- Update
  [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md)
  calculations to maintain compatibility with updates to the *terra*
  package.
- Thanks to Sandra Neubert
  ([@sandra-neubert](https://github.com/sandra-neubert)) for code
  review.

### Documentation updates

- Update publication record.
- Update package citation.

## prioritizr 8.0.6

CRAN release: 2025-01-09

### Notice

- CRAN release.

### New features

- New
  [`add_min_penalties_objective()`](https://prioritizr.net/reference/add_min_penalties_objective.md)
  function to generate solutions that focus on minimizing the penalties
  – as much as possible – whilst ensuring that (i) the cost of the
  solution does not exceed a budget and (ii) all feature representation
  targets are met. This function is designed to aid with hierarchical
  multi-objective optimization. It can be now used instead of specifying
  a minimum set objective with zero costs and a linear constraint to
  specify the budget.
- New `add_rank_importance()` function to evaluate the relative
  importance of planning units selected in a solution
  ([\#337](https://github.com/prioritizr/prioritizr/issues/337)).
  Briefly, this approach involves generating incremental prioritizations
  with increasing budgets, wherein planning units selected in a previous
  increment are locked in to the following solution. Additionally,
  locked out constraints are used to ensure that only planning units
  selected in the original solution are available for selection. The
  advantages of this approach are that it can (i) be computed relatively
  quickly for relatively large problems, (ii) account for the cost of
  different planning units, (iii) account for multiple management zones,
  and
  4.  apply to solutions generated using any objective function.

### Minor improvements and bug fixes

- Update
  [`adjacency_matrix()`](https://prioritizr.net/reference/adjacency_matrix.md),
  [`intersecting_units()`](https://prioritizr.net/reference/intersecting_units.md),
  [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md),
  [`eval_target_coverage_summary()`](https://prioritizr.net/reference/eval_target_coverage_summary.md),
  and [`problem()`](https://prioritizr.net/reference/problem.md) to
  improve processing speed.
- Update
  [`intersecting_units()`](https://prioritizr.net/reference/intersecting_units.md)
  to accommodate very large
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  objects.
- Update
  [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md) so
  that it has an optional `idx` parameter which can be used to specify
  planning unit indices if they have been pre-computed. This new
  parameter can be used to help speed up calculations.
- Update
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  and
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
  to throw error messages with standardized grammar.
- Update internal functions for processing
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  objects to improve processing speed.
- Update
  [`add_cplex_solver()`](https://prioritizr.net/reference/add_cplex_solver.md)
  error message for failing to initialize CPLEX.
- Update error message for internal matrix conversion function.
- Fix typo in error for importance and evaluation functions that is
  thrown when attempting to use a `solution` that is a different class
  than the planning units in `x`.
- Fix bug with warnings displaying the name of internal functions
  instead of user facing functions.
- Fix typo in citation information.

### Documentation updates

- Update Package overview and Calibrating trade-offs vignettes with
  information on the
  [`add_min_penalties_objective()`](https://prioritizr.net/reference/add_min_penalties_objective.md)
  function.
- Update
  [`add_max_utility_objective()`](https://prioritizr.net/reference/add_max_utility_objective.md)
  documentation to make it clear that the function is simply maximizing
  a weighted sum of the features.
- Update publication record.
- Update Calibrating trade-offs vignette to improve internal logic for
  determining the best guess penalty value for preliminary
  prioritizations.
- Small documentation improvements. Thanks to Sandra Neubert
  ([@sandra-neubert](https://github.com/sandra-neubert)).

## prioritizr 8.0.5

### Notice

- Abandoned candidate for CRAN release. Although this version was
  originally a candidate for CRAN release, we decided to add more
  functionality.

## prioritizr 8.0.4.5

### Minor improvements and bug fixes

- Update
  [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md) to
  improve memory efficiency when processing large-scale vector (e.g.,
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html))
  planning unit data. Thanks to Sahebeh Karimi for bug report.
- Update
  [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md) so
  that the `memory = TRUE` parameter can be used to reduce memory
  requirements by processing each feature layer separately. This
  parameter can now be used when processing both vector or raster
  planning unit data (previously it could only be used with raster
  planning unit data).

### Documentation updates

- Fix DOI for citation.
- Fix citations in package overview vignette and package manual entry to
  pass package checks.

## prioritizr 8.0.4.4

### Documentation updates

- Update package citation.
- Update documentation for package manual entry.
- Update
  [`add_asym_connectivity_penalties()`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md),
  [`add_connectivity_penalties()`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  [`marxan_connectivity_data_to_matrix()`](https://prioritizr.net/reference/marxan_connectivity_data_to_matrix.md)
  documentation so that examples are standalone and do not affect the
  session by loading packages.
- Update
  [`marxan_boundary_data_to_matrix()`](https://prioritizr.net/reference/marxan_boundary_data_to_matrix.md)
  and
  [`marxan_connectivity_data_to_matrix()`](https://prioritizr.net/reference/marxan_connectivity_data_to_matrix.md)
  documentation so that examples provides more information on how the
  functions work.
- Fix equation rendering in online documentation
  ([\#344](https://github.com/prioritizr/prioritizr/issues/344)). Thanks
  to Jason Everett ([@jaseeverett](https://github.com/jaseeverett)) and
  Anthony Richardson ([@ric325](https://github.com/ric325)) for bug
  report.

## prioritizr 8.0.4.3

### Documentation updates

- Update
  [`marxan_boundary_data_to_matrix()`](https://prioritizr.net/reference/marxan_boundary_data_to_matrix.md)
  and
  [`marxan_connectivity_data_to_matrix()`](https://prioritizr.net/reference/marxan_connectivity_data_to_matrix.md)
  documentation so that examples are standalone and do not affect the
  session by loading packages.

## prioritizr 8.0.4.2

### Minor improvements and bug fixes

- Fix bug in
  [`add_max_utility_objective()`](https://prioritizr.net/reference/add_max_utility_objective.md)
  that caused the optimization process to throw an error about problem
  infeasibility when using feature data that contain negative values
  ([\#334](https://github.com/prioritizr/prioritizr/issues/334)). Thanks
  to [@hannahmp](https://github.com/hannahmp) for bug report.
- Fix bug in
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  that would cause it to erroneously suggest that many planning units
  don’t have any feature data associated with them. This bug was caused
  when the feature data contained relatively large, negative values.
- Fix bug in
  [`binary_stack()`](https://prioritizr.net/reference/binary_stack.md)
  that caused it to throw an error when working with raster data
  containing zeros
  ([\#333](https://github.com/prioritizr/prioritizr/issues/333)).
- Fix bug in
  [`add_absolute_targets()`](https://prioritizr.net/reference/add_absolute_targets.md)
  where it would not throw a warning to the user know that a problem
  already had targets defined, and so adding the new targets would
  override the existing targets defined for the problem.
- Fix bug in `as.ZonesRaster` that resulted in an error when trying to
  convert a `SpatRaster` zones object (i.e., a `zones` object with
  *terra* package data) into `Raster` zones object (i.e., a `zones`
  object with *raster* package data).
- Fix bug in
  [`write_problem()`](https://prioritizr.net/reference/write_problem.md)
  needlessly printing messages about the *gurobi* package not being
  installed when the function is trying to automatically determine which
  solver to use (i.e., when using `solver = NULL`) and the package is
  not is available.
- Fix bug in
  [`branch_matrix()`](https://prioritizr.net/reference/branch_matrix.md)
  where it would not automatically convert object to the `phylo` class
  in the *ape* package.
- Update warning message for
  [`add_asym_connectivity_penalties()`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md)
  so that it now specifies that asymmetric connectivity values are
  required when symmetric values are incorrectly supplied
  ([\#339](https://github.com/prioritizr/prioritizr/issues/339)). Thanks
  to [@DanWismer](https://github.com/DanWismer) for bug report.
- Update warning messages so that they now indicate which function threw
  the warning message (using
  [`rlang::warn()`](https://rlang.r-lib.org/reference/abort.html)).
- Update [`compile()`](https://prioritizr.net/reference/compile.md) so
  that it throws an error when using the expanded version of a problem
  formulation with negative feature values. This is because the expanded
  version of the problem formulations are not compatible with negative
  feature values. Currently, the expanded version of the problem
  formulation is only required when using
  [`add_feature_contiguity_constraints()`](https://prioritizr.net/reference/add_feature_contiguity_constraints.md).
- Additional tests to improve test coverage.
- Small improvements to code style, maintainability, and logic thanks to
  code review by Sandra Neubert
  ([@sandra-neubert](https://github.com/sandra-neubert)).

### Documentation updates

- Update publication record.
- Fix cross-reference linking issues to classes in other packages
  ([\#340](https://github.com/prioritizr/prioritizr/issues/340)).

## prioritizr 8.0.4.1

### Minor improvements and bug fixes

- Fix issue with [`print()`](https://rdrr.io/r/base/print.html) and
  [`summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
  not displaying correct text for linear constraints
  ([\#330](https://github.com/prioritizr/prioritizr/issues/330)).

## prioritizr 8.0.4

CRAN release: 2024-06-05

### Notice

- CRAN release.

## prioritizr 8.0.3.7

### Notice

- New default portfolio method for
  [`problem()`](https://prioritizr.net/reference/problem.md) objects.
  This new default portfolio – which can be manually specified using
  [`add_default_portfolio()`](https://prioritizr.net/reference/add_default_portfolio.md)
  – involves simply generating a single solution. The reason why this
  new default portfolio method was chosen was because planning problems
  that contain insufficient data (e.g., feature and cost data) to
  identify meaningful priorities can sometimes result in solutions
  containing strange spatial artifacts (e.g., lines or bands of selected
  planning units, see
  [\#205](https://github.com/prioritizr/prioritizr/issues/205) and
  [\#268](https://github.com/prioritizr/prioritizr/issues/268)). Since
  the presence of these spatial artifacts can indicate an
  under-specified problem and shuffling optimization problems can
  suppress them, we have decided to update the default portfolio so that
  it does not shuffle problems. If users wish to prevent spatial
  artifacts from appearing in solutions, then spatial penalties (e.g.,
  [`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md)),
  spatial constraints (e.g.,
  [`add_neighbor_constraints()`](https://prioritizr.net/reference/add_neighbor_constraints.md)),
  or shuffle portfolios (e.g.,
  `add_shuffle_portfolio(number_solutions = 1)`) can be used.

### Minor improvements and bug fixes

- New
  [`add_default_portfolio()`](https://prioritizr.net/reference/add_default_portfolio.md)
  function for specifying the default behavior for generating a solution
  (see Notice above for further details).
- Update [`solve()`](https://prioritizr.net/reference/solve.md) so that
  it provides information on the optimality of solutions
  ([\#323](https://github.com/prioritizr/prioritizr/issues/323)). For
  example, you might specify a 10% optimality gap for the optimization
  process (e.g., using `add_highs_solver(gap = 0.1)`), and this might
  produce a solution that is at least 7% from optimality. The resulting
  output from [`solve()`](https://prioritizr.net/reference/solve.md)
  will now provide this information about the solution (i.e., the 7%
  from optimality), and can be accessed using the `gap` attribute (e.g.,
  `attr(x, "gap")`, where `x` is the output from
  [`solve()`](https://prioritizr.net/reference/solve.md)). Note that
  this information is currently only available when using the Gurobi or
  HiGHS solvers.
- Fix bug in
  [`add_linear_constraints()`](https://prioritizr.net/reference/add_linear_constraints.md)
  and
  [`add_linear_penalties()`](https://prioritizr.net/reference/add_linear_penalties.md)
  that resulted in an incorrect error message being shown
  ([\#324](https://github.com/prioritizr/prioritizr/issues/324)).
- Fix bug in
  [`add_shuffle_portfolio()`](https://prioritizr.net/reference/add_shuffle_portfolio.md)
  that prevented solvers from using a pre-specified starting solution
  (per the `start` parameter) correctly. Please note that this bug did
  not result in incorrect solutions, it only meant that any
  pre-specified starting solutions were not used properly.
- Fix bug in
  [`add_cplex_solver()`](https://prioritizr.net/reference/add_cplex_solver.md)
  that caused solutions to not provide runtime information for the
  optimization process.

## prioritizr 8.0.3.6

### Minor improvements and bug fixes

- Fix bug in
  [`add_shuffle_portfolio()`](https://prioritizr.net/reference/add_shuffle_portfolio.md)
  so that optimization problems are randomly shuffled when a single
  solution is requested. This update should help prevent “strange”
  solutions that contain long horizontal lines/bands of planning units
  ([\#205](https://github.com/prioritizr/prioritizr/issues/205),
  [\#268](https://github.com/prioritizr/prioritizr/issues/268)).
- Update
  [`add_contiguity_constraints()`](https://prioritizr.net/reference/add_contiguity_constraints.md)
  and
  [`add_feature_contiguity_constraints()`](https://prioritizr.net/reference/add_feature_contiguity_constraints.md)
  to be compatible with updates to the *igraph* package.
- Update
  [`write_problem()`](https://prioritizr.net/reference/write_problem.md)
  so that it can use the *gurobi* package to write problems (if
  desired). This substantially reduces run time, because writing
  problems using the *Rsymphony* packages also requires solving them.

### Documentation updates

- Update publication record.

## prioritizr 8.0.3.5

### Minor improvements and bug fixes

- Update
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  to throw warning if a problem has a single feature
  ([\#309](https://github.com/prioritizr/prioritizr/issues/309)). Thanks
  to Sandra Neubert
  ([@sandra-neubert](https://github.com/sandra-neubert)) for code
  contribution.
- Update [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rspatial.github.io/terra/reference/summary.html)
  for [`problem()`](https://prioritizr.net/reference/problem.md) objects
  so that all text is printed at once (rather than sequentially).
- Fix
  [`write_problem()`](https://prioritizr.net/reference/write_problem.md)
  so that it works as expected
  ([\#312](https://github.com/prioritizr/prioritizr/issues/312)).
- Update [`problem()`](https://prioritizr.net/reference/problem.md),
  [`add_linear_constraints()`](https://prioritizr.net/reference/add_linear_constraints.md),
  [`add_linear_penalties()`](https://prioritizr.net/reference/add_linear_penalties.md),
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md),
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md),
  [`adjacency_matrix()`](https://prioritizr.net/reference/adjacency_matrix.md),
  [`binary_stack()`](https://prioritizr.net/reference/binary_stack.md),
  [`category_layer()`](https://prioritizr.net/reference/category_layer.md),
  [`connectivity_matrix()`](https://prioritizr.net/reference/connectivity_matrix.md),[`fast_extract()`](https://prioritizr.net/reference/fast_extract.md),
  [`intersecting_units()`](https://prioritizr.net/reference/intersecting_units.md),
  [`proximity_matrix()`](https://prioritizr.net/reference/proximity_matrix.md),
  [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md),
  [`simulate_data()`](https://prioritizr.net/reference/simulate_data.md),
  [`simulate_species()`](https://prioritizr.net/reference/simulate_species.md),
  [`simulate_cost()`](https://prioritizr.net/reference/simulate_cost.md),
  and [`zones()`](https://prioritizr.net/reference/zones.md) and other
  functions so that they will throw an error if a categorical
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object is provided as an argument
  ([\#313](https://github.com/prioritizr/prioritizr/issues/313)). This
  is because categorical rasters are not supported. Thanks to Martin
  Jung ([@Martin-Jung](https://github.com/Martin-Jung)) for bug report.
- Fix NAMESPACE issues related to registration of internal S3 methods.
- Fix bug with
  [`problem()`](https://prioritizr.net/reference/problem.md) not
  throwing multiple warnings with unusual data (e.g., given cost and
  feature data with negative values, previously only a single warning
  about negative costs would be thrown). Thanks to Sandra Neubert
  ([@sandra-neubert](https://github.com/sandra-neubert)) for bug report.

### Documentation updates

- Update publication record.
- Update package-level manual entry.
- Update URLs.

## prioritizr 8.0.3.4

### Minor improvements and bug fixes

- Update [`problem()`](https://prioritizr.net/reference/problem.md) to
  be more memory efficient when using a sparse matrix (`dgCMatrix`)
  argument for the `rij_matrix` parameter.
- Update error messages for checking that objects have the same
  coordinate reference system and overlapping spatial extents to format
  argument names correctly.
- Update error messages for nested expressions to refer to expressions
  using `Caused by error` instead of `Caused by NULL`.

### Documentation updates

- Update publication record.

## prioritizr 8.0.3.3

### Minor improvements and bug fixes

- Fix
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  and
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  error messages when supplying `locked_in` and `locked_out` objects
  that do not spatially intersect with the planning units.
- Update error message for checking if objects spatially overlap to
  improve clarity.

### Documentation updates

- Update publication record.

## prioritizr 8.0.3.2

### Documentation updates

- Update URLs.

## prioritizr 8.0.3.1

### Minor improvements and bug fixes

- Fix aliasing for package manual entry
  ([\#301](https://github.com/prioritizr/prioritizr/issues/301)).

### Documentation updates

- Update publication record.

## prioritizr 8.0.3

CRAN release: 2023-08-08

### Notice

- CRAN release.

## prioritizr 8.0.2.7

### Notice

- We have developed a better approach for rescaling boundary data to
  avoid numerical issues during optimization
  ([\#297](https://github.com/prioritizr/prioritizr/issues/297)).
  Earlier versions of the package recommended the use of the
  [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html)
  to rescale such data. However, we now realize that this approach can
  produce inconsistencies for boundary length data (e.g., the total
  perimeter of a planning unit might not necessarily equal the sum of
  the edge lengths). In some cases, these inconsistencies can cause
  solutions generated with high boundary penalties (i.e., using
  [`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md)
  with a high `penalty` value) to contain a large reserve (i.e., a
  spatial cluster of selected of planning units) with a single
  unselected planning unit in the middle of the reserve. In the the
  worst case, these inconsistencies produce a situation where increasing
  boundary penalties (i.e., generating multiple solutions with
  [`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md)
  and increasing `penalty` values) does not alter the spatial
  configuration of solutions. Although use of
  [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html)
  did not produce such behavior prior to version 8.0.0, changes to the
  output format for
  [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md)
  in subsequent versions now mean that
  [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html)
  can cause these issues. We now recommend using the new
  [`rescale_matrix()`](https://prioritizr.net/reference/rescale_matrix.md)
  function to rescale boundary length data to avoid numerical issues,
  whilst also avoid such inconsistencies.

### New features

- New
  [`rescale_matrix()`](https://prioritizr.net/reference/rescale_matrix.md)
  function to help with rescaling boundary length (e.g., generated using
  [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md))
  and connectivity (e.g., generated using
  [`connectivity_matrix()`](https://prioritizr.net/reference/connectivity_matrix.md))
  data so avoid numerical issues during optimization
  ([\#297](https://github.com/prioritizr/prioritizr/issues/297)). Thanks
  to Jason Flower ([@jflowernet](https://github.com/jflowernet)) and
  Joan Giménez Verdugo for bug reports.

### Minor improvements and bug fixes

- Update the [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rspatial.github.io/terra/reference/summary.html)
  methods for [`problem()`](https://prioritizr.net/reference/problem.md)
  objects so that they will now better describe situations when the
  planning cost data all contain a constant value (e.g., all costs equal
  to 1).

### Documentation updates

- Update examples and vignettes to use the
  [`rescale_matrix()`](https://prioritizr.net/reference/rescale_matrix.md)
  function instead of the
  [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html)
  function for rescaling boundary length and connectivity data
  ([\#297](https://github.com/prioritizr/prioritizr/issues/297)).
- Update publication record.

## prioritizr 8.0.2.6

### New features

- Update `add_neighbors_constraints()` so that it has an additional
  `clamp` argument so the minimum number of neighbors permitted for each
  planning unit in the solution is clamped to the number of neighbors
  that each planning unit has. For example, if a planning unit has 2
  neighbors, `k = 3`, and `clamp = FALSE`, then the planning unit could
  not ever be selected in the solution. However, if `clamp = TRUE`, then
  the planning unit could potentially be selected in the solution if
  both of its 2 neighbors were also selected.

### Minor improvements and bug fixes

- Fix issue with
  [`problem()`](https://prioritizr.net/reference/problem.md) that
  prevents `features` being supplied as a `data.frame` that contains
  feature names stored as a `factor`
  ([\#295](https://github.com/prioritizr/prioritizr/issues/295)). Thanks
  to Carl Boetigger ([@cboettig](https://github.com/cboettig)) for bug
  report.

### Documentation updates

- Fix URLs.

## prioritizr 8.0.2.5

### Minor improvements and bug fixes

- Update [`problem()`](https://prioritizr.net/reference/problem.md) so
  that it will throw a meaningful error message if the user accidentally
  specifies the geometry column for `sf` planning unit data as a
  feature.

## prioritizr 8.0.2.4

### Minor improvements and bug fixes

- Fix compatibility with updates to *terra* package.
- Fix [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md)
  so that it works when none of the raster layers being processed fit
  into memory
  ([\#290](https://github.com/prioritizr/prioritizr/issues/290)). Thanks
  to Edwards Marc ([@edwardsmarc](https://github.com/edwardsmarc)) for
  bug report.
- Fix spatial extent of built-in raster datasets so that extents are
  between 0 and 1 (i.e.,
  [`get_sim_pu_raster()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_locked_in_raster()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_locked_out_raster()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_zones_pu_raster()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_features()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_zones_features()`](https://prioritizr.net/reference/sim_data.md)).
- Update
  [`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md)
  and
  [`add_manual_bounded_constraints()`](https://prioritizr.net/reference/add_manual_bounded_constraints.md)
  so that the indices in the specified in the argument `data$pu` should
  consistently refer to the total units. In other words, the indices in
  `data$pu` should refer to the row numbers (for planning units in `sf`
  or `data.frame` format) or cell numbers (for planning units in
  `Raster` or `SpatRaster` format) of the planning units that should be
  locked.
- Fix warnings thrown due to package version comparisons.

### Documentation updates

- Update publication record.

## prioritizr 8.0.2.3

### Minor improvements and bug fixes

- Export
  [`solve.ConservationProblem()`](https://prioritizr.net/reference/solve.md)
  so that it can be called directly
  ([\#283](https://github.com/prioritizr/prioritizr/issues/283)). Thanks
  to Tin Buenafe ([@SnBuenafe](https://github.com/SnBuenafe)) for bug
  report.

### Documentation updates

- Update publication record.

## prioritizr 8.0.2.2

### Minor improvements and bug fixes

- Fix compatibility with *highs* package (version 0.1-10)
  ([\#281](https://github.com/prioritizr/prioritizr/issues/281)).

### Documentation updates

- Update publication record.

## prioritizr 8.0.2.1

### Minor improvements and bug fixes

- Update [`problem()`](https://prioritizr.net/reference/problem.md) so
  that an error will be thrown if argument to `features` contains only
  missing (`NA`) values (e.g., an *sf* object is supplied that has `NA`
  values in all rows for a feature’s column).

### Documentation updates

- Update publication record.

## prioritizr 8.0.2

CRAN release: 2023-05-01

### Notice

- The package has been updated to focus on using the *sf* and *terra*
  package for spatial vector and raster datasets. This is because the
  *sf* package is the successor to the *sp* package, and the *terra*
  package is the successor to the *raster* package. By leveraging these
  newer packages, the *prioritizr* package can provide better
  performance. Although *sp* and *raster* package classes (e.g.,
  [`raster::stack()`](https://rdrr.io/pkg/raster/man/stack.html) and
  `sp::SpatialPolyonsDataFrame()`) are still supported, the *prioritizr*
  package will now throw deprecation warnings. Since support for the
  *sp* and *raster* package classes will be fully deprecated and removed
  in a later version this year, we recommend updating code to use the
  *sf* and *terra* packages.

### Major changes

- All *proto* classes have been migrated to *R6* classes. This update
  reduces memory usage
  ([\#238](https://github.com/prioritizr/prioritizr/issues/238)), so
  [`problem()`](https://prioritizr.net/reference/problem.md) objects can
  now contain many more constraints and penalties. Note that any
  [`problem()`](https://prioritizr.net/reference/problem.md) objects
  that were produced using earlier versions of the package are no longer
  compatible. Thanks to Jason Flower
  ([@jflowernet](https://github.com/jflowernet)) for bug report on
  memory issues.
- The *proto*, *raster*, *sf*, *sp* packages are no longer automatically
  loaded alongside *prioritizr*. As such, users will need to load them
  manually (e.g., using
  [`library(sf)`](https://r-spatial.github.io/sf/)).
- The built-in datasets have been removed and replaced with functions to
  import them as needed (i.e.,
  [`get_sim_pu_raster()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_pu_polygons()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_pu_lines()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_pu_points()`](https://prioritizr.net/reference/sim_data.md),,
  [`get_sim_locked_in_raster()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_locked_out_raster()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_zones_pu_raster()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_zones_pu_polygons()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_phylogeny()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_features()`](https://prioritizr.net/reference/sim_data.md),
  [`get_sim_zones_features()`](https://prioritizr.net/reference/sim_data.md)).
  These functions now return
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html),
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
  [`ape::read.tree()`](https://rdrr.io/pkg/ape/man/read.tree.html) and
  [`zones()`](https://prioritizr.net/reference/zones.md) objects. Note
  that these functions are provided because `data(...)` cannot be used
  with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  objects. See [`?data`](https://rdrr.io/r/utils/data.html) for more
  information.
- The
  [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md)
  output format has been updated. This means that users will not be able
  to use boundary data generated using previous versions of the package.
- The
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  now throws an error, instead of a warning, if an old version of the
  *lpsymphony* package is installed that is known to produce incorrect
  results.
- The
  [`marxan_boundary_data_to_matrix()`](https://prioritizr.net/reference/marxan_boundary_data_to_matrix.md)
  function is no longer compatible with boundary data for multiple
  zones.
- The
  [`distribute_load()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function has been deprecated, because it is no longer used. For
  equivalent functionality, See
  [`parallel::splitIndices()`](https://rdrr.io/r/parallel/splitIndices.html).
- The
  [`new_optimization_problem()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  and
  [`predefined_optimization_problem()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  functions have been superseded by the new
  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  function.
- To simplify package documentation and functionality, the following
  functions are no longer exported: `is.Waiver()`,
  `add_default_decisions()` `new_id()`, `is.Id()`, `print.Id()`,
  `pproto()`.
- Updates to improve the error messages and error message handling.
  Hopefully, users should no longer see `"bad error message"`!

### New features

- The [`print()`](https://rdrr.io/r/base/print.html) function for
  [`problem()`](https://prioritizr.net/reference/problem.md),
  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md),
  and [`zones()`](https://prioritizr.net/reference/zones.md) objects has
  been updated to provide more information.
- New
  [`summary()`](https://rspatial.github.io/terra/reference/summary.html)
  function to provide extensive detail on
  [`problem()`](https://prioritizr.net/reference/problem.md) objects.

### Minor improvements and bug fixes

- Fix bug for
  [`add_feature_weights()`](https://prioritizr.net/reference/add_feature_weights.md)
  when applied to problems with an
  [`add_max_phylo_div_objective()`](https://prioritizr.net/reference/add_max_phylo_div_objective.md)
  or `add_max_phylo_end_objectve()`. Specifically, the bug meant that
  weights weren’t being applied to problems with these particular
  objectives.
- Fix instructions in
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
  documentation for opening vignette.
- Update solver functions to provide instructions for installing
  dependencies in error messages when their dependencies are not
  available.
- To ensure consistency among the portfolio functions, all of them
  (except for
  [`add_extra_portfolio()`](https://prioritizr.net/reference/add_extra_portfolio.md))
  default to generating 10 solutions.
- The [`solve()`](https://prioritizr.net/reference/solve.md) function
  will now output
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  objects (instead of
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) objects),
  when the planning unit data are
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  objects.
- The
  [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md)
  function now uses
  [`terra::sharedPaths()`](https://rspatial.github.io/terra/reference/sharedPaths.html)
  for calculations, providing greater performance
  ([\#257](https://github.com/prioritizr/prioritizr/issues/257)). Thanks
  to Jason Flower ([@jflowernet](https://github.com/jflowernet)) for bug
  report.
- The
  [`eval_ferrier_importance()`](https://prioritizr.net/reference/eval_ferrier_importance.md)
  function can now be used with any objective function that uses targets
  and a single zone.
- Fix CRAN note regarding C++ standards
  ([\#263](https://github.com/prioritizr/prioritizr/issues/263)).
- Remove *doParallel* and *plyr* packages as dependencies by simplifying
  the
  [`add_shuffle_portfolio()`](https://prioritizr.net/reference/add_shuffle_portfolio.md)
  and
  [`eval_replacement_importance()`](https://prioritizr.net/reference/eval_replacement_importance.md)
  functions.
- Fix
  [`add_linear_penalties()`](https://prioritizr.net/reference/add_linear_penalties.md)
  function so that the penalty parameter is applied correctly
  ([\#342](https://github.com/prioritizr/prioritizr/issues/342)). In
  previous versions, this bug meant that solving a problem with
  `penalty = 1` would produce solution based on `penalty = -1` (and vice
  versa). Additionally, this bug also meant that compiling/solving a
  problem multiple times would cause the formulation to alternate
  between using `penalty = 1` and `penalty = -1`. Thanks to Carina
  Firkowski ([@Carina-Firkowski](https://github.com/Carina-Firkowski))
  for bug report.

### Documentation updates

- Assorted tweaks to improve writing in the vignettes and documentation.
  Many thanks to Marc Edwards
  ([@edwardsmarc](https://github.com/edwardsmarc)) for code
  contribution!
- Update publication record.

## prioritizr 8.0.1

### Minor improvements and bug fixes

- Assorted bug fixes.

## prioritizr 8.0.0

CRAN release: 2023-03-29

### Notice

- This version contains an incorrect version of the code, due to a
  mistake when preparing for CRAN release. We strongly recommend
  upgrading to version 8.0.1 to ensure correct results. We apologize any
  inconvenience this might have caused.

## prioritizr 7.2.2.7

### Documentation updates

- Update README badges.
- Update publication record.

## prioritizr 7.2.2.6

### Documentation updates

- Update publication record.

## prioritizr 7.2.2.5

### Documentation updates

- Update publication record.

## prioritizr 7.2.2.4

### Minor improvements and bug fixes

- Specify minimum version for Matrix package dependency
  ([\#255](https://github.com/prioritizr/prioritizr/issues/255)). Thanks
  to Bruno Carturan ([@BrunoCartu](https://github.com/BrunoCartu)) for
  bug report.

## prioritizr 7.2.2.3

### New features

- New
  [`add_highs_solver()`](https://prioritizr.net/reference/add_highs_solver.md)
  function for the HiGHS optimization software
  ([\#250](https://github.com/prioritizr/prioritizr/issues/250)).

### Minor improvements and bug fixes

- Update
  [`add_default_solver()`](https://prioritizr.net/reference/add_default_solver.md)
  to use the HiGHS solver if the Gurobi, IBM CPLEX, and CBC solvers
  aren’t available.
- Fix
  [`add_default_solver()`](https://prioritizr.net/reference/add_default_solver.md)
  so that the
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  is used instead of
  [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md).

## prioritizr 7.2.2.2

### Minor improvements and bug fixes

- Update [`problem()`](https://prioritizr.net/reference/problem.md) and
  [`eval_feature_representation_summary()`](https://prioritizr.net/reference/eval_feature_representation_summary.md)
  to avoid needlessly converting sparse matrices to regular matrices
  ([\#252](https://github.com/prioritizr/prioritizr/issues/252)).

### Documentation updates

- Fix URLs.

## prioritizr 7.2.2.1

### Documentation updates

- Fix mistake in `NEWS.md`.
- Update publication record.

## prioritizr 7.2.2

CRAN release: 2022-09-17

### Notice

- Release candidate for CRAN.

### Minor improvements and bug fixes

- Fix compiler warnings.
- Update tests to skip long-running tests on CRAN.

### Documentation updates

- Update examples to minimize overall package check timings for CRAN.

## prioritizr 7.2.1

### Notice

- Release candidate for CRAN (rejected due to package check timings).

## prioritizr 7.2.0.9

### Major changes

- Update
  [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md)
  to use STR query trees by default.
- Update
  [`simulate_data()`](https://prioritizr.net/reference/simulate_data.md),
  [`simulate_cost()`](https://prioritizr.net/reference/simulate_cost.md)
  and
  [`simulate_species()`](https://prioritizr.net/reference/simulate_species.md)
  functions to improve performance using the *fields* package.

### Minor improvements and bug fixes

- Fix compatibility with upcoming *Matrix* package version (version
  1.5-0).
- Update package documentation to provide details for obtaining and
  installing the *cplexAPI* package since it has been archived on CRAN
  ([\#214](https://github.com/prioritizr/prioritizr/issues/214)).
- Fix bug that caused the
  [`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md)
  to throw a segfault when solving a problem wherein the `rij_matrix(x)`
  has a zero amount for the last feature in the last planning unit
  ([\#247](https://github.com/prioritizr/prioritizr/issues/247)). Thanks
  to Jason Everett ([@jaseeverett](https://github.com/jaseeverett)) for
  bug report.
- Remove *maptools*, *PBSmapping*, and *rgeos* packages as dependencies.
  This involved updating the unit tests to hard-code correct results,
  updating examples to use the *sf* package, and updating the
  [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md)
  to use the *geos* package
  ([\#218](https://github.com/prioritizr/prioritizr/issues/218)).

### Documentation updates

- Fix URLs.

## prioritizr 7.2.0.8

### Major changes

- Update
  [`simulate_cost()`](https://prioritizr.net/reference/simulate_cost.md)
  and
  [`simulate_species()`](https://prioritizr.net/reference/simulate_species.md)
  so that they no longer depend on the *RandomFields* package. Note that
  these functions will now produce different outputs from previous
  versions (even when controlling for the random number generator
  state).

## prioritizr 7.2.0.7

### Documentation updates

- Update publication record.

## prioritizr 7.2.0.6

### Documentation updates

- Update publication record.

## prioritizr 7.2.0.5

### Documentation updates

- Update publication record.

## prioritizr 7.2.0.4

### Documentation updates

- Fix URLs.

## prioritizr 7.2.0.3

### Minor improvements and bug fixes

- Update the
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  function to (i) reduce chances of it incorrectly throwing an error
  when the input data won’t actually cause any issues, and (ii) provide
  recommendations for addressing issues.

### Documentation updates

- Update documentation for
  [`add_min_largest_shortfall_objective()`](https://prioritizr.net/reference/add_min_largest_shortfall_objective.md)
  so that examples complete in a shorter period of time.

## prioritizr 7.2.0.2

### Minor improvements and bug fixes

- Fix bug in processing planning unit data when a problem is constructed
  using arguments to (i) `x` that are `numeric` or `matrix` format, (ii)
  `x` that contain missing (`NA`) values, and (iii) `rij_matrix` that
  are in `dgCMatrix` format. This bug only occurred when all three of
  these specific conditions were met. When it occurred, the bug caused
  planning units with `NA` cost values to receive very high cost values
  (e.g., 1e+300). This bug meant that when attempting to solve the
  problem, the presolve checks (per
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md))
  would throw an error complaining about very high cost values
  ([\#236](https://github.com/prioritizr/prioritizr/issues/236)). Thanks
  to [@lmathon](https://github.com/lmathon) for bug report.

## prioritizr 7.2.0.1

### Documentation updates

- Update publication record.

## prioritizr 7.2.0.0

### Major changes

- Update
  [`add_connectivity_penalties()`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  function and documentation so that it is designed specifically for
  symmetric connectivity data.

### New features

- New
  [`add_asym_connectivity_penalties()`](https://prioritizr.net/reference/add_asym_connectivity_penalties.md)
  function that is designed specifically for asymmetric connectivity
  data. This function has been created to help ensure that asymmetric
  connectivity data are handled correctly. For instance, using
  asymmetric connectivity data with
  [`add_connectivity_penalties()`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  function in previous versions of the package sometimes resulted in the
  data being incorrectly treated as symmetric data. Additionally, this
  function uses an updated mathematical formulation for handling
  asymmetric connectivity so that it provides similar results to the
  *Marxan* software
  ([\#223](https://github.com/prioritizr/prioritizr/issues/223)). Thanks
  to Nina Faure Beaulieu ([@ninzyfb](https://github.com/ninzyfb)) for
  bug report.

### Minor improvements and bug fixes

- Fix
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  and
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
  to ensure that a meaningful error message is provided when no planing
  units are locked
  ([\#234](https://github.com/prioritizr/prioritizr/issues/234)). Thanks
  to Alec Nelson ([@AlecNelson](https://github.com/AlecNelson)) for bug
  report.
- Fix
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  so that it does not throw a meaningless warning when the mathematical
  objective function only contains zeros.
- Update
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  to help reduce chances of mis-attributing high connectivity/boundary
  values due to planning unit costs.
- Update
  [`marxan_problem()`](https://prioritizr.net/reference/marxan_problem.md)
  function so that it can be used with asymmetric connectivity data.
  This is now possible because there are dedicated functions for
  symmetric and asymmetric connectivity.

### Documentation updates

- Update publication record.
- Update URLs in publication record so that they pass CRAN checks.

## prioritizr 7.1.1.12

### Documentation updates

- Update publication record.

## prioritizr 7.1.1.11

### Documentation updates

- Update publication record.

## prioritizr 7.1.1.10

### Documentation updates

- Update publication vignette.
- Fix URLs.
- Improve documentation for the `zones` parameter of the
  [`add_connectivity_penalties()`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  function.

## prioritizr 7.1.1.9

### Documentation updates

- Update documentation for
  [`eval_ferrier_importance()`](https://prioritizr.net/reference/eval_ferrier_importance.md)
  ([\#220](https://github.com/prioritizr/prioritizr/issues/220)).
  Although this function is now recommended for general use, the
  documentation contained an outdated warning and so the warning has now
  been removed.

## prioritizr 7.1.1.8

### Minor improvements and bug fixes

- Fix bug so that the
  [`eval_n_summary()`](https://prioritizr.net/reference/eval_n_summary.md)
  function now returns a table with the column name `"n"` (instead of
  `"cost"`) for the number of selected planning units
  ([\#219](https://github.com/prioritizr/prioritizr/issues/219)).

### Documentation updates

- Update publication record.
- Update reference index for package website.
- Fix minor typos in vignettes.

## prioritizr 7.1.1.7

### Minor improvements and bug fixes

- Minimum version numbers are now provided for all R package
  dependencies (excepting base R packages)
  ([\#217](https://github.com/prioritizr/prioritizr/issues/217)).
- The *data.table* package is now listed as a suggested (optional)
  dependency. This is because it is only used by the
  [`marxan_problem()`](https://prioritizr.net/reference/marxan_problem.md)
  for importing *Marxan* data files.

## prioritizr 7.1.1.6

### Documentation updates

- Update publication record.

## prioritizr 7.1.1.5

### Documentation updates

- The *Tasmania tutorial* has been reworked into the *Getting started*
  tutorial. This tutorial now provides short introduction to using the
  package.
- The *Salt Spring Island tutorial* has been reworked into the
  *Connectivity tutorial*. This tutorial now explores different
  approaches for incorporating connectivity.
- The *prioritizr* vignette has been renamed to the *Package overview*
  vignette.
- New *Calibrating trade-offs tutorial* showcasing methods for running
  calibration analyses. It outlines blended and hierarchical approaches
  for generating a set of different prioritizations based on different
  parameters. It also covers different approaches for selecting a
  candidate prioritization based on different trade-offs.

## prioritizr 7.1.1.4

### Minor improvements and bug fixes

- Update tests to reduce run time and pass given slightly different
  results with new Gurobi version (9.5.0).
- Update built-in `sim_pu_sf` and `sim_pu_zones_sf` data given class
  updates to the *sf* package (compatible with version 1.0.3+).

### Documentation updates

- Update example for
  [`write_problem()`](https://prioritizr.net/reference/write_problem.md)
  function.

## prioritizr 7.1.1.3

### Documentation updates

- Update publication record.

## prioritizr 7.1.1.2

### Documentation updates

- Update publication record.

## prioritizr 7.1.1.1

### Documentation updates

- Fix URL in vignette.

## prioritizr 7.1.1

CRAN release: 2021-10-29

### Notice

- Release candidate for CRAN.

## prioritizr 7.1.0.4

### Notice

- Brandon Edwards added to package author list.

### Major changes

- Update
  [`eval_ferrier_importance()`](https://prioritizr.net/reference/eval_ferrier_importance.md)
  function with verified code.

### Minor improvements and bug fixes

- Update
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  function to throw warning when really high values specified in
  [`add_neighbor_constraints()`](https://prioritizr.net/reference/add_neighbor_constraints.md).

### Documentation updates

- Update documentation with information about Ferrier importance scores.
- Update Gurobi Installation guide vignette.
- Update benchmark vignette.

## prioritizr 7.1.0.3

### Minor improvements and bug fixes

Update
[`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md)
function so that it can use a starting solution to reduce run time (via
the `start_solution` parameter).

## prioritizr 7.1.0.2

### Minor improvements and bug fixes

- Remove *xtable* package from Suggests because it is no longer used.

### Documentation updates

- Fix examples that fail package checks.

## prioritizr 7.1.0.1

### Documentation updates

- Update publication vignette.

## prioritizr 7.1.0.0

### New features

- New `add_linear_constraint()` function to add arbitrary constraints.

### Minor improvements and bug fixes

- Update
  [`add_min_shortfall_objective()`](https://prioritizr.net/reference/add_min_shortfall_objective.md)
  and
  [`add_min_largest_shortfall_objective()`](https://prioritizr.net/reference/add_min_largest_shortfall_objective.md)
  functions to handle targets with a target threshold value of zero.

### Documentation updates

- Minor improvements to the documentation. These include moving
  mathematical details to dedicated sections, providing more links in
  the See also sections, fixing text formatting for the
  [`eval_connectivity_summary()`](https://prioritizr.net/reference/eval_connectivity_summary.md)
  function, and tweaking the header in the README.
- Update publication vignette.

## prioritizr 7.0.1.5

### Documentation updates

- Update publication vignette.

## prioritizr 7.0.1.4

### Documentation updates

- Update documentation and examples for
  [`problem()`](https://prioritizr.net/reference/problem.md) function.
- Update publication vignette.

## prioritizr 7.0.1.3

### Documentation updates

- New solver benchmark vignette.

## prioritizr 7.0.1.2

### Minor improvements and bug fixes

- Update
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
  function so that it doesn’t print excess debugging information
  (accidentally introduced in previous version 7.0.1.1).

## prioritizr 7.0.1.1

### New features

- Update
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
  function to support the `node_file_start` parameter for the Gurobi
  software. This functionality is useful solving large problems on
  systems with limited memory
  ([\#192](https://github.com/prioritizr/prioritizr/issues/192)). Thanks
  to [@negira](https://github.com/negira) and Alec Nelson
  ([@AlecNelson](https://github.com/AlecNelson)) for bug reports and
  suggestions.

## prioritizr 7.0.1

CRAN release: 2021-03-31

### Notice

- Release candidate for CRAN.

### Documentation updates

- Update DESCRIPTION with more information on the package usage.
- Update DESCRIPTION with details on *rcbc* package installation.

## prioritizr 7.0.0.8

### New features

- New
  [`write_problem()`](https://prioritizr.net/reference/write_problem.md)
  function to save the mixed integer programming representation of a
  conservation planning problem to a file. This function is useful for
  manually executing optimization solvers.

## prioritizr 7.0.0.7

### Documentation updates

- Fix typo in
  [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md)
  function documentation
  ([\#189](https://github.com/prioritizr/prioritizr/issues/189)).
- Update publication vignette.

## prioritizr 7.0.0.6

### Minor improvements and bug fixes

- Update
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
  function to allow specification of a starting solution
  ([\#187](https://github.com/prioritizr/prioritizr/issues/187)). This
  functionality is useful for conducting a boundary penalty parameter
  calibration exercise. Specifically, users can specify the starting
  solution for a given penalty value based on the solution obtained
  using a smaller penalty value.
- Fix [`solve()`](https://prioritizr.net/reference/solve.md) so it
  assigns layer names based on zone names for solutions in raster
  format.

## prioritizr 7.0.0.5

### Minor improvements and bug fixes

- Update methods for calculating solver runtime.

## prioritizr 7.0.0.4

### Minor improvements and bug fixes

- Fix
  [`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md)
  so that `time_limit` and `verbose` parameters work as expected.

### Documentation updates

- Update publication record.

## prioritizr 7.0.0.3

### Minor improvements and bug fixes

- Update
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
  function to report timings following the same methods as the other
  solvers.

## prioritizr 7.0.0.2

### Minor improvements and bug fixes

- Update
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  function to be more memory efficient
  ([\#183](https://github.com/prioritizr/prioritizr/issues/183)).
- Added *slam* package to dependencies to enable more memory efficient
  usage of the *lpsymphony* package
  ([\#183](https://github.com/prioritizr/prioritizr/issues/183)).

## prioritizr 7.0.0.1

### Minor improvements and bug fixes

- Update unit tests to solve a greater proportion of them using
  continuous integration services
  ([\#181](https://github.com/prioritizr/prioritizr/issues/181)).
- Update
  [`add_default_solver()`](https://prioritizr.net/reference/add_default_solver.md)
  so that
  [`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md)
  is now preferred over all other open source solvers.
- Fix bug in
  [`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md)
  that resulted in incorrect solutions to problems with equality
  constraints.

## prioritizr 7.0.0.0

### Minor improvements and bug fixes

- Remove unused *shiny* package integration and dependencies
  ([\#141](https://github.com/prioritizr/prioritizr/issues/141)).

## prioritizr 6.0.0.2

### New features

- New
  [`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md)
  function to generate solutions using the open source CBC solver via
  the *rcbc* package (<https://github.com/dirkschumacher/rcbc>).

### Minor improvements and bug fixes

- Update
  [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)
  and
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  functions to have a default `time_limit` argument set as the maximum
  machine integer for consistency.
- Update
  [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md),
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md),
  and
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
  functions to require `logical` (`TRUE`/`FALSE`) arguments for the
  `first_feasible` parameter.
- Update
  [`add_default_solver()`](https://prioritizr.net/reference/add_default_solver.md)
  function so that it prefers
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  over
  [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md),
  and
  [`add_cbc_solver()`](https://prioritizr.net/reference/add_cbc_solver.md)
  over all open source solvers.

### Documentation updates

- Previous versions of the package reported that the `gap` parameter for
  the
  [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)
  and
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  corresponded to the maximum absolute difference from the optimal
  objective value. This was an error due to misunderstanding the
  *SYMPHONY* documentation. Under previous versions of the package, the
  `gap` parameter actually corresponded to a relative optimality gap
  expressed as a percentage (such that`gap = 10` indicates that
  solutions must be at least 10% from optimality). We have now fixed
  this error and the documentation described for the `gap` parameter is
  correct. We apologize for any inconvenience this may have caused.
- Update documentation for solvers to provide more detailed information.
- Update publication record.

## prioritizr 6.0.0.1

### New features

- New `add_min_largest_shortfall()` objective function.

### Minor improvements and bug fixes

- Add more helpful error messages when invalid `solution` arguments are
  supplied to the evaluation functions
  ([\#176](https://github.com/prioritizr/prioritizr/issues/176)). Thanks
  to Phil Dyer ([@PhDyellow](https://github.com/PhDyellow)) for bug
  report.
- Add functionality to calculate importance scores using the Ferrier
  method with `sf` planning unit data.

### Documentation updates

- Update Solution format section documentation for evaluation functions
  (i.e. all functions starting with `eval_`) to mention that the
  argument to `solution` should only contain columns that correspond to
  the solution
  ([\#176](https://github.com/prioritizr/prioritizr/issues/176)). Thanks
  to Phil Dyer ([@PhDyellow](https://github.com/PhDyellow)) for bug
  report.
- Add examples using `sf` data to documentation for importance
  evaluation functions
  ([\#176](https://github.com/prioritizr/prioritizr/issues/176)).
- Fix broken link in
  [`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)
  documentation.
- Fix typo in equation for rarity weighted richness documentation.

## prioritizr 6.0.0.0

### New features

- New `eval_cost()` function to calculate the cost of a solution.
- New `eval_boundary()` function to calculate the exposed boundary
  length associated with a solution.
- New `eval_connectivity()` function to calculate the connectivity
  associated with a solution.
- New `eval_feature_representation()` function to assess how well each
  feature is represented by a solution. This function is similar to the
  deprecated `eval_feature_representation()` function, except that it
  follows conventions for other evaluation functions (e.g. `eval_cost`).
- New `eval_target_representation()` function to assess how well each
  target is met by a solution. This function is similar to the
  `eval_feature_representation()`, except that it corresponds to the
  targets in a conservation planning problem.

### Major changes

- Rename `ferrier_score` function as
  [`eval_ferrier_importance()`](https://prioritizr.net/reference/eval_ferrier_importance.md)
  function for consistency.
- Rename `replacement_cost` function as
  [`eval_replacement_importance()`](https://prioritizr.net/reference/eval_replacement_importance.md)
  function for consistency.
- Rename `rarity_weighted_richness` function as
  [`eval_rare_richness_importance()`](https://prioritizr.net/reference/eval_rare_richness_importance.md)
  function for consistency.
- Deprecated
  [`feature_representation()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function. It is now superseded by the `eval_feature_representation()`
  function.

### Minor improvements and bug fixes

- Fix comparability issues with *Matrix* package (version 1.3-0)
  ([\#172](https://github.com/prioritizr/prioritizr/issues/172)).

### Documentation updates

- Add NEWS to build process
  ([\#173](https://github.com/prioritizr/prioritizr/issues/173)).
- Update publication vignette.

## prioritizr 5.0.3.2

### Documentation updates

- Add Schuster et al. (2020) to documentation to provide information on
  solver benchmarks
  ([\#170](https://github.com/prioritizr/prioritizr/issues/170)). Thanks
  to Stefan Blumentrath ([@ninsbl](https://github.com/ninsbl)) for
  suggestion.

## prioritizr 5.0.3.1

### Minor improvements and bug fixes

- Fix
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
  function to enable a single planning unit from being locked out of
  multiple zones (when data are specified in raster format).

## prioritizr 5.0.3

CRAN release: 2020-11-24

### Notice

- Release candidate for CRAN.

## prioritizr 5.0.2.7

### Documentation updates

- Update publication record vignette.
- Fix URLs for CRAN checks.

## prioritizr 5.0.2.6

### Minor improvements and bug fixes

- Implement GitHub Actions continuous integration (i.e. update tests and
  README).
- Update [`problem()`](https://prioritizr.net/reference/problem.md)
  function to reduce memory consumption for sparse matrix arguments
  ([\#164](https://github.com/prioritizr/prioritizr/issues/164)).
- Fix compatibility issues between the *testthat* package and the
  *gurobi* package in package tests.

### Documentation updates

- Update Tasmania vignette to remove superfluous warnings
  ([\#168](https://github.com/prioritizr/prioritizr/issues/168)). Thanks
  to Jason Flower ([@jflowernet](https://github.com/jflowernet)) for bug
  report.
- Update publication record vignette.

## prioritizr 5.0.2.5

### New features

- New
  [`add_cplex_solver()`](https://prioritizr.net/reference/add_cplex_solver.md)
  function to generate solutions using [IBM
  CPLEX](https://www.ibm.com/products/ilog-cplex-optimization-studio)
  (via the *cplexAPI* package).

## prioritizr 5.0.2.4

### Minor improvements and bug fixes

- Fix target calculations in
  [`add_loglinear_targets()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  and
  [`loglinear_interpolation()`](https://prioritizr.net/reference/loglinear_interpolation.md)
  functions. Previously they used a natural logarithm for log-linear
  interpolation. To follow target setting approaches outlined by
  Rodrigues et al. (2004), they now use the decadic logarithm (i.e.
  [`log10()`](https://rdrr.io/r/base/Log.html)).

### Documentation updates

- Update publication record vignette.
- Update
  [`add_gap_portfolio()`](https://prioritizr.net/reference/add_gap_portfolio.md)
  documentation to note that it only works for problems with binary
  decisions
  ([\#159](https://github.com/prioritizr/prioritizr/issues/159)). Thanks
  to [@kkemink](https://github.com/kkemink) for report.

## prioritizr 5.0.2.3

### Documentation updates

- Update publication record vignette.

## prioritizr 5.0.2.2

### Documentation updates

- Update publication record vignette.

## prioritizr 5.0.2.1

### Documentation updates

- Update documentation for
  [`ferrier_score()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function. It no longer incorrectly states that these scores can be
  calculated using CLUZ and now states that this functionality is
  experimental until the formulation can be double checked.

## prioritizr 5.0.2

CRAN release: 2020-07-30

### Notice

- Release candidate for CRAN.

## prioritizr 5.0.1.7

### Minor improvements and bug fixes

- Fix tests for updated datasets in the *prioritizrdata* package.

### Documentation updates

- Fix small typos in documentation.
- Update citation for Scriven et al. (2020) in the Publication Record
  vignette.
- Update Salt Spring Island vignette with Ferrier method for calculating
  irreplaceability scores and adjust for changes in cost data.
- Update examples to run with CRAN checks (i.e. `--run-donttest`).

## prioritizr 5.0.1.6

### Minor improvements and bug fixes

- Fix
  [`feature_representation()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  bug incorrectly throwing error with vector planning unit data
  (e.g. `sf`-class data).

## prioritizr 5.0.1.5

### Minor improvements and bug fixes

- Fix typo causing
  [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md) to
  throw an error for large raster data
  ([\#151](https://github.com/prioritizr/prioritizr/issues/151)).
- Fix “Non-file package-anchored link(s) in documentation object”
  warnings in R-devel checks.

## prioritizr 5.0.1.4

### New features

- New
  [`add_linear_penalties()`](https://prioritizr.net/reference/add_linear_penalties.md)
  to add penalties that penalize planning units according to a linear
  metric.

### Documentation updates

- Update
  [`connectivity_matrix()`](https://prioritizr.net/reference/connectivity_matrix.md)
  documentation to provide an example of how to generate connectivity
  matrices that account for functional connectivity.
- Add more information to the documentation for the
  [`solve()`](https://prioritizr.net/reference/solve.md) function.
- Add links to the documentation for the
  [`solve()`](https://prioritizr.net/reference/solve.md) function to the
  Salt Spring Island and Tasmania vignettes.

## prioritizr 5.0.1.3

### Minor improvements and bug fixes

- Update [`compile()`](https://prioritizr.net/reference/compile.md) to
  throw warning when compiling problems that include feature weights and
  an objective function that does not use feature weights.

## prioritizr 5.0.1.2

### Documentation updates

- Add Schuster *et al.* (2020) to publication record.
- Update Hanson *et al.* (2020) in publication record.
- Update Flower *et al.* (2020) in publication record.

## prioritizr 5.0.1.1

### Minor improvements and bug fixes

- Update
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
  function to provide more options for controlling the pre-solve step
  when solving a problem.

## prioritizr 5.0.1

CRAN release: 2020-05-15

### Notice

- Release candidate for CRAN.

## prioritizr 5.0.0.1

### New features

- New
  [`ferrier_score()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function to compute irreplaceability scores following Ferrier *et al*
  (2000).

## prioritizr 5.0.0.0

### New features

- Add full support for *sf* package
  ([\#6](https://github.com/prioritizr/prioritizr/issues/6)).
- New
  [`proximity_matrix()`](https://prioritizr.net/reference/proximity_matrix.md)
  function to generate matrices indicating which planning units are
  within a certain distance of each other
  ([\#6](https://github.com/prioritizr/prioritizr/issues/6)).
- New
  [`add_extra_portfolio()`](https://prioritizr.net/reference/add_extra_portfolio.md),
  [`add_top_portfolio()`](https://prioritizr.net/reference/add_top_portfolio.md),
  [`add_gap_portfolio()`](https://prioritizr.net/reference/add_gap_portfolio.md)
  functions to provide specific options for generating portfolios
  ([\#134](https://github.com/prioritizr/prioritizr/issues/134)).

### Major changes

- Rename
  [`connected_matrix()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function to
  [`adjacency_matrix()`](https://prioritizr.net/reference/adjacency_matrix.md)
  function to follow the naming conventions of other spatial association
  functions ([\#6](https://github.com/prioritizr/prioritizr/issues/6)).
- Deprecate
  [`set_number_of_threads()`](https://prioritizr.net/reference/prioritizr-deprecated.md),
  [`get_number_of_threads()`](https://prioritizr.net/reference/prioritizr-deprecated.md),
  and
  [`is.parallel()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  functions since they are no longer used with new data extraction
  methods.
- Deprecate
  [`add_pool_portfolio()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function because the new
  [`add_extra_portfolio()`](https://prioritizr.net/reference/add_extra_portfolio.md)
  and
  [`add_top_portfolio()`](https://prioritizr.net/reference/add_top_portfolio.md)
  functions provide this functionality
  ([\#134](https://github.com/prioritizr/prioritizr/issues/134)).

### Minor improvements and bug fixes

- Enhance `intersecting_units` and `fast_extract` functions to use the
  *exactextractr* and *fasterize* packages to speed up raster data
  extraction
  ([\#130](https://github.com/prioritizr/prioritizr/issues/130)).
- Fix compatibility issues with upcoming version of tibble (3.0.0).
- Fix bug in
  [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md)
  function when handling `SpatialPolygon` planning unit data that
  contain multiple polygons (e.g. a single planning unit contains to two
  separate islands)
  ([\#132](https://github.com/prioritizr/prioritizr/issues/132)).
- Remove *velox* package dependency since it may be archived on CRAN
  ([\#130](https://github.com/prioritizr/prioritizr/issues/130)). Thanks
  to Jeffrey Evans ([@jeffreyevans](https://github.com/jeffreyevans))
  for report.
- Built-in datasets are now saved with latest workspace version
  (i.e. version 3).

### Documentation updates

- Add Flower *et al.* (2020), Hanson *et al.* (2020), and Visalli *et
  al.* (2020) to publication record
  ([\#131](https://github.com/prioritizr/prioritizr/issues/131)). Thanks
  to Jason Flower ([@jflowernet](https://github.com/jflowernet)) for
  report.

## prioritizr 4.1.5.2

### Minor improvements and bug fixes

- Fix bug in
  [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)
  and
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  throwing an an infeasible error message for feasible problems
  containing continuous or semi-continuous variables.

## prioritizr 4.1.5.1

### Minor improvements and bug fixes

- Add Lin *et al.* (in press) to publication record.

## prioritizr 4.1.5

CRAN release: 2020-02-06

### Notice

- Release candidate for CRAN.

## prioritizr 4.1.4.4

### Documentation updates

- Fix warnings in R-devel CRAN checks related to documentation.

## prioritizr 4.1.4.3

### Documentation updates

- Add Williams *et al.* (in press) to publication record.

## prioritizr 4.1.4.2

### Minor improvements and bug fixes

- Make error message for
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  function more informative
  ([\#124](https://github.com/prioritizr/prioritizr/issues/124)). Thanks
  to Amanda Liczner ([@aliczner](https://github.com/aliczner)) for bug
  report.

### Documentation updates

- Add Rodewald *et al.* (2019) to publication record.
- Update in press version of Rodewald *et al.* (2019).

## prioritizr 4.1.4.1

### Documentation updates

- Add Scriven *et al.* (in press) to publication record.

## prioritizr 4.1.4

CRAN release: 2019-10-03

### Notice

- Release candidate for CRAN.

## prioritizr 4.1.3.3

### Minor improvements and bug fixes

- Fix [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md)
  so that amounts are calculated correctly for vector-based planning
  unit data.

### Documentation updates

- Fix documentation for
  [`fast_extract()`](https://prioritizr.net/reference/fast_extract.md).

## prioritizr 4.1.3.2

### Documentation updates

- Add Rodewald *et al.* (in press) to publication record.
- Update reference for Bombi *et al.* (2019) in publication record.

## prioritizr 4.1.3.1

### Documentation updates

- Fix typo in README.

## prioritizr 4.1.3

CRAN release: 2019-09-06

### Notice

- Release candidate for CRAN.

## prioritizr 4.1.2.8

### Minor improvements and bug fixes

- Update
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  and
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
  functions so that they no longer throw an unnecessary warning when
  when they are added to multi-zone problems using raster data with `NA`
  values.

### Documentation updates

- Update documentation for
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  and
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
  functions to provide recommended practices for raster data.
- Update documentation for constraints missing “See also” and “Value”
  sections.

## prioritizr 4.1.2.7

### Minor improvements and bug fixes

- Fix issue with
  [`rarity_weighted_richness()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  returning incorrect scores when the feature data contains one feature
  that has zeros amounts in all planning units (e.g. the `tas_features`
  object in the *prioritizrdata* package;
  [\#120](https://github.com/prioritizr/prioritizr/issues/120)).
- Fix issue with
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md)
  returning solution statuses that are slightly larger than one
  (e.g. 1+1.0e-10) when solving problems with proportion-type decisions
  ([\#118](https://github.com/prioritizr/prioritizr/issues/118)). Thanks
  to Martin Jung ([@Martin-Jung](https://github.com/Martin-Jung)) for
  bug report.

## prioritizr 4.1.2.6

### New features

- New
  [`add_manual_bounded_constraints()`](https://prioritizr.net/reference/add_manual_bounded_constraints.md)
  function to apply lower and upper bounds on planning units statuses in
  a solution
  ([\#118](https://github.com/prioritizr/prioritizr/issues/118)). Thanks
  to Martin Jung ([@Martin-Jung](https://github.com/Martin-Jung)) for
  the suggestion.

### Minor improvements and bug fixes

- Update
  [`replacement_cost()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function to use parallel processing to speed up calculations
  ([\#119](https://github.com/prioritizr/prioritizr/issues/119)).

## prioritizr 4.1.2.5

### Minor improvements and bug fixes

- Update
  [`add_gurobi_solver()`](https://prioritizr.net/reference/add_gurobi_solver.md),
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md),
  and
  [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)
  functions so that they will not return solutions with values less than
  zero or greater than one when solving problems with proportion-type
  decisions. This issue is the result of inconsistent precision when
  performing floating point arithmetic
  ([\#117](https://github.com/prioritizr/prioritizr/issues/117)). Thanks
  to Martin Jung ([@Martin-Jung](https://github.com/Martin-Jung)) for
  bug report.
- Update
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  and
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
  functions to provide a more helpful error message the
  `locked_in`/`locked_out` argument refers to a column with data that
  are not logical (i.e. `TRUE`/`FALSE`;
  [\#118](https://github.com/prioritizr/prioritizr/issues/118)). Thanks
  to Martin Jung ([@Martin-Jung](https://github.com/Martin-Jung)) for
  bug report.

## prioritizr 4.1.2.4

### Minor improvements and bug fixes

- Update [`solve()`](https://prioritizr.net/reference/solve.md) function
  to throw a more accurate and helpful error message when no solutions
  are found (e.g. due to problem infeasibility or solver time limits).
- Standardize error messages so that none of them end in a full stop.

## prioritizr 4.1.2.3

### Major changes

- Rename
  [`add_max_phylo_objective()`](https://prioritizr.net/reference/add_max_phylo_div_objective.md)
  function to
  [`add_max_phylo_div_objective()`](https://prioritizr.net/reference/add_max_phylo_div_objective.md).

### New features

- New
  [`add_max_phylo_end_objective()`](https://prioritizr.net/reference/add_max_phylo_end_objective.md)
  function to maximize the phylogenetic endemism of species adequately
  represented in a prioritization
  ([\#113](https://github.com/prioritizr/prioritizr/issues/113)). Thanks
  to [@FerreiraPSM](https://github.com/FerreiraPSM) for the suggestion.

### Minor improvements and bug fixes

- Update simulated phylogeny dataset (`sim_phylogeny`).

### Documentation updates

- Add
  [`add_max_phylo_end_objective()`](https://prioritizr.net/reference/add_max_phylo_end_objective.md),
  [`replacement_cost()`](https://prioritizr.net/reference/prioritizr-deprecated.md),
  and
  [`rarity_weighted_richness()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  functions to the Prioritizr vignette.
- Update examples for
  [`add_max_phylo_div_objective()`](https://prioritizr.net/reference/add_max_phylo_div_objective.md)
  function.
- Prettify equations in the documentation for objective functions.

## prioritizr 4.1.2.2

### New features

- New
  [`replacement_cost()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function to calculate irreproducibility scores for each planning unit
  in a solution using the replacement cost method
  ([\#26](https://github.com/prioritizr/prioritizr/issues/26)).
- New
  [`rarity_weighted_richness()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function to calculate irreproducibility scores for each planning unit
  in a solution using rarity weighted richness scores
  ([\#26](https://github.com/prioritizr/prioritizr/issues/26)).

### Documentation updates

- New `irreplaceability` manual entry to document functions for
  calculating irreproducibility scores.
- Updated *Salt Spring Island* vignette with a section on calculating
  and interpreting irreplaceability scores.

## prioritizr 4.1.2.1

### Minor improvements and bug fixes

- Fix compiler warnings thrown during package installation.
- Skip tests on CRAN’s Windows system to reduce CRAN check times.
- Skip plotting data in examples during testing to reduce CRAN check
  times.
- Throw warning message if both the *prioritizr* and *oppr* packages are
  loaded at the same time.

### Documentation updates

- Fix typo.
- Fix broken links to *Gurobi* academic licenses.

## prioritizr 4.1.2

### Notice

- Release candidate for CRAN (rejected).

## prioritizr 4.1.1.2

### Minor improvements and bug fixes

- Fix example throwing an error during CRAN checks.

## prioritizr 4.1.1.1

### Documentation updates

- Add Bombi *et al.* (in press) to publication record.

## prioritizr 4.1.1.0

### Documentation updates

- Fix broken link in main vignette.

## prioritizr 4.1.1

CRAN release: 2019-06-06

### Notice

- Release candidate for CRAN.

## prioritizr 4.1.0.1

### New features

- New
  [`add_min_shortfall_objective()`](https://prioritizr.net/reference/add_min_shortfall_objective.md)
  function to find solutions that minimize target shortfalls.

### Minor improvements and bug fixes

- Fix [`problem()`](https://prioritizr.net/reference/problem.md) tests
  so that they work when no solvers are installed.

### Documentation updates

- Add new
  [`add_min_shortfall_objective()`](https://prioritizr.net/reference/add_min_shortfall_objective.md)
  function to main vignette.

## prioritizr 4.1.0.0

### Minor improvements and bug fixes

- The
  [`feature_representation()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function now requires missing (`NA`) values for planning unit statuses
  in a solution for planning units that have missing (`NA`) cost data.

## prioritizr 4.0.4.1

### New features

- New
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  function to investigate potential sources of numerical instability
  before trying to solve a problem. The manual entry for this function
  discusses common sources of numerical instability and approaches for
  fixing them.

### Minor improvements and bug fixes

- The [`solve()`](https://prioritizr.net/reference/solve.md) function
  will now use the
  [`presolve_check()`](https://prioritizr.net/reference/presolve_check.md)
  function to verify that problems do not have obvious sources of
  numerical instability before trying to solve them. If a problem is
  likely to have numerical instability issues then this function will
  now throw an error (unless the `solve(x, force = TRUE)`).
- The
  [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)
  function now uses sparse matrix formats so that attempts can be made
  to solve large problems with SYMPHONY—though it is unlikely that
  *SYMPHONY* will be able to solve such problems in a feasible period of
  time.
- Fix warnings thrown by the *tibble* package when calling
  [`tibble::as.tibble()`](https://tibble.tidyverse.org/reference/deprecated.html)
  instead of
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).

### Documentation updates

- Add example for calculating feature representation a solution in
  tabular format output by
  [`solve()`](https://prioritizr.net/reference/solve.md)
  ([\#110](https://github.com/prioritizr/prioritizr/issues/110)). Thanks
  to Martin Jung ([@Martin-Jung](https://github.com/Martin-Jung)) for
  suggestion.
- Fix several typos in documentation.
- Thrown warnings are now immediately visible.
- Update references in the publication record vignette.
- Specify English (US) in the DESCRIPTION file.

## prioritizr 4.0.4

CRAN release: 2019-04-12

### Notice

- Release candidate for CRAN.

## prioritizr 4.0.3.1

### Minor improvements and bug fixes

- Retain debugging symbols to conform with CRAN policies.

## prioritizr 4.0.3

CRAN release: 2019-04-08

### Notice

- Release candidate for CRAN.

## prioritizr 4.0.2.16

### Documentation updates

- Add new citations.

## prioritizr 4.0.2.15

### Documentation updates

- Fix typos in documentation for
  [`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md)
  and
  [`add_connectivity_penalties()`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  function
  ([\#106](https://github.com/prioritizr/prioritizr/issues/106)).

## prioritizr 4.0.2.14

### Minor improvements and bug fixes

- Fix bug where use of
  [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)
  and
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  sometimes returned infeasible solutions when subjected to a time limit
  ([\#105](https://github.com/prioritizr/prioritizr/issues/105)). Thanks
  to [@magalicombes](https://github.com/magalicombes) for bug report.

## prioritizr 4.0.2.13

### Minor improvements and bug fixes

- Fix assorted bugs in the render, setter, and getter parameter
  functions for `ConservationProblem-class` objects. These methods were
  implemented to be used in future interactive applications and are not
  currently used in the package. As a consequence, these bugs do not
  affect the correctness of any results.

## prioritizr 4.0.2.12

### Minor improvements and bug fixes

- Fix `bad error message` error being thrown when input rasters are not
  comparable (i.e. same coordinate reference system, extent,
  resolutions, and dimensionality)
  ([\#104](https://github.com/prioritizr/prioritizr/issues/104)). Thanks
  to [@faengl](https://github.com/faengl) for bug report.

### Documentation updates

- Add Domisch *et al.* (2019) to publication record vignette.

## prioritizr 4.0.2.11

### Minor improvements and bug fixes

- Fix issue [`solve()`](https://prioritizr.net/reference/solve.md)
  printing annoying text about `tbl_df`
  ([\#75](https://github.com/prioritizr/prioritizr/issues/75)). Thanks
  to Javier Fajardo
  ([@javierfajnolla](https://github.com/javierfajnolla)) for bug report.

## prioritizr 4.0.2.10

### Documentation updates

- Tweak
  [`add_max_features_objective()`](https://prioritizr.net/reference/add_max_features_objective.md)
  example code.

## prioritizr 4.0.2.9

### Documentation updates

- Update publication record vignette.

## prioritizr 4.0.2.8

### Minor improvements and bug fixes

- Fix bug where the
  [`add_neighbor_constraints()`](https://prioritizr.net/reference/add_neighbor_constraints.md)
  and
  [`add_contiguity_constraints()`](https://prioritizr.net/reference/add_contiguity_constraints.md)
  functions used more memory than they actually needed
  ([\#102](https://github.com/prioritizr/prioritizr/issues/102)). This
  is because the argument validation code converted sparse matrix
  objects (i.e. `dgCMatrix`) to base objects (i.e. `matrix`) class
  temporarily. This bug only meant inefficient utilization of computer
  resources—it did not affect the correctness of any results.

## prioritizr 4.0.2.7

### New feature

- New
  [`add_mandatory_allocation_constraints()`](https://prioritizr.net/reference/add_mandatory_allocation_constraints.md)
  function. This function can be used to ensure that every planning unit
  is allocated to a management zone in the solution. It is useful when
  developing land-use plans where every single parcel of land must be
  assigned to a specific land-use zone.

### Minor improvements and bug fixes

- Fix bug in the `$find(x)` method for `Collection` prototypes that
  caused it to throw an error incorrectly. This method was not used in
  earlier versions of this package.

### Documentation updates

- Add the
  [`add_mandatory_allocation_constraints()`](https://prioritizr.net/reference/add_mandatory_allocation_constraints.md)
  to the Management Zones and Prioritizr vignettes.

## prioritizr 4.0.2.6

### Minor improvements and bug fixes

- Fix bug the
  [`feature_representation()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function that caused the “amount_held” column to have NA values
  instead of the correct values. This bug only affected problems with
  multiple zones.

## prioritizr 4.0.2.5

### Minor improvements and bug fixes

- Fix bug in argument validation code for the
  [`category_layer()`](https://prioritizr.net/reference/category_layer.md)
  function that it this function to incorrectly throw an error claiming
  that the input argument to `x` was invalid when it was in fact valid.
  This bug is encountered when different layers the argument to `x` have
  non-NA values in different cells.

## prioritizr 4.0.2.4

### Documentation updates

- Update instructions for activating *Gurobi* licenses on remote
  machines ([\#98](https://github.com/prioritizr/prioritizr/issues/98)).

## prioritizr 4.0.2.3

### Minor improvements and bug fixes

- The
  [`add_contiguity_constraints()`](https://prioritizr.net/reference/add_contiguity_constraints.md)
  function now uses sparse matrix formats internally for single-zone
  problems. This means that the constraints can be applied to
  single-zoned problem with many more planning units.

## prioritizr 4.0.2.2

### Minor improvements and bug fixes

- The
  [`add_connectivity_penalties()`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  function now uses sparse matrix formats internally for single-zone
  problems. This means that connectivity penalties can be applied to
  single-zoned problem with many more planning units.

## prioritizr 4.0.2.1

### Minor improvements and bug fixes

- Update warning text when compiling problems that contain (i) objective
  functions that do not use targets and (ii) targets
  ([\#93](https://github.com/prioritizr/prioritizr/issues/93)).

### Documentation updates

- Update documentation for the
  [`add_max_utility_objective()`](https://prioritizr.net/reference/add_max_utility_objective.md)
  and
  [`add_max_cover_objective()`](https://prioritizr.net/reference/add_max_cover_objective.md)
  functions to make it clearer that they do not use targets
  ([\#94](https://github.com/prioritizr/prioritizr/issues/94)).

## prioritizr 4.0.2

CRAN release: 2018-06-28

### Notice

- Release candidate for CRAN.

## prioritizr 4.0.1.6

### Minor improvements and bug fixes

- Fix bug in
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  and
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
  that incorrectly threw an error when using `logical` locked data
  (i.e. `TRUE`/`FALSE`) because it incorrectly thought that valid inputs
  were invalid.
- Fix bug in
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md),
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md),
  and
  [`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md)
  where solving the same problem object twice resulted in incorrect
  planning units being locked in or out of the solution
  ([\#92](https://github.com/prioritizr/prioritizr/issues/92)). Thanks
  to Javier Fajardo
  ([@javierfajnolla](https://github.com/javierfajnolla)) for bug report.
- Added unit tests for objectives, constraints, decisions, targets, and
  penalties to ensure that solving problems twice does not result in
  different solutions.

## prioritizr 4.0.1.5

### Minor improvements and bug fixes

- Fix bug in
  [`feature_abundances()`](https://prioritizr.net/reference/feature_abundances.md)
  that caused the solve function to throw an error when attempting to
  solve problems with a single feature.
- Fix bug in
  [`add_cuts_portfolio()`](https://prioritizr.net/reference/add_cuts_portfolio.md)
  that caused the portfolio to return solutions that were not within the
  specified optimality gap when using the *Gurobi* solver.
- Add the ability to specify the search pool method and number of
  solutions to the
  [`add_pool_portfolio()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function.

## prioritizr 4.0.1.4

### Minor improvements and bug fixes

- The
  [`feature_representation()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function now allows `numeric` solutions with attributes (e.g. when
  output by the [`solve()`](https://prioritizr.net/reference/solve.md)
  function) when calculating representation statistics for problems with
  `numeric` planning unit data
  ([\#91](https://github.com/prioritizr/prioritizr/issues/91)). Thanks
  to Javier Fajardo
  ([@javierfajnolla](https://github.com/javierfajnolla)) for bug report.
- The
  [`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)
  function threw a warning when some features had targets equal to zero.
  This resulted in an excessive amount of warnings. Now, warnings are
  thrown for targets that are less then zero.
- The [`problem()`](https://prioritizr.net/reference/problem.md)
  function sometimes incorrectly threw a warning that feature data had
  negative values when the data actually did not contain negative
  values. This has now been addressed.

## prioritizr 4.0.1.3

### Minor improvements and bug fixes

- The `problem` function now allows negative values in the cost and
  feature data (and throws a warning if such data are detected).
- The
  [`add_absolute_targets()`](https://prioritizr.net/reference/add_absolute_targets.md)
  and
  [`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)
  functions now allow negative targets (but throw a warning if such
  targets are specified).
- The `compile` function throws an error if a problem is compiled using
  the expanded formulation with negative feature data.
- The
  [`add_absolute_targets()`](https://prioritizr.net/reference/add_absolute_targets.md)
  function now throws an warning—instead of an error—if the specified
  targets are greater than the feature abundances in planning units to
  accommodate negative values in feature data.

## prioritizr 4.0.1.2

### Documentation updates

- Fix
  [`add_max_cover_objective()`](https://prioritizr.net/reference/add_max_cover_objective.md)
  in *prioritizr* vignette
  ([\#90](https://github.com/prioritizr/prioritizr/issues/90)).

## prioritizr 4.0.1.1

### Minor improvements and bug fixes

- The
  [`add_loglinear_targets()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function now includes a
  [`feature_abundances()`](https://prioritizr.net/reference/feature_abundances.md)
  parameter for specifying the total amount of each feature to use when
  calculating the targets
  ([\#89](https://github.com/prioritizr/prioritizr/issues/89)). Thanks
  to Liz Law ([@lizlaw](https://github.com/lizlaw)) for the suggestion.

### Documentation updates

- The
  [`add_relative_targets()`](https://prioritizr.net/reference/add_relative_targets.md)
  documentation now makes it clear that locked out planning units are
  included in the calculations for setting targets
  ([\#89](https://github.com/prioritizr/prioritizr/issues/89)).

## prioritizr 4.0.1

CRAN release: 2018-05-26

### Notice

- Release candidate for CRAN.

## prioritizr 4.0.0.12

### New feature

- New
  [`feature_abundances()`](https://prioritizr.net/reference/feature_abundances.md)
  function to calculate the total amount of each feature in the planning
  units ([\#86](https://github.com/prioritizr/prioritizr/issues/86)).
  Thanks to Javier Fajardo
  ([@javierfajnolla](https://github.com/javierfajnolla)) for the
  suggestion.

## prioritizr 4.0.0.11

### Documentation updates

- Fix some equations in the documentation
  ([\#83](https://github.com/prioritizr/prioritizr/issues/83)).

## prioritizr 4.0.0.10

### Minor improvements and bug fixes

- Add version requirements for *assertthat* and *tibble* packages
  ([\#82](https://github.com/prioritizr/prioritizr/issues/82)).

## prioritizr 4.0.0.9

### Documentation updates

- Fix minor typos in the *Gurobi installation guide*.
- Update the *Management zones tutorial*.

## prioritizr 4.0.0.8

### Documentation updates

- Add instructions for setting up the *Gurobi* Academic license on a
  computer that it is not connected to a university computer network
  using a computer that is on an academic network
  ([\#81](https://github.com/prioritizr/prioritizr/issues/81)). For
  example, these instructions could be used to set up *Gurobi* on a
  cloud-based system using a laptop computer that is connected a
  university’s wireless network.

## prioritizr 4.0.0.7

### Minor improvements and bug fixes

- The
  [`add_cuts_portfolio()`](https://prioritizr.net/reference/add_cuts_portfolio.md)
  function uses the *Gurobi* solution pool to generate unique solutions
  within a specified gap of optimality when tasked with solving problems
  with *Gurobi* (version 8.0.0+)
  ([\#80](https://github.com/prioritizr/prioritizr/issues/80)).

## prioritizr 4.0.0.6

### New features

- New
  [`add_pool_portfolio()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function to generate a portfolio of solutions using the *Gurobi*
  solution pool
  ([\#77](https://github.com/prioritizr/prioritizr/issues/77)).

## prioritizr 4.0.0.5

### Minor improvements and bug fixes

- The
  [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md)
  function now has the experimental functionality to use GEOS STR trees
  to speed up processing
  ([\#74](https://github.com/prioritizr/prioritizr/issues/74)).
- Solutions obtained from *Gurobi* that contain binary-type decisions
  are explicitly rounded to the nearest integer. This is because
  *Gurobi* can output solutions to binary problems that contain values
  which not exactly zero or one (e.g. 0.9999997 using default settings)
  ([\#78](https://github.com/prioritizr/prioritizr/issues/78)).

## prioritizr 4.0.0.4

### New features

- New
  [`feature_representation()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  function to how well features are represented in solutions
  ([\#73](https://github.com/prioritizr/prioritizr/issues/73)).

## prioritizr 4.0.0.3

### Minor improvements and bug fixes

- The *prioritizrdata* package has been listed under Suggests.

### Documentation updates

- The vignettes in the *prioritizrdata* package have been moved to this
  package to make them easier to find.

## prioritizr 4.0.0.2

### Minor improvements and bug fixes

- Fix issue with the
  [`solve()`](https://prioritizr.net/reference/solve.md) function
  printing superfluous text
  ([\#75](https://github.com/prioritizr/prioritizr/issues/75)).

## prioritizr 4.0.0.1

### Documentation updates

- Minor improvements to the documentation for the
  [`problem()`](https://prioritizr.net/reference/problem.md) function.

## prioritizr 4.0.0.0

### New features

- Added functionality to build and solve problems with multiple
  management zones
  ([\#14](https://github.com/prioritizr/prioritizr/issues/14)).
- New built-in datasets `sim_pu_zones_stack`, `sim_pu_zones_polygons`,
  and `sim_features_zones` for exploring conservation problems with
  multiple management zones.
- New `zones` function and `Zones` class to organize data with multiple
  zones.
- New
  [`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)
  function for creating targets that pertain to multiple management
  zones.
- New
  [`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md)
  function to manually specify which planning units should or shouldn’t
  be allocated to specific zones in solutions.
- New
  [`binary_stack()`](https://prioritizr.net/reference/binary_stack.md),
  [`category_layer()`](https://prioritizr.net/reference/category_layer.md),
  and
  [`category_vector()`](https://prioritizr.net/reference/category_vector.md)
  functions have been provided to help work with data for multiple
  management zones.

### Major updates

- The [`problem()`](https://prioritizr.net/reference/problem.md)
  function now accepts `Zone` objects as arguments for `feature` to
  create problems with multiple zones.
- The
  [`add_relative_targets()`](https://prioritizr.net/reference/add_relative_targets.md)
  and
  [`add_absolute_targets()`](https://prioritizr.net/reference/add_absolute_targets.md)
  functions for adding targets to problems can be used to specify
  targets for each feature in each zone.
- The [`solve()`](https://prioritizr.net/reference/solve.md) function
  now returns a `list` of solutions when generating a portfolio of
  solutions.
- All functions for adding constraints and penalties now have parameters
  that specify how they should treat planning units allocate to
  different zones (using the `zones` parameter) and specify how they
  they should be applied (using the `data` parameter. All of these
  functions have default arguments that mean that problems with a single
  zone should have the same optimal solution as problems created in the
  earlier version of the package.

### Minor improvements and bug fixes

- The
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  and
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
  functions for specifying which planning units are locked in or out now
  accept `matrix` arguments for specifying which zones are locked in or
  out.
- The
  [`add_feature_weights()`](https://prioritizr.net/reference/add_feature_weights.md)
  function can be used to weight different the representation of each
  feature in each zone.

### Documentation updates

- New *Management zones* vignette on building and solving problems with
  multiple management zones.
- Added mention of zones functionality to package DESCRIPTION, summary
  (i.e.,
  [`?prioritizr`](https://prioritizr.net/reference/prioritizr.md)), and
  README.
- The *Quick Start Guide* and *Prioritizr Basics* vignettes have been
  consolidated into the *prioritizr* vignette.
- The
  [`marxan_problem()`](https://prioritizr.net/reference/marxan_problem.md)
  has been updated with more comprehensive documentation and to provide
  more helpful error messages. For clarity, it will now only work with
  tabular data in the standard *Marxan* format.

## prioritizr 3.0.3.6

### Documentation updates

- Fix typo in README.
- Update documentation for
  [`add_boundary_penalties()`](https://prioritizr.net/reference/add_boundary_penalties.md)
  ([\#62](https://github.com/prioritizr/prioritizr/issues/62)). Thanks
  to Liz Law ([@lizlaw](https://github.com/lizlaw)) for report.

## prioritizr 3.0.3.5

### Minor improvements and bug fixes

- Fix bug where
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  and
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
  throw an exception when used with semi-continuous-type decisions
  ([\#59](https://github.com/prioritizr/prioritizr/issues/59)).
- Error message in
  [`compile()`](https://prioritizr.net/reference/compile.md) thrown when
  the same planning unit is locked in and locked out now prints the
  planning unit indices in a readable format.

## prioritizr 3.0.3.4

### Minor improvements and bug fixes

- Fix bug where
  [`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
  and
  [`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
  are ignored when using proportion-type decisions
  ([\#58](https://github.com/prioritizr/prioritizr/issues/58)).

## prioritizr 3.0.3.3

### Minor improvements and bug fixes

- Fix bug in
  [`predefined_optimization_problem()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  which incorrectly recognized some inputs as invalid when they were in
  fact valid.
- Addressed NOTE in `R CMD check` related to *proto* package in Depends.

## prioritizr 3.0.3.2

### Minor improvements and bug fixes

- Moved *proto* package from Imports to Depends in DESCRIPTION

## prioritizr 3.0.3.1

### Minor improvements and bug fixes

- Depends on R version 3.4.0 (avoids ‘patchlevel 0’ NOTE/WARNING in
  checks)

## prioritizr 3.0.3

CRAN release: 2017-11-22

### Notice

- Release candidate for CRAN.

## prioritizr 3.0.2.3

### Minor improvements and bug fixes

- Unit tests that fail when using *lpsymphony* due to a bug in
  *lpsymphony* are now skipped (partially addressing
  [\#40](https://github.com/prioritizr/prioritizr/issues/40)).

## prioritizr 3.0.2.2

### Minor improvements and bug fixes

- Update
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  to throw warnings to alert users to potentially incorrect solutions
  (partially addressing
  [\#40](https://github.com/prioritizr/prioritizr/issues/40)).

## prioritizr 3.0.2.1

### Documentation updates

- Vignette sizes have been reduced.

## prioritizr 3.0.2

### Notice

- Release candidate for CRAN. Release postponed due issues on Travis CI.

## prioritizr 3.0.1.1

### Minor improvements and bug fixes

- Unit tests for `add_*_objectives` now pass when executed with slow
  solvers (partially addressing
  [\#40](https://github.com/prioritizr/prioritizr/issues/40)).
- Update [`compile()`](https://prioritizr.net/reference/compile.md) to
  work when no solvers are installed
  ([\#41](https://github.com/prioritizr/prioritizr/issues/41)).
- Gap arguments in `add_*_solvers` are now unbounded and can accept
  values larger than 1
  ([\#44](https://github.com/prioritizr/prioritizr/issues/44)).

## prioritizr 3.0.1

CRAN release: 2017-11-08

### Notice

- Release candidate for CRAN.

## prioritizr 3.0.0.0

### Major changes

- The
  [`add_max_cover_objective()`](https://prioritizr.net/reference/add_max_cover_objective.md)
  function has been renamed to the
  [`add_max_utility_objective()`](https://prioritizr.net/reference/add_max_utility_objective.md),
  because the formulation does not follow the historical formulation of
  the maximum coverage reserve selection problem
  ([\#38](https://github.com/prioritizr/prioritizr/issues/38)).
- The
  [`add_max_cover_objective()`](https://prioritizr.net/reference/add_max_cover_objective.md)
  function now follows the historical maximum coverage objective. This
  fundamentally changes
  [`add_max_cover_objective()`](https://prioritizr.net/reference/add_max_cover_objective.md)
  function and breaks compatibility with previous versions
  ([\#38](https://github.com/prioritizr/prioritizr/issues/38)).

### Minor improvements and bug fixes

- Update
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  examples and tests to skip on Linux operating systems.
- Add tests to unit tests that were being skipped in new version of
  *testthat* package.

## prioritizr 2.0.4.1

### Minor improvements and bug fixes

- Fix bug with
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  causing error when attempting to solve problems.

## prioritizr 2.0.4

### Notice

- Release candidate for CRAN. Release postponed due to bug report.

## prioritizr 2.0.3.1

### Minor improvements and bug fixes

- Fix bug when solving problems with `numeric` vector data that caused
  an error.
- Fix bug in compiling problems with `numeric` vector input with rij
  data containing NA values.
- Added unit tests for solving problems with various input formats.
- Updated package sizes reported in `cran-comments.md` file.

## prioritizr 2.0.3

### Notice

- Initial release candidate for CRAN. Release postponed due to bug
  report.

## prioritizr 2.0.2.9

### Documentation updates

- Added vignette to record publications that use *prioritizr*
  ([\#35](https://github.com/prioritizr/prioritizr/issues/35)).

## prioritizr 2.0.2.8

### Minor improvements and bug fixes

- Unit tests now compatible with development version of *testthat*
  ([\#34](https://github.com/prioritizr/prioritizr/issues/34)).

## prioritizr 2.0.2.7

### Minor improvements and bug fixes

- Fix bug in `apply_boundary_penalties()` and
  [`add_connectivity_penalties()`](https://prioritizr.net/reference/add_connectivity_penalties.md)
  causing the function to throw an error when the number of
  boundaries/edges is less than the number of planning units.

## prioritizr 2.0.2.6

### Minor improvements and bug fixes

- Makevars now compatible with Mac OSX Sierra
  ([\#33](https://github.com/prioritizr/prioritizr/issues/33)).

## prioritizr 2.0.2.5

### Minor improvements and bug fixes

- Fix bug in
  [`boundary_matrix()`](https://prioritizr.net/reference/boundary_matrix.md)
  calculations
  ([\#30](https://github.com/prioritizr/prioritizr/issues/30)).

## prioritizr 2.0.2.4

### Documentation updates

- Minor tweaks to vignettes.

## prioritizr 2.0.2.3

### Documentation updates

- Add logo to README files and package website
  ([\#31](https://github.com/prioritizr/prioritizr/issues/31)).

## prioritizr 2.0.2.2

### Minor improvements and bug fixes

- Remove *prioritizrdata* from package Suggests.
- Add *shiny* and *xtable* to Suggests for rendering parameters.
- Added code for `ScalarParameter` and `ArrayParameter` prototypes to
  check that functions for generating widgets have their dependencies
  installed.
- Fix bug when `numeric` planning unit data and portfolios that caused
  the [`solve()`](https://prioritizr.net/reference/solve.md) to throw an
  error.
- Remove R-devel from AppVeyor testing because it fails for unknown
  reasons.

### Documentation updates

- Broad-scale improvements to documentation.
- Fix documentation for
  [`add_max_phylo_objective()`](https://prioritizr.net/reference/add_max_phylo_div_objective.md)
  ([\#24](https://github.com/prioritizr/prioritizr/issues/24)).
- Update Gurobi Installation vignette.
- URLs for *lpsymphony* on Bioconductor now use the package’s DOI.
- Add more comprehensive tests to portfolios.

## prioritizr 2.0.2.1

### Major changes

- Removed shiny functions for now to prepare for CRAN release.

### Documentation updates

- Rebuilt website and documentation.

## prioritizr 2.0.2.0

### Documentation updates

- Included vignette on Gurobi solver installation and testing.

## prioritizr 2.0.1.0

### Major changes

- Fixed bug where `Spatial*DataFrame` input to
  [`marxan_problem()`](https://prioritizr.net/reference/marxan_problem.md)
  would always use the first column in the attribute table for the cost
  data. **This bug is serious** so analysis that used
  `Spatial*DataFrame` inputs in
  [`marxan_problem()`](https://prioritizr.net/reference/marxan_problem.md)
  should be rerun.

### Minor improvements and bug fixes

- Added functionality to use feature abundance/occurrence data stored as
  columns in the planning unit data when constructing
  [`problem()`](https://prioritizr.net/reference/problem.md) objects.

## prioritizr 2.0.0.2

### Minor improvements and bug fixes

- Skip
  [`add_cuts_portfolio()`](https://prioritizr.net/reference/add_cuts_portfolio.md)
  on Travis.

## prioritizr 2.0.0.1

### Minor improvements and bug fixes

- Skip
  [`add_cuts_portfolio()`](https://prioritizr.net/reference/add_cuts_portfolio.md)
  and
  [`add_shuffle_portfolio()`](https://prioritizr.net/reference/add_shuffle_portfolio.md)
  tests on CRAN.

## prioritizr 2.0.0.0

### Major changes

- This version breaks compatibility with previous releases because
  solutions in `data.frame` and `Spatial*DataFrame` objects are now
  stored in columns named “solution\_\*” (e.g. “solution_1”) to store
  multiple solutions.
- Solutions now contain additional information in stored in the object’s
  attributes
  ([\#24](https://github.com/prioritizr/prioritizr/issues/24)). See
  `README.Rmd` for examples on accessing this information.

### New features

- Added support for multiple solutions
  ([\#23](https://github.com/prioritizr/prioritizr/issues/23)).

### Minor improvements and bug fixes

- Add logical `verbose` argument to all solvers. This replaces the
  `verbosity`
- The verbosity of information presented when solving problems using
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  and
  [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md)
  is reduced.

### Documentation updates

- Tidy examples in `add_gurobi_solver.R`, `add_lpsymphony_solver.R`,
  `add_rsymphony_solver.R`, and `solvers.R`. argument in
  [`add_lpsymphony_solver()`](https://prioritizr.net/reference/add_lsymphony_solver.md)
  and
  [`add_rsymphony_solver()`](https://prioritizr.net/reference/add_rsymphony_solver.md).
- Assorted spelling mistakes have been fixed.

## prioritizr 1.0.2.3

### Minor improvements and bug fixes

- `ConservationProblem$print()` now only prints the first three species
  names and a count of the total number of features. This update means
  that `ConservationProblem` objects with lots of features can now
  safely be printed without polluting the R console.
- Fix bug where *lpsymphony* and *Rsymphony* solvers would return
  solutions containing NA values if they did not find a feasible
  solution within the argument to `time_limit`.

### Documentation updates

- Attempt to make equations in help files prettier.

## prioritizr 1.0.2.2

### Minor improvements and bug fixes

- Update
  [`marxan_problem()`](https://prioritizr.net/reference/marxan_problem.md)
  to work with absolute file paths and the `INPUTDIR` in Marxan input
  files ([\#19](https://github.com/prioritizr/prioritizr/issues/19)).
  Thanks to Dan Rosauer ([@DanRosauer](https://github.com/DanRosauer))
  for bug report.

## prioritizr 1.0.2.1

### Minor improvements and bug fixes

- Fix bug in [`solve()`](https://prioritizr.net/reference/solve.md) when
  the rij data does not contain the highest planning unit identifier
  specified when building the
  [`problem()`](https://prioritizr.net/reference/problem.md)
  ([\#20](https://github.com/prioritizr/prioritizr/issues/20)).

## prioritizr 1.0.2.0

### Minor improvements and bug fixes

- Passes CRAN checks on Winbuilder.
- Added *roxygen2* to Suggests for Travis CI.

## prioritizr 1.0.1.6

### Minor improvements and bug fixes

- Simplify vignette workflow. Vignettes can now be compiled by using
  [`devtools::build_vignettes()`](https://devtools.r-lib.org/reference/build_vignettes.html).
  Earlier versions needed the vignettes to be compiled using the
  *Makefile* to copy files around to avoid tangled R code causing
  failures during R CMD CHECK. Although no longer needed, the vignettes
  can still be compiled using the shell command `make vigns` if desired.
- Make the *data.table* package automatically installed when
  *prioritizr* is installed
  ([\#18](https://github.com/prioritizr/prioritizr/issues/18)).
- Move *shiny*, *shinydashboard*, and *leaflet* packages to Imports to
  avoid polluting users environment.
- Update preliminary versions of the shiny apps to call functions from
  other packages explicitly.
- Lint objective function definition files.
- Added *rmarkdown* package to Suggests following recommended practices.

### Documentation updates

- The `README.Rmd` now lives in the top-level directory following
  standard practices. It should now be complied using
  `rmarkdown::render("README.Rmd")` or using the shell command
  `make readme`. Note that the figures for `README.md` can be found in
  the directory `man/figures`.
- The example for `prshiny` will now only be run if executed during an
  interactive R session. Prior to this R CMD CHECK would hang.
- UTF-8 math characters in vignettes have been replaced with with
  MathJax compatible latex expressions.
- R code in the vignettes has been linted to follow the package’s style
  guide.
- Fix example in vignette `quick_start.Rmd` showing how to run
  [`marxan_problem()`](https://prioritizr.net/reference/marxan_problem.md)
  using input [`data.frame()`](https://rdrr.io/r/base/data.frame.html)
  objects.
- Fix bug in vignette `quick_start.Rmd` counting number of selected
  planning units
- `README.Rmd` tweaks to make it look prettier on website.
- Remove “” latex sequences from objective function definition files
  because CRAN doesn’t support *amsmath* extensions in equations.
- Update examples in objective function files to only show relevant
  objectives

## prioritizr 1.0.1.5

### Minor improvements and bug fixes

- Enable 64 bit Armadillo flag. This increases the maximum size of
  problems that can be solved.
- Disable bound-checks in Armadillo matrix operations. This should
  reduce processing time when running the
  [`compile()`](https://prioritizr.net/reference/compile.md) function.

## prioritizr 1.0.1.4

### Minor improvements and bug fixes

- Fix bug in `problem.data.frame` that meant that it did not check for
  missing values in `rij$pu`.

## prioritizr 1.0.1.3

### Minor improvements and bug fixes

- Fix bugs
  [`add_absolute_targets()`](https://prioritizr.net/reference/add_absolute_targets.md)
  and add_relative_targets\` related to their standardGeneric being
  incorrectly defined
- Reduce installation size using Dirk Eddelbuettel’s awesome advice:
  <http://dirk.eddelbuettel.com/blog/2017/08/14#009_compact_shared_libraries>
- Fix bug in `add_corridor_targets()` when argument `connectivities` is
  a `list`. The elements in the list are assumed to be `dsCMatrix`
  objects (aka symmetric sparse matrices in a compressed format) and are
  coerced to `dgCMatrix` objects to reduce computational burden. There
  was a typo, however, and so the objects were coerced to `dgCmatrix`
  and not `dgCMatrix`. This evidently was ok in earlier versions of the
  *RcppArmadillo* and/or *Matrix* packages but not in the most recent
  versions.

## prioritizr 1.0.1.2

### Minor improvements and bug fixes

- Fix bug in [`problem()`](https://prioritizr.net/reference/problem.md)
  causing node stack overflows
  ([\#21](https://github.com/prioritizr/prioritizr/issues/21)). Thanks
  to Dan Rosauer () for bug report.

## prioritizr 1.0.1.1

### Minor improvements and bug fixes

- Add *roxygen2* to package SUGGESTS for building vignettes.

## prioritizr 1.0.1.0

### Minor improvements and bug fixes

- Fix issue where
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
  returns `NA` on some systems preventing users from using the Gurobi
  solver–even when one thread is specified.

## prioritizr 1.0.0.5

### Minor improvements and bug fixes

- Fix building issue due to incorrect file order in DESCRIPTION.

## prioritizr 1.0.0.4

### Minor improvements and bug fixes

- Compatibility with R 3.4.0.
- Replace `structure(NULL, ...)` with `structure(list(), ...)`.
- Register compiled library files.
- Remove duplicate definition of
  [`new_waiver()`](https://prioritizr.net/reference/new_waiver.md).
- Update tests to skip if *prioritizrdata* package not installed.

## prioritizr 1.0.0.3

### Major changes

- Make `add_default_objectives()` and `add_default_targets()` private
  functions.

### Documentation updates

- Fix missing links in documentation.
- Fix typos in *roxygen2* parameters.
- Move `add_default_decisions()` and
  [`add_default_solver()`](https://prioritizr.net/reference/add_default_solver.md)
  to own help file.

## prioritizr 1.0.0.2

### Minor improvements and bug fixes

- Fix bug in
  [`rij_matrix()`](https://prioritizr.net/reference/rij_matrix.md)
  duplicating feature data
  ([\#13](https://github.com/prioritizr/prioritizr/issues/13)).

## prioritizr 1.0.0.1

### Minor improvements and bug fixes

- Fix *velox* package dependency
  ([\#8](https://github.com/prioritizr/prioritizr/issues/8)).
- Fix bug in
  [`add_corridor_constraints()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
  that fails to actually add the constraints with argument to
  `connectivity` is a list.
- Fix bug in `make install` command so that it now actually installs the
  package.

### Documentation updates

- Fix link to Joe’s website in the package’s website.

## prioritizr 1.0.0.0

### Notice

- R interface fully functional.

## prioritizr 0.1.2.9

### Major changes

- Package re-implementation.

## prioritizr 0.1.2

CRAN release: 2016-12-01

### Notice

- Prepare for CRAN submission.

### New features

- Introduce maximum target coverage model.

### Minor improvements and bug fixes

- Add continuous integration.
- Fixed various bugs.

### Documentation updates

- Add full vignette in addition to quickstart guide.

## prioritizr 0.1.1

### Major changes

- Initial package version.
