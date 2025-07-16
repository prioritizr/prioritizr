# prioritizr 8.0.6.8

#### Minor improvements and bug fixes

- Update `rij_matrix()` function to reduce run time.
- Update `solve()` function and the importance functions to ensure consistency
  their in output formats. Note that these changes do not alter their outputs.
- Update `eval_ferrier_importance()` function to better provide error messages
  with improved formatting.
- Update internal `any_nonNA()`, `any_nonzero()`, and `all_binary()` functions
  for processing raster data.
- Update internal `any_nonzero()` and `any_nonNA()` functions to provide better
  error messages.

# prioritizr 8.0.6.7

## New features

- New `calibrate_cohon_penalty()` function for automatically identifying
  a suitable penalty value for the penalties functions (#175). It is designed
  to work with any objective function and any of the penalty functions
  available in the package.
- New `add_neighbor_penalties()` function to reduce spatial fragmentation.
  This function is especially useful when working with large-scale problems or
  open source solvers.
- Update `add_cbc_solver()`, `add_gurobi_solver()`, and `add_highs_solver()`,
  functions with a new `control` parameter that can be used to manually
  specify additional parameters for customizing the optimization process (#354).

## Minor improvements and bug fixes

- Update problem formulation for `add_connectivity_penalties()`,
  `add_asym_connectivity_penalties()`, and `add_boundary_penalties()` to
  slightly improve solve times. In particular, instead of using binary
  variables to model the product of the planning unit decision variables,
  continuous variables are now used. The documentation for these functions
  has also been updated to mention this information. Thanks to Bistra Dilkina
  for the suggestion.
- Update `add_neighbor_constraints()` function so that setting `clamp = TRUE`
  is more likely to resolve infeasibility issues. In particular, setting
  `clamp = TRUE` will (i) limit the minimum number of neighbors for a given
  planning unit based on the locked out constraints of neighboring planning
  units and (ii) not apply this constraint to any locked in or locked out
  planning units.
- Update `add_min_shortfall_objective()` and
  `add_min_largest_shortfall_objective()` functions to employ a slightly
  different problem formulation that -- despite being functionally identical to
  the previous formulation -- has better performance for large-scale
  problems (#357). Thanks to Aboozar Mohammadi (\@AboozarM) for the suggestion.
- Update `write_problem()` function to support all the file formats supported by
  the Gurobi solver (per `gurobi::gurobi_write()`). Of particular note,
  this means that problems can now be saved in compressed file file format
  (e.g., `.mps.gz`).
- Update `add_cbc_solver()` function so that the `presolve` parameter
  can be used to specify the intensity of the presolve process. Similar to
  `add_gurobi_solver()`, the `presolve` parameter is now specified as an integer
  value. The default value is now 2, which specifies the most intensive
  level of presolve. For backwards compatibility, a value of `TRUE`
  is treated as a value of 1.
- Update `eval_target_coverage_amount()` so that the relative shortfall
  for each target is now calculated by dividing the absolute shortfall
  by the absolute target. This change is to ensure consistency with the minimum
  shortfall objective.
- Fix bug in internal `repr.list()` function that displayed duplicate class
  names.
- Fix bug in `adjacency_matrix()`, `compile()`, and `zone_names()` functions
  that caused an unhelpful error message when calling the function without
  any arguments.
- Update unit tests for `add_boundary_penalties()`,
  `add_connectivity_penalties()`, and `add_asym_connectivity_penalties()` to
  reduce run time.
- Fix bug in unit tests for `add_asym_connectivity_penalties()`, and
  `eval_rank_importance()` functions. Note that these bugs do not affect
  the correctness of the functions as implemented in the package.
- Classes are now exported to make it easier for reverse dependencies to add
  their own objectives, constraints, penalties, targets, and solvers.

## Documentation updates

- Fix mistake in `add_gurobi_solver()` function documentation for the
  `numeric_focus` parameter.
- Fix typo in equation for `add_max_utility_objective()` (#373). Thanks to
  Anthony Richardson (\@ric325) for bug report.
- Update Calibrating trade-offs vignette with new `calibrate_cohon_penalty()`
  function.
- Update package overview vignette with new `add_neighbor_penalties()` function.
- Update solver benchmarks vignette to remove unnecessary package dependencies.
- Update publication record.

# prioritizr 8.0.6.6

## Minor improvements and bug fixes

- Fix bug in `eval_rank_importance()` function that could lead to incorrect
  importance values when considering proportion or semi-continuous decision
  types (i.e., problems with `add_proportion_decisions()` or
  `add_semicontinuous_decisions()`).

## Documentation updates

- Update publication record.

# prioritizr 8.0.6.5

## Minor improvements and bug fixes

- Fix bug in `eval_rank_importance()` function that caused a superfluous
  warning to be thrown when locked constraints (i.e,
  `add_locked_in_constraints()`, `add_locked_out_constraints()`, or
  `add_manual_locked_constraints()`).

# prioritizr 8.0.6.4

## Minor improvements and bug fixes

- Update `add_locked_in_constraints()`, `add_locked_out_constraints()`,
  `add_manual_locked_constraints()`, and `add_manual_bounded_constraints()`
  functions so that planning units can be locked based on their planning unit
  identifier values when specifying `data.frame` planning units (#359). These
  functions have also been updated to provide more informative error messages
  when invalid data are specified. Thanks to Martin Jung (\@Martin-Jung) for
  bug report.
- Fix `eval_rank_importance()` to better account for proportion-type and
  semi-continuous decision types (#367). Thanks to Martin Jung (\@Martin-Jung)
  for bug report.
- Fix bug in `print()` and `summary()` functions for `problem()` objects that
  caused the functions to incorrectly show the classes of the planning unit
  data that inherit from multiple classes. For example, this means that
  `sf::st_sf()` planning units will now be shown as having `"sf"` data, rather
  than `"sftbl_dftbldata.frame"` data. Similarly, `tibble::tibble()` planning
  units will now be shown as `tbl_df` instead of `tbldfdata.frame`.
- Update internal `all_finite()` function to perform faster for `character`
  vector arguments.
- Update dependencies so that the _slam_ package is now an optional dependency.
  This is because the _slam_ package is only required when using
  `add_lpsymphony_solver()` and `add_gurobi_solver()`.
- Update `marxan_problem()` to provide better validation of input data
  and more informative error messages. This update also involves replacing the
  _data.table_ package with the _vroom_ package.
- Thanks to Sandra Neubert (\@sandra-neubert) for code review.

## Documentation updates

- Update `add_locked_in_constraints()`, `add_locked_out_constraints()`,
  `add_manual_locked_constraints()`, and `add_manual_bounded_constraints()`
  documentation to provide more detail on specifying which planning units
  should be constrained (#359). Thanks to Martin Jung (\@Martin-Jung) for bug
  report.
- Update README to thank Theodoros Ploumis (\@theodorosploumis) for the logo.
- Update documentation for `eval_rank_importance()`.
- Standardize terminology for referring to "cells" in raster data. Previously,
  some parts of the documentation referred to them as pixels.
- Update publication record.

# prioritizr 8.0.6.3

## Minor improvements and bug fixes

- Fix bug in `add_manual_targets()` that caused segmentation faults when
  invalid arguments to `data` were specified (#363).

## Documentation updates

- Update publication record.

# prioritizr 8.0.6.2

## Minor improvements and bug fixes

- Update internal functions (i.e., `all_match_of`, and `is_match_of()`) for
  validating arguments to be compatible with `character` vectors produced
  using the _glue_ package (#360). Thanks to Dan Wismer (\@DanWismer) for bug
  report.

## Documentation updates

- Update `eval_feature_representation_summary()` documentation to improve
  description of the output data frame (#355). Thanks to Sam Bradshaw
  (\@sam-bradshaw-wcmc) for bug report.

# prioritizr 8.0.6.1

## Minor improvements and bug fixes

- Update `boundary_matrix()` calculations to maintain compatibility with
  updates to the _terra_ package.
- Thanks to Sandra Neubert (\@sandra-neubert) for code review.

## Documentation updates

- Update publication record.
- Update package citation.

# prioritizr 8.0.6

## Notice

- CRAN release.

## New features

- New `add_min_penalties_objective()` function to generate solutions that
  focus on minimizing the penalties -- as much as possible -- whilst ensuring
  that (i) the cost of the solution does not exceed a budget and (ii) all
  feature representation targets are met. This function is designed to aid with
  hierarchical multi-objective optimization. It can be now used instead of
  specifying a minimum set objective with zero costs and
  a linear constraint to specify the budget.
- New `add_rank_importance()` function to evaluate the relative importance of
  planning units selected in a solution (#337). Briefly, this approach involves
  generating incremental prioritizations with increasing budgets, wherein
  planning units selected in a previous increment are locked in to the
  following solution. Additionally, locked out constraints are used to ensure
  that only planning units selected in the original solution are available for
  selection. The advantages of this approach are that it can (i) be computed
  relatively quickly for relatively large problems, (ii) account for the cost
  of different planning units, (iii) account for multiple management zones, and
  (iv) apply to solutions generated using any objective function.

## Minor improvements and bug fixes

- Update `adjacency_matrix()`, `intersecting_units()`, `rij_matrix()`,
  `eval_target_coverage_summary()`, and `problem()` to improve processing speed.
- Update `intersecting_units()` to accommodate very large `sf::st_sf()` objects.
- Update `rij_matrix()` so that it has an optional `idx` parameter which
  can be used to specify planning unit indices if they have been pre-computed.
  This new parameter can be used to help speed up calculations.
- Update `add_locked_in_constraints()` and `add_locked_out_constraints()` to
  throw error messages with standardized grammar.
- Update internal functions for processing `sf::st_sf()` objects to improve
  processing speed.
- Update `add_cplex_solver()` error message for failing to initialize CPLEX.
- Update error message for internal matrix conversion function.
- Fix typo in error for importance and evaluation functions that
  is thrown when attempting to use a `solution` that is a different
  class than the planning units in `x`.
- Fix bug with warnings displaying the name of internal functions instead
  of user facing functions.
- Fix typo in citation information.

## Documentation updates

- Update Package overview and Calibrating trade-offs vignettes with information
  on the `add_min_penalties_objective()` function.
- Update `add_max_utility_objective()` documentation to make it clear that
  the function is simply maximizing a weighted sum of the features.
- Update publication record.
- Update Calibrating trade-offs vignette to improve internal logic for
  determining the best guess penalty value for preliminary prioritizations.
- Small documentation improvements. Thanks to Sandra Neubert (\@sandra-neubert).

# prioritizr 8.0.5

## Notice

- Abandoned candidate for CRAN release. Although this version was originally
  a candidate for CRAN release, we decided to add more functionality.

# prioritizr 8.0.4.5

## Minor improvements and bug fixes

- Update `rij_matrix()` to improve memory efficiency when processing
  large-scale vector (e.g., `sf::st_sf()`) planning unit data. Thanks to
  Sahebeh Karimi for bug report.
- Update `rij_matrix()` so that the `memory = TRUE` parameter can be used to
  reduce memory requirements by processing each feature layer separately. This
  parameter can now be used when processing both vector or raster planning
  unit data (previously it could only be used with raster planning unit data).

## Documentation updates

- Fix DOI for citation.
- Fix citations in package overview vignette and package manual entry to
  pass package checks.

# prioritizr 8.0.4.4

## Documentation updates

- Update package citation.
- Update documentation for package manual entry.
- Update `add_asym_connectivity_penalties()`, `add_connectivity_penalties()`
  `marxan_connectivity_data_to_matrix()` documentation so that examples
  are standalone and do not affect the session by loading packages.
- Update `marxan_boundary_data_to_matrix()` and
  `marxan_connectivity_data_to_matrix()` documentation so that examples
  provides more information on how the functions work.
- Fix equation rendering in online documentation (#344). Thanks to Jason
  Everett (\@jaseeverett) and Anthony Richardson (\@ric325) for bug report.

# prioritizr 8.0.4.3

## Documentation updates

- Update `marxan_boundary_data_to_matrix()` and
  `marxan_connectivity_data_to_matrix()` documentation so that examples
  are standalone and do not affect the session by loading packages.

# prioritizr 8.0.4.2

## Minor improvements and bug fixes

- Fix bug in `add_max_utility_objective()` that caused the optimization
  process to throw an error about problem infeasibility when using
  feature data that contain negative values (#334). Thanks to \@hannahmp
  for bug report.
- Fix bug in `presolve_check()` that would cause it to erroneously suggest that
  many planning units don't have any feature data associated with them. This
  bug was caused when the feature data contained relatively large, negative values.
- Fix bug in `binary_stack()` that caused it to throw an error when working
  with raster data containing zeros (#333).
- Fix bug in `add_absolute_targets()` where it would not throw a warning to
  the user know that a problem already had targets defined, and so adding
  the new targets would override the existing targets defined for the problem.
- Fix bug in `as.ZonesRaster` that resulted in an error when trying to
  convert a `SpatRaster` zones object (i.e., a `zones` object with _terra_
  package data) into `Raster` zones object (i.e., a `zones` object with
  _raster_ package data).
- Fix bug in `write_problem()` needlessly printing messages about the
  _gurobi_ package not being installed when the function is trying to
  automatically determine which solver to use (i.e., when using
  `solver = NULL`) and the package is not is available.
- Fix bug in `branch_matrix()` where it would not automatically convert
  object to the `phylo` class in the _ape_ package.
- Update warning message for `add_asym_connectivity_penalties()` so that
  it now specifies that asymmetric connectivity values are required when
  symmetric values are incorrectly supplied (#339). Thanks to \@DanWismer for
  bug report.
- Update warning messages so that they now indicate which function threw
  the warning message (using `rlang::warn()`).
- Update `compile()` so that it throws an error when using the expanded
  version of a problem formulation with negative feature values. This
  is because the expanded version of the problem formulations are not
  compatible with negative feature values. Currently, the expanded
  version of the problem formulation is only required when using
  `add_feature_contiguity_constraints()`.
- Additional tests to improve test coverage.
- Small improvements to code style, maintainability, and logic thanks to code
  review by Sandra Neubert (\@sandra-neubert).

## Documentation updates

- Update publication record.
- Fix cross-reference linking issues to classes in other packages (#340).

# prioritizr 8.0.4.1

## Minor improvements and bug fixes

- Fix issue with `print()` and `summarize()` not displaying correct text for
  linear constraints (#330).

# prioritizr 8.0.4

## Notice

- CRAN release.

# prioritizr 8.0.3.7

## Notice

- New default portfolio method for `problem()` objects. This
  new default portfolio -- which can be manually specified using
  `add_default_portfolio()` -- involves simply generating a single solution.
  The reason why this new default portfolio method was chosen was because
  planning problems that contain insufficient data (e.g., feature and cost
  data) to identify meaningful priorities can sometimes result in solutions
  containing strange spatial artifacts (e.g., lines or bands of selected
  planning units, see #205 and #268). Since the presence of these spatial
  artifacts can indicate an under-specified problem and shuffling
  optimization problems can suppress them, we have
  decided to update the default portfolio so that it does not shuffle problems.
  If users wish to prevent spatial artifacts from appearing in solutions, then
  spatial penalties (e.g., `add_boundary_penalties()`), spatial constraints
  (e.g., `add_neighbor_constraints()`), or shuffle portfolios
  (e.g., `add_shuffle_portfolio(number_solutions = 1)`) can be used.

## Minor improvements and bug fixes

- New `add_default_portfolio()` function for specifying the default
  behavior for generating a solution (see Notice above for further details).
- Update `solve()` so that it provides information on the optimality of
  solutions (#323). For example, you might specify a 10% optimality gap
  for the optimization process (e.g., using `add_highs_solver(gap = 0.1)`), and
  this might produce a solution that is at least 7% from optimality. The
  resulting output from `solve()` will now provide this information about
  the solution (i.e., the 7% from optimality), and can be accessed
  using the `gap` attribute (e.g., `attr(x, "gap")`, where `x` is the output
  from `solve()`). Note that this information is currently only available when
  using the Gurobi or HiGHS solvers.
- Fix bug in `add_linear_constraints()` and `add_linear_penalties()` that
  resulted in an incorrect error message being shown (#324).
- Fix bug in `add_shuffle_portfolio()` that prevented solvers from using a
  pre-specified starting solution (per the `start` parameter) correctly.
  Please note that this bug did not result in incorrect solutions, it only
  meant that any pre-specified starting solutions were not used properly.
- Fix bug in `add_cplex_solver()` that caused solutions to not provide
  runtime information for the optimization process.

# prioritizr 8.0.3.6

## Minor improvements and bug fixes

- Fix bug in `add_shuffle_portfolio()` so that optimization problems are
  randomly shuffled when a single solution is requested. This update should
  help prevent "strange" solutions that contain long horizontal lines/bands of
  planning units (#205, #268).
- Update `add_contiguity_constraints()` and
  `add_feature_contiguity_constraints()` to be compatible with updates to
  the _igraph_ package.
- Update `write_problem()` so that it can use the _gurobi_ package to write
  problems (if desired). This substantially reduces run time, because writing
  problems using the _Rsymphony_ packages also requires solving them.

## Documentation updates

- Update publication record.

# prioritizr 8.0.3.5

## Minor improvements and bug fixes

- Update `presolve_check()` to throw warning if a problem has a single feature
  (#309). Thanks to Sandra Neubert (\@sandra-neubert) for code contribution.
- Update `print()` and `summary()` for `problem()` objects so that all
  text is printed at once (rather than sequentially).
- Fix `write_problem()` so that it works as expected (#312).
- Update `problem()`, `add_linear_constraints()`, `add_linear_penalties()`,
  `add_locked_in_constraints()`, `add_locked_out_constraints()`,
  `adjacency_matrix()`, `binary_stack()`, `category_layer()`,
  `connectivity_matrix()`,`fast_extract()`,  `intersecting_units()`,
  `proximity_matrix()`, `rij_matrix()`, `simulate_data()`,
  `simulate_species()`, `simulate_cost()`, and `zones()` and other functions so
  that they will throw an error if a categorical `terra::rast()` object is
  provided as an argument (#313). This is because categorical rasters are not
  supported. Thanks to Martin Jung (\@Martin-Jung) for bug report.
- Fix NAMESPACE issues related to registration of internal S3 methods.
- Fix bug with `problem()` not throwing multiple warnings with unusual data
  (e.g., given cost and feature data with negative values, previously
  only a single warning about negative costs would be thrown).
  Thanks to Sandra Neubert (\@sandra-neubert) for bug report.

## Documentation updates

- Update publication record.
- Update package-level manual entry.
- Update URLs.

# prioritizr 8.0.3.4

## Minor improvements and bug fixes

- Update `problem()` to be more memory efficient when using a sparse matrix
  (`dgCMatrix`) argument for the `rij_matrix` parameter.
- Update error messages for checking that objects have the same coordinate
  reference system and overlapping spatial extents to format argument names
  correctly.
- Update error messages for nested expressions to refer to expressions using
  `Caused by error` instead of `Caused by NULL`.

## Documentation updates

- Update publication record.

# prioritizr 8.0.3.3

## Minor improvements and bug fixes

- Fix `add_locked_in_constraints()` and `add_locked_in_constraints()` error
  messages when supplying `locked_in` and `locked_out` objects
  that do not spatially intersect with the planning units.
- Update error message for checking if objects spatially overlap to improve
  clarity.

## Documentation updates

- Update publication record.

# prioritizr 8.0.3.2

## Documentation updates

- Update URLs.

# prioritizr 8.0.3.1

## Minor improvements and bug fixes

- Fix aliasing for package manual entry (#301).

## Documentation updates

- Update publication record.

# prioritizr 8.0.3

## Notice

- CRAN release.

# prioritizr 8.0.2.7

## Notice

- We have developed a better approach for rescaling boundary data to
  avoid numerical issues during optimization (#297). Earlier versions of the
  package recommended the use of the `scales::rescale()` to rescale such data.
  However, we now realize that this approach can produce inconsistencies for
  boundary length data (e.g., the total perimeter of a planning unit might not
  necessarily equal the sum of the edge lengths). In some cases, these
  inconsistencies can cause solutions generated with high boundary
  penalties (i.e., using `add_boundary_penalties()` with a high `penalty`
  value) to contain a large reserve (i.e., a spatial cluster of selected of
  planning units) with a single unselected planning unit in the middle of the
  reserve. In the the worst case, these inconsistencies produce a situation
  where increasing boundary penalties (i.e., generating multiple solutions with
  `add_boundary_penalties()` and increasing `penalty` values)
  does not alter the spatial configuration of solutions. Although use of
  `scales::rescale()` did not produce such behavior prior to version 8.0.0,
  changes to the output format for `boundary_matrix()` in subsequent versions
  now mean that `scales::rescale()` can cause these issues. We now recommend
  using the new `rescale_matrix()` function to rescale boundary length data to
  avoid numerical issues, whilst also avoid such inconsistencies.

## New features

- New `rescale_matrix()` function to help with rescaling boundary length
  (e.g., generated using `boundary_matrix()`) and connectivity
  (e.g., generated using `connectivity_matrix()`) data so avoid
  numerical issues during optimization (#297). Thanks to
  Jason Flower (\@jflowernet) and Joan Giménez Verdugo for bug reports.

## Minor improvements and bug fixes

- Update the `print()` and `summary()` methods for `problem()` objects
  so that they will now better describe situations when the planning cost
  data all contain a constant value (e.g., all costs equal to 1).

## Documentation updates

- Update examples and vignettes to use the `rescale_matrix()` function
  instead of the `scales::rescale()` function for rescaling boundary
  length and connectivity data (#297).
- Update publication record.

# prioritizr 8.0.2.6

## New features

- Update `add_neighbors_constraints()` so that it has an additional
  `clamp` argument so the minimum number of neighbors permitted for
  each planning unit in the solution is clamped to the number of neighbors that
  each planning unit has. For example, if a planning unit has 2 neighbors,
  `k = 3`, and `clamp = FALSE`, then the planning unit could not
  ever be selected in the solution. However, if `clamp = TRUE`, then
  the planning unit could potentially be selected in the solution if both of
  its 2 neighbors were also selected.

## Minor improvements and bug fixes

- Fix issue with `problem()` that prevents `features` being supplied as
  a `data.frame` that contains feature names stored as a `factor` (#295).
  Thanks to Carl Boetigger (\@cboettig) for bug report.

## Documentation updates

- Fix URLs.

# prioritizr 8.0.2.5

## Minor improvements and bug fixes

- Update `problem()` so that it will throw a meaningful error message if the
  user accidentally specifies the geometry column for `sf` planning unit data
  as a feature.

# prioritizr 8.0.2.4

## Minor improvements and bug fixes

- Fix compatibility with updates to _terra_ package.
- Fix `rij_matrix()` so that it works when none of the raster layers being
  processed fit into memory (#290). Thanks to Edwards Marc (\@edwardsmarc) for
  bug report.
- Fix spatial extent of built-in raster datasets so that extents are between
  0 and 1 (i.e., `get_sim_pu_raster()`, `get_sim_locked_in_raster()`,
  `get_sim_locked_out_raster()`, `get_sim_zones_pu_raster()`,
  `get_sim_features()`, `get_sim_zones_features()`).
- Update `add_manual_locked_constraints()` and
  `add_manual_bounded_constraints()` so that the indices in the
  specified in the argument `data$pu` should consistently refer to the total
  units. In other words, the indices in `data$pu` should refer to the row
  numbers (for planning units in `sf` or `data.frame` format) or cell numbers
  (for planning units in `Raster` or `SpatRaster` format) of the planning units
  that should be locked.
- Fix warnings thrown due to package version comparisons.

## Documentation updates

- Update publication record.

# prioritizr 8.0.2.3

## Minor improvements and bug fixes

- Export `solve.ConservationProblem()` so that it can be called directly
  (#283). Thanks to Tin Buenafe (\@SnBuenafe) for bug report.

## Documentation updates

- Update publication record.

# prioritizr 8.0.2.2

## Minor improvements and bug fixes

- Fix compatibility with _highs_ package (version 0.1-10) (#281).

## Documentation updates

- Update publication record.

# prioritizr 8.0.2.1

## Minor improvements and bug fixes

- Update `problem()` so that an error will be thrown if argument to `features`
  contains only missing (`NA`) values (e.g., an _sf_ object is supplied that
  has `NA` values in all rows for a feature's column).

## Documentation updates

- Update publication record.

# prioritizr 8.0.2

## Notice

- The package has been updated to focus on using the _sf_ and _terra_ package
  for spatial vector and raster datasets. This is because the _sf_ package is
  the successor to the _sp_ package, and the _terra_ package is the successor
  to the _raster_ package. By leveraging these newer packages, the _prioritizr_
  package can provide better performance. Although _sp_ and _raster_ package
  classes (e.g., `raster::stack()` and `sp::SpatialPolyonsDataFrame()`)
  are still supported, the _prioritizr_ package will now throw deprecation
  warnings. Since support for the _sp_ and _raster_ package classes
  will be fully deprecated and removed in a later version this year, we
  recommend updating code to use the _sf_ and _terra_ packages.

## Major changes

- All _proto_ classes have been migrated to _R6_ classes. This update reduces
  memory usage (#238), so `problem()` objects can now contain many more
  constraints and penalties. Note that any `problem()` objects
  that were produced using earlier versions of the package are no longer
  compatible. Thanks to Jason Flower (\@jflowernet) for bug report on memory issues.
- The _proto_, _raster_, _sf_, _sp_ packages are no longer automatically
  loaded alongside _prioritizr_. As such, users will need to load them manually
  (e.g., using `library(sf)`).
- The built-in datasets have been removed and replaced with functions
  to import them as needed (i.e., `get_sim_pu_raster()`,
  `get_sim_pu_polygons()`, `get_sim_pu_lines()`, `get_sim_pu_points()`,,
  `get_sim_locked_in_raster()`, `get_sim_locked_out_raster()`,
  `get_sim_zones_pu_raster()`, `get_sim_zones_pu_polygons()`,
  `get_sim_phylogeny()`, `get_sim_features()`, `get_sim_zones_features()`).
  These functions now return `sf::st_sf()`,
  `terra::rast()`, `ape::read.tree()` and `zones()` objects.
  Note that these functions are provided because `data(...)` cannot be
  used with `terra::rast()` objects. See `?data` for more information.
- The `boundary_matrix()` output format has been updated. This means that
  users will not be able to use boundary data generated using previous
  versions of the package.
- The `add_lpsymphony_solver()` now throws an error, instead of a warning,
  if an old version of the _lpsymphony_ package is installed that is known
  to produce incorrect results.
- The `marxan_boundary_data_to_matrix()` function is no longer compatible
  with boundary data for multiple zones.
- The `distribute_load()` function has been deprecated, because it is no
  longer used. For equivalent functionality, See `parallel::splitIndices()`.
- The `new_optimization_problem()` and `predefined_optimization_problem()`
  functions have been superseded by the new `optimization_problem()` function.
- To simplify package documentation and functionality, the following functions
  are no longer exported: `is.Waiver()`, `add_default_decisions()`
  `new_id()`, `is.Id()`, `print.Id()`, `pproto()`.
- Updates to improve the error messages and error message handling.
  Hopefully, users should no longer see `"bad error message"`!

## New features

- The `print()` function for `problem()`, `optimization_problem()`, and
  `zones()` objects has been updated to provide more information.
- New `summary()` function to provide extensive detail on `problem()` objects.

## Minor improvements and bug fixes

- Fix bug for `add_feature_weights()` when applied to problems with
  an `add_max_phylo_div_objective()` or `add_max_phylo_end_objectve()`.
  Specifically, the bug meant that weights weren't being applied to
  problems with these particular objectives.
- Fix instructions in `add_gurobi_solver()` documentation for opening vignette.
- Update solver functions to provide instructions for installing
  dependencies in error messages when their dependencies are not available.
- To ensure consistency among the portfolio functions, all of them (except for
  `add_extra_portfolio()`) default to generating 10 solutions.
- The `solve()` function will now output `tibble::tibble()` objects
  (instead of `data.frame()` objects), when the planning unit data are
  `tibble::tibble()` objects.
- The `boundary_matrix()` function now uses `terra::sharedPaths()` for
  calculations, providing greater performance (#257). Thanks to Jason Flower (\@jflowernet) for bug report.
- The `eval_ferrier_importance()` function can now be used with
  any objective function that uses targets and a single zone.
- Fix CRAN note regarding C++ standards (#263).
- Remove _doParallel_ and _plyr_ packages as dependencies by simplifying
  the `add_shuffle_portfolio()` and `eval_replacement_importance()` functions.
- Fix `add_linear_penalties()` function so that the penalty parameter is applied
  correctly (#342). In previous versions, this bug meant that solving a problem
  with `penalty = 1` would produce solution based on `penalty = -1` (and vice
  versa). Additionally, this bug also meant that compiling/solving a problem
  multiple times would cause the formulation to alternate between using
  `penalty = 1` and `penalty = -1`. Thanks to Carina Firkowski
  (\@Carina-Firkowski) for bug report.

## Documentation updates

- Assorted tweaks to improve writing in the vignettes and documentation.
  Many thanks to Marc Edwards (\@edwardsmarc) for code contribution!
- Update publication record.

# prioritizr 8.0.1

## Minor improvements and bug fixes

- Assorted bug fixes.

# prioritizr 8.0.0

## Notice

- This version contains an incorrect version of the code, due to a mistake
  when preparing for CRAN release. We strongly recommend upgrading to
  version 8.0.1 to ensure correct results. We apologize any inconvenience this
  might have caused.

# prioritizr 7.2.2.7

## Documentation updates

- Update README badges.
- Update publication record.

# prioritizr 7.2.2.6

## Documentation updates

- Update publication record.

# prioritizr 7.2.2.5

## Documentation updates

- Update publication record.

# prioritizr 7.2.2.4

## Minor improvements and bug fixes

- Specify minimum version for Matrix package dependency (#255). Thanks to
  Bruno Carturan (\@BrunoCartu) for bug report.

# prioritizr 7.2.2.3

## New features

- New `add_highs_solver()` function for the HiGHS optimization software (#250).

## Minor improvements and bug fixes

- Update `add_default_solver()` to use the HiGHS solver if the Gurobi, IBM
  CPLEX, and CBC solvers aren't available.
- Fix `add_default_solver()` so that the `add_lpsymphony_solver()` is used
  instead of `add_rsymphony_solver()`.

# prioritizr 7.2.2.2

## Minor improvements and bug fixes

- Update `problem()` and `eval_feature_representation_summary()` to avoid
  needlessly converting sparse matrices to regular matrices (#252).

## Documentation updates

- Fix URLs.

# prioritizr 7.2.2.1

## Documentation updates

- Fix mistake in `NEWS.md`.
- Update publication record.

# prioritizr 7.2.2

## Notice

- Release candidate for CRAN.

## Minor improvements and bug fixes

- Fix compiler warnings.
- Update tests to skip long-running tests on CRAN.

## Documentation updates

- Update examples to minimize overall package check timings for CRAN.

# prioritizr 7.2.1

## Notice

- Release candidate for CRAN (rejected due to package check timings).

# prioritizr 7.2.0.9

## Major changes

- Update `boundary_matrix()` to use STR query trees by default.
- Update `simulate_data()`, `simulate_cost()` and `simulate_species()`
  functions to improve performance using the _fields_ package.

## Minor improvements and bug fixes

- Fix compatibility with upcoming _Matrix_ package version (version 1.5-0).
- Update package documentation to provide details for obtaining and installing
  the _cplexAPI_ package since it has been archived on CRAN (#214).
- Fix bug that caused the `add_cbc_solver()` to throw a segfault when solving
  a problem wherein the `rij_matrix(x)` has a zero amount for the last feature
  in the last planning unit (#247). Thanks to Jason Everett (\@jaseeverett) for
  bug report.
- Remove _maptools_, _PBSmapping_, and _rgeos_ packages as dependencies.
  This involved updating the unit tests to hard-code correct results,
  updating examples to use the _sf_ package, and updating the
  `boundary_matrix()` to use the _geos_ package (#218).

## Documentation updates

- Fix URLs.

# prioritizr 7.2.0.8

## Major changes

- Update `simulate_cost()` and `simulate_species()` so that they no longer
  depend on the _RandomFields_ package. Note that these functions will now
  produce different outputs from previous versions (even when controlling
  for the random number generator state).

# prioritizr 7.2.0.7

## Documentation updates

- Update publication record.

# prioritizr 7.2.0.6

## Documentation updates

- Update publication record.

# prioritizr 7.2.0.5

## Documentation updates

- Update publication record.

# prioritizr 7.2.0.4

## Documentation updates

- Fix URLs.

# prioritizr 7.2.0.3

## Minor improvements and bug fixes

- Update the `presolve_check()` function to (i) reduce chances of
  it incorrectly throwing an error when the input data won't actually
  cause any issues, and (ii) provide recommendations for addressing issues.

## Documentation updates

- Update documentation for `add_min_largest_shortfall_objective()` so that
  examples complete in a shorter period of time.

# prioritizr 7.2.0.2

## Minor improvements and bug fixes

- Fix bug in processing planning unit data when a problem is constructed
  using arguments to (i) `x` that are `numeric` or `matrix` format, (ii)
  `x` that contain missing (`NA`) values, and (iii) `rij_matrix` that
  are in `dgCMatrix` format. This bug only occurred when all three of these
  specific conditions were met. When it occurred, the bug caused planning units
  with `NA` cost values to receive very high cost values (e.g., 1e+300).
  This bug  meant that when attempting to solve the problem, the
  presolve checks (per `presolve_check()`) would throw an error complaining
  about very high cost values (#236). Thanks to \@lmathon for bug report.

# prioritizr 7.2.0.1

## Documentation updates

- Update publication record.

# prioritizr 7.2.0.0

## Major changes

- Update `add_connectivity_penalties()` function and documentation so that
  it is designed specifically for symmetric connectivity data.

## New features

- New `add_asym_connectivity_penalties()` function that is designed
  specifically for asymmetric connectivity data. This function has been
  created to help ensure that asymmetric connectivity data are handled
  correctly. For instance, using asymmetric connectivity data with
  `add_connectivity_penalties()` function in previous versions of the package
  sometimes resulted in the data being incorrectly treated as symmetric data.
  Additionally, this function uses an updated mathematical formulation
  for handling asymmetric connectivity so that it provides similar
  results to the _Marxan_ software (#223). Thanks to Nina Faure Beaulieu
  (\@ninzyfb) for bug report.

## Minor improvements and bug fixes

- Fix `add_locked_in_constraints()` and `add_locked_out_constraints()`
  to ensure that a meaningful error message is provided when no planing
  units are locked (#234). Thanks to Alec Nelson (\@AlecNelson) for bug report.
- Fix `presolve_check()` so that it does not throw a meaningless warning
  when the mathematical objective function only contains zeros.
- Update `presolve_check()` to help reduce chances of mis-attributing
  high connectivity/boundary values due to planning unit costs.
- Update `marxan_problem()` function so that it can be used with asymmetric
  connectivity data. This is now possible because there are dedicated functions
  for symmetric and asymmetric connectivity.

## Documentation updates

- Update publication record.
- Update URLs in publication record so that they pass CRAN checks.

# prioritizr 7.1.1.12

## Documentation updates

- Update publication record.

# prioritizr 7.1.1.11

## Documentation updates

- Update publication record.

# prioritizr 7.1.1.10

## Documentation updates

- Update publication vignette.
- Fix URLs.
- Improve documentation for the `zones` parameter of the
  `add_connectivity_penalties()` function.

# prioritizr 7.1.1.9

## Documentation updates

- Update documentation for `eval_ferrier_importance()` (#220). Although this
  function is now recommended for general use, the documentation
  contained an outdated warning and so the warning has now been removed.

# prioritizr 7.1.1.8

## Minor improvements and bug fixes

- Fix bug so that the `eval_n_summary()` function now returns a table with
  the column name `"n"` (instead of `"cost"`) for the number
  of selected planning units (#219).

## Documentation updates

- Update publication record.
- Update reference index for package website.
- Fix minor typos in vignettes.

# prioritizr 7.1.1.7

## Minor improvements and bug fixes

- Minimum version numbers are now provided for all R package dependencies
  (excepting base R packages) (#217).
- The _data.table_ package is now listed as a suggested (optional) dependency.
  This is because it is only used by the `marxan_problem()` for importing
  _Marxan_ data files.

# prioritizr 7.1.1.6

## Documentation updates

- Update publication record.

# prioritizr 7.1.1.5

## Documentation updates

- The _Tasmania tutorial_ has been reworked into the _Getting started_ tutorial. This tutorial now provides short introduction to using the package.
- The _Salt Spring Island tutorial_ has been reworked into the _Connectivity tutorial_. This tutorial now explores different approaches for incorporating connectivity.
- The _prioritizr_ vignette has been renamed to the _Package overview_ vignette.
- New _Calibrating trade-offs tutorial_ showcasing methods for running calibration analyses. It outlines blended and hierarchical approaches for generating a set of different prioritizations based on different parameters. It also covers different approaches for selecting a candidate prioritization based on different trade-offs.

# prioritizr 7.1.1.4

## Minor improvements and bug fixes

- Update tests to reduce run time and pass given slightly different results
  with new Gurobi version (9.5.0).
- Update built-in `sim_pu_sf` and `sim_pu_zones_sf` data given class
  updates to the _sf_ package (compatible with version 1.0.3+).

## Documentation updates

- Update example for `write_problem()` function.

# prioritizr 7.1.1.3

## Documentation updates

- Update publication record.

# prioritizr 7.1.1.2

## Documentation updates

- Update publication record.

# prioritizr 7.1.1.1

## Documentation updates

- Fix URL in vignette.

# prioritizr 7.1.1

## Notice

- Release candidate for CRAN.

# prioritizr 7.1.0.4

## Notice

- Brandon Edwards added to package author list.

## Major changes

- Update `eval_ferrier_importance()` function with verified code.

## Minor improvements and bug fixes

- Update `presolve_check()` function to throw warning when
  really high values specified in `add_neighbor_constraints()`.

## Documentation updates

- Update documentation with information about Ferrier importance scores.
- Update Gurobi Installation guide vignette.
- Update benchmark vignette.

# prioritizr 7.1.0.3

## Minor improvements and bug fixes

Update `add_cbc_solver()` function so that it can use a starting solution to reduce run time (via the `start_solution` parameter).

# prioritizr 7.1.0.2

## Minor improvements and bug fixes

- Remove _xtable_ package from Suggests because it is no longer used.

## Documentation updates

- Fix examples that fail package checks.

# prioritizr 7.1.0.1

## Documentation updates

- Update publication vignette.

# prioritizr 7.1.0.0

## New features

- New `add_linear_constraint()` function to add arbitrary constraints.

## Minor improvements and bug fixes

- Update `add_min_shortfall_objective()` and
  `add_min_largest_shortfall_objective()` functions to handle targets with
  a target threshold value of zero.

## Documentation updates

- Minor improvements to the documentation. These include moving mathematical
  details to dedicated sections, providing more links in the See also sections,
  fixing text formatting for the `eval_connectivity_summary()` function,
  and tweaking the header in the README.
- Update publication vignette.

# prioritizr 7.0.1.5

## Documentation updates

- Update publication vignette.

# prioritizr 7.0.1.4

## Documentation updates

- Update documentation and examples for `problem()` function.
- Update publication vignette.

# prioritizr 7.0.1.3

## Documentation updates

- New solver benchmark vignette.

# prioritizr 7.0.1.2

## Minor improvements and bug fixes

- Update `add_gurobi_solver()` function so that it doesn't print excess
  debugging information (accidentally introduced in previous version 7.0.1.1).

# prioritizr 7.0.1.1

## New features

- Update `add_gurobi_solver()` function to support the `node_file_start`
  parameter for the Gurobi software. This functionality is useful solving large
  problems on systems with limited memory (#192). Thanks to \@negira and Alec Nelson (\@AlecNelson) for bug reports and suggestions.

# prioritizr 7.0.1

## Notice

- Release candidate for CRAN.

## Documentation updates

- Update DESCRIPTION with more information on the package usage.
- Update DESCRIPTION with details on _rcbc_ package installation.

# prioritizr 7.0.0.8

## New features

- New `write_problem()` function to save the mixed integer programming
  representation of a conservation planning problem to a file. This
  function is useful for manually executing optimization solvers.

# prioritizr 7.0.0.7

## Documentation updates

- Fix typo in `rij_matrix()` function documentation (#189).
- Update publication vignette.

# prioritizr 7.0.0.6

## Minor improvements and bug fixes

- Update `add_gurobi_solver()` function to allow specification of a starting
  solution (#187). This functionality is useful for conducting a boundary
  penalty parameter calibration exercise. Specifically, users can specify the
  starting solution for a given penalty value based on the solution
  obtained using a smaller penalty value.
- Fix `solve()` so it assigns layer names based on zone names for solutions in
  raster format.

# prioritizr 7.0.0.5

## Minor improvements and bug fixes

- Update methods for calculating solver runtime.

# prioritizr 7.0.0.4

## Minor improvements and bug fixes

- Fix `add_cbc_solver()` so that `time_limit` and `verbose` parameters work
  as expected.

## Documentation updates

- Update publication record.

# prioritizr 7.0.0.3

## Minor improvements and bug fixes

- Update `add_gurobi_solver()` function to report timings following the same
  methods as the other solvers.

# prioritizr 7.0.0.2

## Minor improvements and bug fixes

- Update `add_lpsymphony_solver()` function to be more memory efficient (#183).
- Added _slam_ package to dependencies to enable more memory efficient
  usage of the _lpsymphony_ package (#183).

# prioritizr 7.0.0.1

## Minor improvements and bug fixes

- Update unit tests to solve a greater proportion of them using continuous
  integration services (#181).
- Update `add_default_solver()` so that `add_cbc_solver()` is now preferred
  over all other open source solvers.
- Fix bug in `add_cbc_solver()` that resulted in incorrect solutions to
  problems with equality constraints.

# prioritizr 7.0.0.0

## Minor improvements and bug fixes

- Remove unused _shiny_ package integration and dependencies (#141).

# prioritizr 6.0.0.2

## New features

- New `add_cbc_solver()` function to generate solutions using the open source
  CBC solver via the _rcbc_ package (https://github.com/dirkschumacher/rcbc).

## Minor improvements and bug fixes

- Update `add_rsymphony_solver()` and `add_lpsymphony_solver()` functions to
  have a default `time_limit` argument set as the maximum machine integer for
  consistency.
- Update `add_rsymphony_solver()`, `add_lpsymphony_solver()`, and
  `add_gurobi_solver()` functions to require `logical` (`TRUE`/`FALSE`)
  arguments for the `first_feasible` parameter.
- Update `add_default_solver()` function so that it prefers
  `add_lpsymphony_solver()` over `add_rsymphony_solver()`, and
  `add_cbc_solver()` over all open source solvers.

## Documentation updates

- Previous versions of the package reported that the `gap` parameter
  for the `add_rsymphony_solver()` and `add_lpsymphony_solver()` corresponded
  to the maximum absolute difference from the optimal objective value.
  This was an error due to misunderstanding the _SYMPHONY_ documentation.
  Under previous versions of the package, the `gap` parameter actually
  corresponded to a relative optimality gap expressed
  as a percentage (such that`gap = 10` indicates that solutions must be at
  least 10% from optimality). We have now fixed this error and the documentation
  described for the `gap` parameter is correct. We apologize for any
  inconvenience this may have caused.
- Update documentation for solvers to provide more detailed information.
- Update publication record.

# prioritizr 6.0.0.1

## New features

- New `add_min_largest_shortfall()` objective function.

## Minor improvements and bug fixes

- Add more helpful error messages when invalid `solution` arguments are
  supplied to the evaluation functions (#176). Thanks to Phil Dyer (\@PhDyellow)
  for bug report.
- Add functionality to calculate importance scores using the Ferrier method
  with `sf` planning unit data.

## Documentation updates

- Update Solution format section documentation for evaluation functions
  (i.e. all functions starting with `eval_`) to mention that
  the argument to `solution` should only contain columns that correspond to
  the solution (#176). Thanks to Phil Dyer (\@PhDyellow) for bug report.
- Add examples using `sf` data to documentation for importance
  evaluation functions (#176).
- Fix broken link in `add_manual_targets()` documentation.
- Fix typo in equation for rarity weighted richness documentation.


# prioritizr 6.0.0.0

## New features

- New `eval_cost()` function to calculate the cost of a solution.
- New `eval_boundary()` function to calculate the exposed boundary length
  associated with a solution.
- New `eval_connectivity()` function to calculate the connectivity associated
  with a solution.
- New `eval_feature_representation()` function to assess how well each
  feature is represented by a solution. This function is similar to the
  deprecated `eval_feature_representation()` function, except that it
  follows conventions for other evaluation functions (e.g. `eval_cost`).
- New `eval_target_representation()` function to assess how well each
  target is met by a solution. This function is similar to the
  `eval_feature_representation()`, except that it corresponds to the targets
  in a conservation planning problem.

## Major changes

- Rename `ferrier_score` function as `eval_ferrier_importance()` function for
  consistency.
- Rename `replacement_cost` function as `eval_replacement_importance()` function
  for consistency.
- Rename `rarity_weighted_richness` function as
  `eval_rare_richness_importance()` function for consistency.
- Deprecated `feature_representation()` function. It is now superseded by the
  `eval_feature_representation()` function.

## Minor improvements and bug fixes

- Fix comparability issues with _Matrix_ package (version 1.3-0) (#172).

## Documentation updates

- Add NEWS to build process (#173).
- Update publication vignette.

# prioritizr 5.0.3.2

## Documentation updates

- Add Schuster et al. (2020) to documentation to provide information on
  solver benchmarks (#170). Thanks to Stefan Blumentrath (\@ninsbl) for
  suggestion.

# prioritizr 5.0.3.1

## Minor improvements and bug fixes

- Fix `add_locked_out_constraints()` function to enable a single planning unit
  from being locked out of multiple zones (when data are specified in raster
  format).

# prioritizr 5.0.3

## Notice

- Release candidate for CRAN.

# prioritizr 5.0.2.7

## Documentation updates

- Update publication record vignette.
- Fix URLs for CRAN checks.

# prioritizr 5.0.2.6

## Minor improvements and bug fixes

- Implement GitHub Actions continuous integration (i.e. update tests and
  README).
- Update `problem()` function to reduce memory consumption for sparse
  matrix arguments (#164).
- Fix compatibility issues between the _testthat_ package and the _gurobi_
  package in package tests.

## Documentation updates

- Update Tasmania vignette to remove superfluous warnings (#168). Thanks to
  Jason Flower (\@jflowernet) for bug report.
- Update publication record vignette.

# prioritizr 5.0.2.5

## New features

- New `add_cplex_solver()` function to generate solutions using
  [IBM CPLEX](https://www.ibm.com/products/ilog-cplex-optimization-studio)
  (via the _cplexAPI_ package).

# prioritizr 5.0.2.4

## Minor improvements and bug fixes

- Fix target calculations in `add_loglinear_targets()` and
  `loglinear_interpolation()` functions. Previously they used a natural
  logarithm for log-linear interpolation. To follow target setting approaches
  outlined by Rodrigues et al. (2004), they now use the decadic logarithm (i.e.
  `log10()`).

## Documentation updates

- Update publication record vignette.
- Update `add_gap_portfolio()` documentation to note that it only works for
  problems with binary decisions (#159). Thanks to \@kkemink for report.

# prioritizr 5.0.2.3

## Documentation updates

- Update publication record vignette.

# prioritizr 5.0.2.2

## Documentation updates

- Update publication record vignette.

# prioritizr 5.0.2.1

## Documentation updates

- Update documentation for `ferrier_score()` function. It no longer incorrectly
  states that these scores can be calculated using CLUZ and now states
  that this functionality is experimental until the formulation can be double
  checked.

# prioritizr 5.0.2

## Notice

- Release candidate for CRAN.

# prioritizr 5.0.1.7

## Minor improvements and bug fixes

- Fix tests for updated datasets in the _prioritizrdata_ package.

## Documentation updates

- Fix small typos in documentation.
- Update citation for Scriven et al. (2020) in the Publication Record vignette.
- Update Salt Spring Island vignette with Ferrier method for calculating
  irreplaceability scores and adjust for changes in cost data.
- Update examples to run with CRAN checks (i.e. `--run-donttest`).

# prioritizr 5.0.1.6

## Minor improvements and bug fixes

- Fix `feature_representation()` bug incorrectly throwing error with vector
  planning unit data (e.g. `sf`-class data).

# prioritizr 5.0.1.5

## Minor improvements and bug fixes

- Fix typo causing `rij_matrix()` to throw an error for large raster data
  (#151).
- Fix "Non-file package-anchored link(s) in documentation object" warnings in
  R-devel checks.

# prioritizr 5.0.1.4

## New features

- New `add_linear_penalties()` to add penalties that penalize planning units
  according to a linear metric.

## Documentation updates

- Update `connectivity_matrix()` documentation to provide an example of how
  to generate connectivity matrices that account for functional connectivity.
- Add more information to the documentation for the `solve()` function.
- Add links to the documentation for the `solve()` function to the Salt Spring
  Island and Tasmania vignettes.

# prioritizr 5.0.1.3

## Minor improvements and bug fixes

- Update `compile()` to throw warning when compiling problems that include
  feature weights and an objective function that does not use feature weights.

# prioritizr 5.0.1.2

## Documentation updates

- Add Schuster _et al._ (2020) to publication record.
- Update Hanson _et al._ (2020) in publication record.
- Update Flower _et al._ (2020) in publication record.

# prioritizr 5.0.1.1

## Minor improvements and bug fixes

- Update `add_gurobi_solver()` function to provide more options for controlling
  the pre-solve step when solving a problem.

# prioritizr 5.0.1

## Notice

- Release candidate for CRAN.

# prioritizr 5.0.0.1

## New features

- New `ferrier_score()` function to compute irreplaceability scores following
  Ferrier _et al_ (2000).

# prioritizr 5.0.0.0

## New features

- Add full support for _sf_ package (#6).
- New `proximity_matrix()` function to generate matrices indicating which
  planning units are within a certain distance of each other (#6).
- New `add_extra_portfolio()`, `add_top_portfolio()`, `add_gap_portfolio()`
  functions to provide specific options for generating portfolios (#134).

## Major changes

- Rename `connected_matrix()` function to `adjacency_matrix()` function to
  follow the naming conventions of other spatial association functions (#6).
- Deprecate `set_number_of_threads()`, `get_number_of_threads()`, and
  `is.parallel()` functions since they are no longer used with new data
  extraction methods.
- Deprecate `add_pool_portfolio()` function because the new
  `add_extra_portfolio()` and `add_top_portfolio()` functions provide this
  functionality (#134).

## Minor improvements and bug fixes

- Enhance `intersecting_units` and `fast_extract` functions to use the
  _exactextractr_ and _fasterize_ packages to speed up raster data extraction
  (#130).
- Fix compatibility issues with upcoming version of tibble (3.0.0).
- Fix bug in `boundary_matrix()` function when handling `SpatialPolygon`
  planning unit data that contain multiple polygons (e.g. a single planning unit
  contains to two separate islands) (#132).
- Remove _velox_ package dependency since it may be archived on CRAN (#130).
  Thanks to Jeffrey Evans (\@jeffreyevans) for report.
- Built-in datasets are now saved with latest workspace version
  (i.e. version 3).

## Documentation updates

- Add Flower _et al._ (2020), Hanson _et al._ (2020), and
  Visalli _et al._ (2020) to publication record (#131). Thanks to Jason Flower
  (\@jflowernet) for report.

# prioritizr 4.1.5.2

## Minor improvements and bug fixes

- Fix bug in `add_rsymphony_solver()` and `add_lpsymphony_solver()` throwing an
  an infeasible error message for feasible problems containing continuous or
  semi-continuous variables.

# prioritizr 4.1.5.1

## Minor improvements and bug fixes

- Add Lin _et al._ (in press) to publication record.

# prioritizr 4.1.5

## Notice

- Release candidate for CRAN.

# prioritizr 4.1.4.4

## Documentation updates

- Fix warnings in R-devel CRAN checks related to documentation.

# prioritizr 4.1.4.3

## Documentation updates

- Add Williams _et al._ (in press) to publication record.

# prioritizr 4.1.4.2

## Minor improvements and bug fixes

- Make error message for `presolve_check()` function more informative (#124).
  Thanks to Amanda Liczner (\@aliczner) for bug report.

## Documentation updates

- Add Rodewald _et al._ (2019) to publication record.
- Update in press version of Rodewald _et al._ (2019).

# prioritizr 4.1.4.1

## Documentation updates

- Add Scriven _et al._ (in press) to publication record.

# prioritizr 4.1.4

## Notice

- Release candidate for CRAN.

# prioritizr 4.1.3.3

## Minor improvements and bug fixes

- Fix `rij_matrix()` so that amounts are calculated correctly for
  vector-based planning unit data.

## Documentation updates

- Fix documentation for `fast_extract()`.

# prioritizr 4.1.3.2

## Documentation updates

- Add Rodewald _et al._ (in press) to publication record.
- Update reference for Bombi _et al._ (2019) in publication record.

# prioritizr 4.1.3.1

## Documentation updates

- Fix typo in README.

# prioritizr 4.1.3

## Notice

- Release candidate for CRAN.

# prioritizr 4.1.2.8

## Minor improvements and bug fixes

- Update `add_locked_in_constraints()` and `add_locked_out_constraints()`
  functions so that they no longer throw an unnecessary warning when
  when they are added to multi-zone problems using raster data with `NA` values.

## Documentation updates

- Update documentation for `add_locked_in_constraints()` and
  `add_locked_out_constraints()` functions to provide recommended practices
  for raster data.
- Update documentation for constraints missing "See also" and "Value" sections.

# prioritizr 4.1.2.7

## Minor improvements and bug fixes

- Fix issue with `rarity_weighted_richness()` returning incorrect scores when
  the feature data contains one feature that has zeros amounts in all planning
  units (e.g. the `tas_features` object in the _prioritizrdata_ package;
  #120).
- Fix issue with `add_gurobi_solver()` returning solution statuses that are
  slightly larger than one (e.g. 1+1.0e-10) when solving problems with
  proportion-type decisions (#118). Thanks to Martin Jung (\@Martin-Jung) for
  bug report.

# prioritizr 4.1.2.6

## New features

- New `add_manual_bounded_constraints()` function to apply lower and upper
  bounds on planning units statuses in a solution (#118). Thanks to Martin Jung
  (\@Martin-Jung) for suggestion.

## Minor improvements and bug fixes

- Update `replacement_cost()` function to use parallel processing to speed up
  calculations (#119).

# prioritizr 4.1.2.5

## Minor improvements and bug fixes

- Update `add_gurobi_solver()`, `add_lpsymphony_solver()`, and
  `add_rsymphony_solver()` functions so that they will not return solutions with
  values less than zero or greater than one when solving problems with
  proportion-type decisions. This issue is the result of inconsistent precision
  when performing floating point arithmetic (#117). Thanks to Martin Jung
  (\@Martin-Jung) for bug report.
- Update `add_locked_in_constraints()` and `add_locked_out_constraints()`
  functions to provide a more helpful error message the `locked_in`/`locked_out`
  argument refers to a column with data that are not logical (i.e.
  `TRUE`/`FALSE`; #118). Thanks to Martin Jung (\@Martin-Jung) for bug report.

# prioritizr 4.1.2.4

## Minor improvements and bug fixes

- Update `solve()` function to throw a more accurate and helpful error
  message when no solutions are found (e.g. due to problem infeasibility or
  solver time limits).
- Standardize error messages so that none of them end in a full stop.

# prioritizr 4.1.2.3

## Major changes

- Rename `add_max_phylo_objective()` function to
  `add_max_phylo_div_objective()`.

## New features

- New `add_max_phylo_end_objective()` function to maximize the phylogenetic
  endemism of species adequately represented in a prioritization (#113).
  Thanks to \@FerreiraPSM for suggestion.

## Minor improvements and bug fixes

- Update simulated phylogeny dataset (`sim_phylogeny`).

## Documentation updates

- Add `add_max_phylo_end_objective()`, `replacement_cost()`, and
  `rarity_weighted_richness()` functions to the Prioritizr vignette.
- Update examples for `add_max_phylo_div_objective()` function.
- Prettify equations in the documentation for objective functions.

# prioritizr 4.1.2.2

## New features

- New `replacement_cost()` function to calculate irreproducibility scores
  for each planning unit in a solution using the replacement cost method (#26).
- New `rarity_weighted_richness()` function to calculate irreproducibility
  scores for each planning unit in a solution using rarity weighted richness
  scores (#26).

## Documentation updates

- New `irreplaceability` manual entry to document functions for calculating
  irreproducibility scores.
- Updated _Salt Spring Island_ vignette with a section on calculating and
  interpreting irreplaceability scores.

# prioritizr 4.1.2.1

## Minor improvements and bug fixes

- Fix compiler warnings thrown during package installation.
- Skip tests on CRAN's Windows system to reduce CRAN check times.
- Skip plotting data in examples during testing to reduce CRAN check times.
- Throw warning message if both the _prioritizr_ and _oppr_ packages are
  loaded at the same time.

## Documentation updates

- Fix typo.
- Fix broken links to _Gurobi_ academic licenses.

# prioritizr 4.1.2

## Notice

- Release candidate for CRAN (rejected).

# prioritizr 4.1.1.2

## Minor improvements and bug fixes

- Fix example throwing an error during CRAN checks.

# prioritizr 4.1.1.1

## Documentation updates

- Add Bombi _et al._ (in press) to publication record.

# prioritizr 4.1.1.0

## Documentation updates

- Fix broken link in main vignette.

# prioritizr 4.1.1

## Notice

- Release candidate for CRAN.

# prioritizr 4.1.0.1

## New features

- New `add_min_shortfall_objective()` function to find solutions that minimize
  target shortfalls.

## Minor improvements and bug fixes

- Fix `problem()` tests so that they work when no solvers are installed.

## Documentation updates

- Add new `add_min_shortfall_objective()` function to main vignette.

# prioritizr 4.1.0.0

## Minor improvements and bug fixes

- The `feature_representation()` function now requires missing (`NA`) values for
  planning unit statuses in a solution for planning units that have missing
  (`NA`) cost data.

# prioritizr 4.0.4.1

## New features

- New `presolve_check()` function to investigate potential sources of numerical
  instability before trying to solve a problem. The manual entry for this
  function discusses common sources of numerical instability and approaches
  for fixing them.

## Minor improvements and bug fixes

- The `solve()` function will now use the `presolve_check()` function to
  verify that problems do not have obvious sources of numerical instability
  before trying to solve them. If a problem is likely to have numerical
  instability issues then this function will now throw an error (unless
  the `solve(x, force = TRUE)`).
- The `add_rsymphony_solver()` function now uses sparse matrix formats so that
  attempts can be made to solve large problems with SYMPHONY---though it is
  unlikely that _SYMPHONY_ will be able to solve such problems in a feasible
  period of time.
- Fix warnings thrown by the _tibble_ package when calling
  `tibble::as.tibble()` instead of `tibble::as_tibble()`.

## Documentation updates

- Add example for calculating feature representation a solution in tabular
  format output by `solve()` (#110). Thanks to Martin Jung (\@Martin-Jung) for
  suggestion.
- Fix several typos in documentation.
- Thrown warnings are now immediately visible.
- Update references in the publication record vignette.
- Specify English (US) in the DESCRIPTION file.

# prioritizr 4.0.4

## Notice

- Release candidate for CRAN.

# prioritizr 4.0.3.1

## Minor improvements and bug fixes

- Retain debugging symbols to conform with CRAN policies.

# prioritizr 4.0.3

## Notice

- Release candidate for CRAN.

# prioritizr 4.0.2.16

## Documentation updates

- Add new citations.

# prioritizr 4.0.2.15

## Documentation updates

- Fix typos in documentation for `add_boundary_penalties()` and
  `add_connectivity_penalties()` function (#106).

# prioritizr 4.0.2.14

## Minor improvements and bug fixes

- Fix bug where use of `add_rsymphony_solver()` and `add_lpsymphony_solver()`
  sometimes returned infeasible solutions when subjected to a
  time limit (#105). Thanks to \@magalicombes for bug report.

# prioritizr 4.0.2.13

## Minor improvements and bug fixes

- Fix assorted bugs in the render, setter, and getter parameter functions for
  `ConservationProblem-class` objects. These methods were implemented to be
  used in future interactive applications and are not currently used in the
  package. As a consequence, these bugs do not affect the correctness of
  any results.

# prioritizr 4.0.2.12

## Minor improvements and bug fixes

- Fix `bad error message` error being thrown when input rasters are not
  comparable (i.e. same coordinate reference system, extent, resolutions, and
  dimensionality) (#104). Thanks to \@faengl for bug report.

## Documentation updates

- Add Domisch _et al._ (2019) to publication record vignette.

# prioritizr 4.0.2.11

## Minor improvements and bug fixes

- Fix issue `solve()` printing annoying text about `tbl_df` (#75). Thanks to
  Javier Fajardo (\@javierfajnolla) for bug report.

# prioritizr 4.0.2.10

## Documentation updates

- Tweak `add_max_features_objective()` example code.

# prioritizr 4.0.2.9

## Documentation updates

- Update publication record vignette.

# prioritizr 4.0.2.8

## Minor improvements and bug fixes

- Fix bug where the `add_neighbor_constraints()` and
  `add_contiguity_constraints()` functions used more memory than they actually
  needed (#102). This is because the argument validation code converted sparse
  matrix objects (i.e. `dgCMatrix`) to base objects (i.e. `matrix`) class
  temporarily. This bug only meant inefficient utilization of computer
  resources---it did not affect the correctness of any results.

# prioritizr 4.0.2.7

## New feature
- New `add_mandatory_allocation_constraints()` function. This function can be
  used to ensure that every planning unit is allocated to a management zone in
  the solution. It is useful when developing land-use plans where every single
  parcel of land must be assigned to a specific land-use zone.

## Minor improvements and bug fixes

- Fix bug in the `$find(x)` method for `Collection` prototypes that caused
  it to throw an error incorrectly. This method was not used in earlier versions
  of this package.

## Documentation updates

- Add the `add_mandatory_allocation_constraints()` to the Management Zones and
  Prioritizr vignettes.

# prioritizr 4.0.2.6

## Minor improvements and bug fixes

- Fix bug the `feature_representation()` function that caused the "amount_held"
  column to have NA values instead of the correct values. This bug only
  affected problems with multiple zones.

# prioritizr 4.0.2.5

## Minor improvements and bug fixes

- Fix bug in argument validation code for the `category_layer()` function that
  it this function to incorrectly throw an error claiming that the input
  argument to `x` was invalid when it was in fact valid. This bug is
  encountered when different layers the argument to `x` have non-NA values in
  different cells.

# prioritizr 4.0.2.4

## Documentation updates

- Update instructions for activating _Gurobi_ licenses on remote machines (#98).

# prioritizr 4.0.2.3

## Minor improvements and bug fixes

- The `add_contiguity_constraints()` function now uses sparse matrix formats
  internally for single-zone problems. This means that the constraints
  can be applied to single-zoned problem with many more planning units.

# prioritizr 4.0.2.2

## Minor improvements and bug fixes

- The `add_connectivity_penalties()` function now uses sparse matrix formats
  internally for single-zone problems. This means that connectivity penalties
  can be applied to single-zoned problem with many more planning units.

# prioritizr 4.0.2.1

## Minor improvements and bug fixes

- Update warning text when compiling problems that contain (i) objective
  functions that do not use targets and (ii) targets (#93).

## Documentation updates

- Update documentation for the `add_max_utility_objective()` and
  `add_max_cover_objective()` functions to make it clearer that they
  do not use targets (#94).

# prioritizr 4.0.2

## Notice

- Release candidate for CRAN.

# prioritizr 4.0.1.6

## Minor improvements and bug fixes

- Fix bug in `add_locked_in_constraints()` and `add_locked_out_constraints()`
  that incorrectly threw an error when using `logical` locked data
  (i.e. `TRUE`/`FALSE`) because it incorrectly thought that valid inputs were
  invalid.
- Fix bug in `add_locked_in_constraints()`, `add_locked_out_constraints()`,
  and `add_manual_locked_constraints()` where solving the same problem object
  twice resulted in incorrect planning units being locked in or out of the
  solution (#92). Thanks to Javier Fajardo (\@javierfajnolla) for bug report.
- Added unit tests for objectives, constraints, decisions, targets, and
  penalties to ensure that solving problems twice does not result in
  different solutions.

# prioritizr 4.0.1.5

## Minor improvements and bug fixes

- Fix bug in `feature_abundances()` that caused the solve function to throw an
  error when attempting to solve problems with a single feature.
- Fix bug in `add_cuts_portfolio()` that caused the portfolio to return
  solutions that were not within the specified optimality gap when using the
  _Gurobi_ solver.
- Add the ability to specify the search pool method and number of solutions to
  the `add_pool_portfolio()` function.

# prioritizr 4.0.1.4

## Minor improvements and bug fixes

- The `feature_representation()` function now allows `numeric` solutions with
  attributes (e.g. when output by the `solve()` function) when calculating
  representation statistics for problems with `numeric` planning unit data
  (#91). Thanks to Javier Fajardo (\@javierfajnolla) for bug report.
- The `add_manual_targets()` function threw a warning when some features had
  targets equal to zero. This resulted in an excessive amount of warnings. Now,
  warnings are thrown for targets that are less then zero.
- The `problem()` function sometimes incorrectly threw a warning that feature
  data had negative values when the data actually did not contain negative
  values. This has now been addressed.

# prioritizr 4.0.1.3

## Minor improvements and bug fixes

- The `problem` function now allows negative values in the cost and feature
  data (and throws a warning if such data are detected).
- The `add_absolute_targets()` and `add_manual_targets()` functions now allow
  negative targets (but throw a warning if such targets are specified).
- The `compile` function throws an error if a problem is compiled using
  the expanded formulation with negative feature data.
- The `add_absolute_targets()` function now throws an warning---instead of an
  error---if the specified targets are greater than the feature abundances
  in planning units to accommodate negative values in feature data.

# prioritizr 4.0.1.2

## Documentation updates

- Fix `add_max_cover_objective()` in _prioritizr_ vignette (#90).

# prioritizr 4.0.1.1

## Minor improvements and bug fixes

- The `add_loglinear_targets()` function now includes a `feature_abundances()`
  parameter for specifying the total amount of each feature to use when
  calculating the targets (#89). Thanks to Liz Law (\@lizlaw) for suggestion.

## Documentation updates

- The `add_relative_targets()` documentation now makes it clear that locked out
  planning units are included in the calculations for setting targets (#89).

# prioritizr 4.0.1

## Notice

- Release candidate for CRAN.

# prioritizr 4.0.0.12

## New feature

- New `feature_abundances()` function to calculate the total amount of each
  feature in the planning units (#86). Thanks to Javier Fajardo
  (\@javierfajnolla) for suggestion.

# prioritizr 4.0.0.11

## Documentation updates

- Fix some equations in the documentation (#83).

# prioritizr 4.0.0.10

## Minor improvements and bug fixes

- Add version requirements for _assertthat_ and _tibble_ packages (#82).

# prioritizr 4.0.0.9

## Documentation updates

- Fix minor typos in the _Gurobi installation guide_.
- Update the _Management zones tutorial_.

# prioritizr 4.0.0.8

## Documentation updates

- Add instructions for setting up the _Gurobi_ Academic license on a computer
  that it is not connected to a university computer network using a computer
  that is on an academic network (#81). For example, these instructions could be
  used to set up _Gurobi_ on a cloud-based system using a laptop computer that
  is connected a university's wireless network.

# prioritizr 4.0.0.7

## Minor improvements and bug fixes

- The `add_cuts_portfolio()` function uses the _Gurobi_ solution pool to
  generate unique solutions within a specified gap of optimality when tasked
  with solving problems with _Gurobi_ (version 8.0.0+) (#80).

# prioritizr 4.0.0.6

## New features

- New `add_pool_portfolio()` function to generate a portfolio of solutions using
  the _Gurobi_ solution pool (#77).

# prioritizr 4.0.0.5

## Minor improvements and bug fixes

- The `boundary_matrix()` function now has the experimental functionality to
  use GEOS STR trees to speed up processing (#74).
- Solutions obtained from _Gurobi_ that contain binary-type decisions are
  explicitly rounded to the nearest integer. This is because _Gurobi_ can output
  solutions to binary problems that contain values which not exactly zero or
  one (e.g. 0.9999997 using default settings) (#78).

# prioritizr 4.0.0.4

## New features

- New `feature_representation()` function to how well features are represented
  in solutions (#73).

# prioritizr 4.0.0.3

## Minor improvements and bug fixes

- The _prioritizrdata_ package has been listed under Suggests.

## Documentation updates

- The vignettes in the _prioritizrdata_ package have been moved to this package
  to make them easier to find.

# prioritizr 4.0.0.2

## Minor improvements and bug fixes

- Fix issue with the `solve()` function printing superfluous text (#75).

# prioritizr 4.0.0.1

## Documentation updates

- Minor improvements to the documentation for the `problem()` function.

# prioritizr 4.0.0.0

## New features

- Added functionality to build and solve problems with multiple management
  zones (#14).
- New built-in datasets `sim_pu_zones_stack`, `sim_pu_zones_polygons`,
  and `sim_features_zones` for exploring conservation problems with
  multiple management zones.
- New `zones` function and `Zones` class to organize data with multiple
  zones.
- New `add_manual_targets()` function for creating targets that pertain to
  multiple management zones.
- New `add_manual_locked_constraints()` function to manually specify which
  planning units should or shouldn't be allocated to specific zones in
  solutions.
- New `binary_stack()`, `category_layer()`, and `category_vector()` functions
  have been provided to help work with data for multiple management zones.

## Major updates

- The `problem()` function now accepts `Zone` objects as arguments for
  `feature` to create problems with multiple zones.
- The `add_relative_targets()` and `add_absolute_targets()` functions for adding
  targets to problems can be used to specify targets for each feature in
  each zone.
- The `solve()` function now returns a `list` of solutions when generating
  a portfolio of solutions.
- All functions for adding constraints and penalties now have
  parameters that specify how they should treat planning units allocate to
  different zones (using the `zones` parameter) and specify how they
  they should be applied (using the `data` parameter. All of these functions
  have default arguments that mean that problems with a single zone
  should have the same optimal solution as problems created in the earlier
  version of the package.

## Minor improvements and bug fixes

- The `add_locked_in_constraints()` and `add_locked_out_constraints()`
  functions for specifying which planning units are locked in or out
  now accept `matrix` arguments for specifying which zones are locked
  in or out.
- The `add_feature_weights()` function can be used to weight different
  the representation of each feature in each zone.

## Documentation updates

- New _Management zones_ vignette on building and solving problems with
  multiple management zones.
- Added mention of zones functionality to package DESCRIPTION, summary (i.e.,
  `?prioritizr`), and README.
- The _Quick Start Guide_ and _Prioritizr Basics_ vignettes have been
  consolidated into the _prioritizr_ vignette.
- The `marxan_problem()` has been updated with more comprehensive documentation
  and to provide more helpful error messages. For clarity, it will now only
  work with tabular data in the standard _Marxan_ format.

# prioritizr 3.0.3.6

## Documentation updates

- Fix typo in README.
- Update documentation for `add_boundary_penalties()` (#62). Thanks to Liz Law
  (\@lizlaw) for report.

# prioritizr 3.0.3.5

## Minor improvements and bug fixes

- Fix bug where `add_locked_in_constraints()` and `add_locked_out_constraints()`
  throw an exception when used with semi-continuous-type decisions (#59).
- Error message in `compile()` thrown when the same planning unit is locked in
  and locked out now prints the planning unit indices in a readable format.

# prioritizr 3.0.3.4

## Minor improvements and bug fixes

- Fix bug where `add_locked_in_constraints()` and `add_locked_out_constraints()`
  are ignored when using proportion-type decisions (#58).

# prioritizr 3.0.3.3

## Minor improvements and bug fixes

- Fix bug in `predefined_optimization_problem()` which incorrectly recognized
  some inputs as invalid when they were in fact valid.
- Addressed NOTE in `R CMD check` related to _proto_ package in Depends.

# prioritizr 3.0.3.2

## Minor improvements and bug fixes

- Moved _proto_ package from Imports to Depends in DESCRIPTION

# prioritizr 3.0.3.1

## Minor improvements and bug fixes

- Depends on R version 3.4.0 (avoids 'patchlevel 0' NOTE/WARNING in checks)

# prioritizr 3.0.3

## Notice

- Release candidate for CRAN.

# prioritizr 3.0.2.3

## Minor improvements and bug fixes

- Unit tests that fail when using _lpsymphony_ due to a bug in _lpsymphony_
  are now skipped (partially addressing #40).

# prioritizr 3.0.2.2

## Minor improvements and bug fixes

- Update `add_lpsymphony_solver()` to throw warnings to alert users to
  potentially incorrect solutions (partially addressing #40).

# prioritizr 3.0.2.1

## Documentation updates

- Vignette sizes have been reduced.

# prioritizr 3.0.2

## Notice

- Release candidate for CRAN. Release postponed due issues on Travis CI.

# prioritizr 3.0.1.1

## Minor improvements and bug fixes

- Unit tests for `add_*_objectives` now pass when executed with slow solvers
  (partially addressing #40).
- Update `compile()` to work when no solvers are installed (#41).
- Gap arguments in `add_*_solvers` are now unbounded and can accept values
  larger than 1 (#44).

# prioritizr 3.0.1

## Notice

- Release candidate for CRAN.

# prioritizr 3.0.0.0

## Major changes
- The `add_max_cover_objective()` function has been renamed to the
  `add_max_utility_objective()`, because the formulation does not follow the
  historical formulation of the maximum coverage reserve selection problem
  (#38).
- The `add_max_cover_objective()` function now follows the historical maximum
  coverage objective. This fundamentally changes `add_max_cover_objective()`
  function and breaks compatibility with previous versions (#38).

## Minor improvements and bug fixes

- Update `add_lpsymphony_solver()` examples and tests to skip on Linux
  operating systems.
- Add tests to unit tests that were being skipped in new version of _testthat_
  package.

# prioritizr 2.0.4.1

## Minor improvements and bug fixes

- Fix bug with `add_lpsymphony_solver()` causing error when attempting to solve
  problems.

# prioritizr 2.0.4

## Notice

- Release candidate for CRAN. Release postponed due to bug report.

# prioritizr 2.0.3.1

## Minor improvements and bug fixes

- Fix bug when solving problems with `numeric` vector data that caused an error.
- Fix bug in compiling problems with `numeric` vector input with rij data
  containing NA values.
- Added unit tests for solving problems with various input formats.
- Updated package sizes reported in `cran-comments.md` file.

# prioritizr 2.0.3

## Notice

- Initial release candidate for CRAN. Release postponed due to bug report.

# prioritizr 2.0.2.9

## Documentation updates

- Added vignette to record publications that use _prioritizr_ (#35).

# prioritizr 2.0.2.8

## Minor improvements and bug fixes

- Unit tests now compatible with development version of _testthat_ (#34).

# prioritizr 2.0.2.7

## Minor improvements and bug fixes

- Fix bug in `apply_boundary_penalties()` and `add_connectivity_penalties()`
  causing the function to throw an error when the number of boundaries/edges is
  less than the number of planning units.

# prioritizr 2.0.2.6

## Minor improvements and bug fixes

- Makevars now compatible with Mac OSX Sierra (#33).

# prioritizr 2.0.2.5

## Minor improvements and bug fixes

- Fix bug in `boundary_matrix()` calculations (#30).

# prioritizr 2.0.2.4

## Documentation updates

- Minor tweaks to vignettes.

# prioritizr 2.0.2.3

## Documentation updates

- Add logo to README files and package website (#31).

# prioritizr 2.0.2.2

## Minor improvements and bug fixes

- Remove _prioritizrdata_ from package Suggests.
- Add _shiny_ and _xtable_ to Suggests for rendering parameters.
- Added code for `ScalarParameter` and `ArrayParameter` prototypes to check
  that functions for generating widgets have their dependencies installed.
- Fix bug when `numeric` planning unit data and portfolios that caused the
  `solve()` to throw an error.
- Remove R-devel from AppVeyor testing because it fails for unknown
  reasons.

## Documentation updates

- Broad-scale improvements to documentation.
- Fix documentation for `add_max_phylo_objective()` (#24).
- Update Gurobi Installation vignette.
- URLs for _lpsymphony_ on Bioconductor now use the package's DOI.
- Add more comprehensive tests to portfolios.

# prioritizr 2.0.2.1

## Major changes

- Removed shiny functions for now to prepare for CRAN release.

## Documentation updates

- Rebuilt website and documentation.

# prioritizr 2.0.2.0

## Documentation updates

- Included vignette on Gurobi solver installation and testing.

# prioritizr 2.0.1.0

## Major changes

- Fixed bug where `Spatial*DataFrame` input to `marxan_problem()` would always
  use the first column in the attribute table for the cost data. **This bug is
  serious** so analysis that used `Spatial*DataFrame` inputs in
  `marxan_problem()` should be rerun.

## Minor improvements and bug fixes

- Added functionality to use feature abundance/occurrence data stored as
  columns in the planning unit data when constructing `problem()` objects.

# prioritizr 2.0.0.2

## Minor improvements and bug fixes

- Skip `add_cuts_portfolio()` on Travis.

# prioritizr 2.0.0.1

## Minor improvements and bug fixes

- Skip `add_cuts_portfolio()` and `add_shuffle_portfolio()` tests on CRAN.

# prioritizr 2.0.0.0

## Major changes

- This version breaks compatibility with previous releases because
  solutions in `data.frame` and `Spatial*DataFrame` objects
  are now stored in columns named "solution_*" (e.g. "solution_1")
  to store multiple solutions.
- Solutions now contain additional information in stored in the object's
  attributes (#24). See `README.Rmd` for examples on accessing this
  information.

## New features

- Added support for multiple solutions (#23).

## Minor improvements and bug fixes

- Add logical `verbose` argument to all solvers. This replaces the `verbosity`
- The verbosity of information presented when solving problems using
  `add_lpsymphony_solver()` and `add_rsymphony_solver()` is reduced.

## Documentation updates

- Tidy examples in `add_gurobi_solver.R`, `add_lpsymphony_solver.R`,
  `add_rsymphony_solver.R`, and `solvers.R`.
  argument in `add_lpsymphony_solver()` and `add_rsymphony_solver()`.
- Assorted spelling mistakes have been fixed.

# prioritizr 1.0.2.3

## Minor improvements and bug fixes

- `ConservationProblem$print()` now only prints the first three species names
  and a count of the total number of features. This update means that
  `ConservationProblem` objects with lots of features can now safely be printed
  without polluting the R console.
- Fix bug where _lpsymphony_ and _Rsymphony_ solvers would return solutions
  containing NA values if they did not find a feasible solution within
  the argument to `time_limit`.

## Documentation updates

- Attempt to make equations in help files prettier.

# prioritizr 1.0.2.2

## Minor improvements and bug fixes

- Update `marxan_problem()` to work with absolute file paths and the `INPUTDIR`
  in Marxan input files (#19). Thanks to Dan Rosauer (\@DanRosauer) for bug
  report.

# prioritizr 1.0.2.1

## Minor improvements and bug fixes

- Fix bug in `solve()` when the rij data does not contain the highest planning
  unit identifier specified when building the `problem()` (#20).

# prioritizr 1.0.2.0

## Minor improvements and bug fixes

- Passes CRAN checks on Winbuilder.
- Added _roxygen2_ to Suggests for Travis CI.

# prioritizr 1.0.1.6

## Minor improvements and bug fixes

- Simplify vignette workflow. Vignettes can now be compiled by using
  `devtools::build_vignettes()`. Earlier versions needed the vignettes to be
  compiled using the _Makefile_ to copy files around to avoid tangled R code
  causing failures during R CMD CHECK. Although no longer needed, the vignettes
  can still be compiled using the shell command `make vigns` if
  desired.
- Make the _data.table_ package automatically installed when _prioritizr_ is
  installed (#18).
- Move _shiny_, _shinydashboard_, and _leaflet_ packages to Imports to avoid
  polluting users environment.
- Update preliminary versions of the shiny apps to call functions from other
  packages explicitly.
- Lint objective function definition files.
- Added _rmarkdown_ package to Suggests following recommended practices.

## Documentation updates

- The `README.Rmd` now lives in the top-level directory following standard
  practices. It should now be complied using `rmarkdown::render("README.Rmd")`
  or using the shell command `make readme`. Note that the figures for
  `README.md` can be found in the directory `man/figures`.
- The example for `prshiny` will now only be run if executed during an
  interactive R session. Prior to this R CMD CHECK would hang.
- UTF-8 math characters in vignettes have been replaced with with MathJax
  compatible latex expressions.
- R code in the vignettes has been linted to follow the package's style guide.
- Fix example in vignette `quick_start.Rmd` showing how to run
  `marxan_problem()` using input `data.frame()` objects.
- Fix bug in vignette `quick_start.Rmd` counting number of selected planning
  units
- `README.Rmd` tweaks to make it look prettier on website.
- Remove "\text" latex sequences from objective function definition files
  because CRAN doesn't support _amsmath_ extensions in equations.
- Update examples in objective function files to only show relevant objectives

# prioritizr 1.0.1.5

## Minor improvements and bug fixes

- Enable 64 bit Armadillo flag. This increases the maximum size of problems
  that can be solved.
- Disable bound-checks in Armadillo matrix operations. This should reduce
  processing time when running the `compile()` function.

# prioritizr 1.0.1.4

## Minor improvements and bug fixes

- Fix bug in `problem.data.frame` that meant that it did not check for missing
  values in `rij$pu`.

# prioritizr 1.0.1.3

## Minor improvements and bug fixes

- Fix bugs `add_absolute_targets()` and add_relative_targets` related to their
  standardGeneric being incorrectly defined
- Reduce installation size using Dirk Eddelbuettel's awesome advice:
  http://dirk.eddelbuettel.com/blog/2017/08/14#009_compact_shared_libraries
- Fix bug in `add_corridor_targets()` when argument  `connectivities` is a
  `list`. The elements in the list are assumed to be `dsCMatrix` objects
  (aka symmetric sparse matrices in a compressed format) and are coerced
  to `dgCMatrix` objects to reduce computational burden. There was a typo,
  however, and so the objects were coerced to `dgCmatrix` and not `dgCMatrix`.
  This evidently was ok in earlier versions of the _RcppArmadillo_ and/or
  _Matrix_ packages but not in the most recent versions.

# prioritizr 1.0.1.2

## Minor improvements and bug fixes

- Fix bug in `problem()` causing node stack overflows (#21). Thanks to Dan
  Rosauer (\DanRosauer) for bug report.

# prioritizr 1.0.1.1

## Minor improvements and bug fixes

- Add _roxygen2_ to package SUGGESTS for building vignettes.

# prioritizr 1.0.1.0

## Minor improvements and bug fixes

- Fix issue where `parallel::detectCores()` returns `NA` on some systems
  preventing users from using the Gurobi solver--even when one thread is
  specified.

# prioritizr 1.0.0.5

## Minor improvements and bug fixes

- Fix building issue due to incorrect file order in DESCRIPTION.

# prioritizr 1.0.0.4

## Minor improvements and bug fixes

- Compatibility with R 3.4.0.
- Replace `structure(NULL, ...)` with `structure(list(), ...)`.
- Register compiled library files.
- Remove duplicate definition of `new_waiver()`.
- Update tests to skip if _prioritizrdata_ package not installed.

# prioritizr 1.0.0.3

## Major changes

- Make `add_default_objectives()` and `add_default_targets()` private functions.

## Documentation updates

- Fix missing links in documentation.
- Fix typos in _roxygen2_ parameters.
- Move `add_default_decisions()` and `add_default_solver()` to own help file.

# prioritizr 1.0.0.2

## Minor improvements and bug fixes

- Fix bug in `rij_matrix()` duplicating feature data (#13).

# prioritizr 1.0.0.1

## Minor improvements and bug fixes

- Fix _velox_ package dependency (#8).
- Fix bug in `add_corridor_constraints()` that fails to actually add the
  constraints with argument to `connectivity` is a list.
- Fix bug in `make install` command so that it now actually installs the
  package.

## Documentation updates

- Fix link to Joe's website in the package's website.

# prioritizr 1.0.0.0

## Notice

- R interface fully functional.

# prioritizr 0.1.2.9

## Major changes

- Package re-implementation.

# prioritizr 0.1.2

## Notice

- Prepare for CRAN submission.

## New features

- Introduce maximum target coverage model.

## Minor improvements and bug fixes

- Add continuous integration.
- Fixed various bugs.

## Documentation updates

- Add full vignette in addition to quickstart guide.

# prioritizr 0.1.1

## Major changes

- Initial package version.
