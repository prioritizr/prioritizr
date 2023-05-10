# prioritizr 8.0.2.1

- Update `problem()` so that an error will be thrown if argument to `features`
  contains only missing (`NA`) values (e.g., an _sf_ object is supplied that
  has `NA` values in all rows for a feature's column).
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

## Breaking changes

- All _proto_ classes have been migrated to _R6_ classes. This update reduces
  memory usage (#238), so `problem()` objects can now contain many more
  constraints and penalties. Note that any `problem()` objects
  that were produced using earlier versions of the package are no longer
  compatible.
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
  if an old version of the _lpsymphony R_ package is installed that is known
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

## New features

- The `print()` function for `problem()`, `optimization_problem()`, and
  `zones()` objects has been updated to provide more information.
- New `summary()` function to provide extensive detail on `problem()` objects.
- Updates to improve the error messages and error message handling.
  Hopefully, users should no longer see `"bad error message"`!

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
- Update publication record.
- The `solve()` function will now output `tibble::tibble()` objects
  (instead of `data.frame()` objects), when the planning unit data are
  `tibble::tibble()` objects.
- The `boundary_matrix()` function now uses `terra::sharedPaths()` for
  calculations, providing greater performance (#257).
- The `eval_ferrier_importance()` function can now be used with
  any objective function that uses targets and a single zone.
- Fix CRAN note regarding C++ standards (#263).
- Remove _doParallel_ and _plyr_ packages as dependencies by simplifying
  the `add_shuffle_portfolio()` and `eval_replacement_importance()` functions.
- Assorted tweaks to improve writing in the vignettes and documentation.
  Many thanks to Marc Edwards (@edwardsmarc)!

# prioritizr 8.0.1

- Assorted bug fixes.

# prioritizr 8.0.0

- This version contains an incorrect version of the code, due to a mistake
  when preparing for CRAN release. We strongly recommend upgrading to
  version 8.0.1 to ensure correct results. We apologize any inconvenience this
  might have caused.

# prioritizr 7.2.2.7

- Update README badges.
- Update publication record.

# prioritizr 7.2.2.6

- Update publication record.

# prioritizr 7.2.2.5

- Update publication record.

# prioritizr 7.2.2.4

- Specify minimum version for Matrix package dependency (#255).

# prioritizr 7.2.2.3

- New `add_highs_solver()` function for the HiGHS optimization software (#250).
- Update `add_default_solver()` to use the HiGHS solver if the Gurobi, IBM
  CPLEX, and CBC solvers aren't available.
- Fix `add_default_solver()` so that the `add_lpsymphony_solver()` is used
  instead of `add_rsymphony_solver()`.

# prioritizr 7.2.2.2

- Update `problem()` and `eval_feature_representation_summary()` to avoid
  needlessly converting sparse matrices to regular matrices (#252).
- Fix broken URLs in documentation.

# prioritizr 7.2.2.1

- Fix mistake in NEWS.md.
- Update publication record.

# prioritizr 7.2.2

- Release candidate for CRAN.
- Fix compiler warnings.
- Update tests to skip long-running tests on CRAN.
- Update examples to minimize overall package check timings for CRAN.

# prioritizr 7.2.1

- Release candidate for CRAN (rejected due to package check timings).

# prioritizr 7.2.0.9

- Fix compatibility with upcoming _Matrix_ package version (version 1.5-0).
- Update package documentation to provide details for obtaining and installing
  the _cplexAPI_ package since it has been archived on CRAN (#214).
- Fix bug that caused the `add_cbc_solver()` to throw a segfault when solving
  a problem wherein the `rij_matrix(x)` has a zero amount for the last feature
  in the last planning unit (#247).
- Update `simulate_data()`, `simulate_cost()` and `simulate_species()`
  functions to improve performance using the _fields_ package.
- Update `boundary_matrix()` to use STR query trees by default.
- Remove _maptools_, _PBSmapping_, and _rgeos_ packages as dependencies.
  This involved updating the unit tests to hard-code correct results,
  updating examples to use the _sf_ package, and updating the
  `boundary_matrix()` to use the _geos_ package (#218).
- Fix broken URLs in package documentation.

# prioritizr 7.2.0.8

- Update `simulate_cost()` and `simulate_species()` so that they no longer
  depend on the _RandomFields_ package.

# prioritizr 7.2.0.7

- Update publication record.

# prioritizr 7.2.0.6

- Update publication record.

# prioritizr 7.2.0.5

- Update publication record.

# prioritizr 7.2.0.4

- Fix broken URLs in documentation.

# prioritizr 7.2.0.3

- Update the `presolve_check()` function to (i) reduce chances of
  it incorrectly throwing an error when the input data won't actually
  cause any issues, and (ii) provide recommendations for addressing issues.
- Update documentation for `add_min_largest_shortfall_objective()` so that
  examples complete in a shorter period of time.

# prioritizr 7.2.0.2

- Fix bug in processing planning unit data when a problem is constructed
  using arguments to (i) `x` that are `numeric` or `matrix` format, (ii)
  `x` that contain missing (`NA`) values, and (iii) `rij_matrix` that
  are in `dgCMatrix` format. This bug only occurred when all three of these
  specific conditions were met. When it occurred, the bug caused planning units
  with `NA` cost values to receive very high cost values (e.g., 1e+300).
  This bug  meant that when attempting to solve the problem, the
  presolve checks (per `presolve_check()`) would throw an error complaining
  about very high cost values (#236).

# prioritizr 7.2.0.1

- Update publication record.

# prioritizr 7.2.0.0

- Fix `add_locked_in_constraints()` and `add_locked_out_constraints()`
  to ensure that a meaningful error message is provided when no planing
  units are locked (#234).
- Fix `presolve_check()` so that it does not throw a meaningless warning
  when the mathematical objective function only contains zeros.
- Update `presolve_check()` to help reduce chances of mis-attributing
  high connectivity/boundary values due to planning unit costs.
- Update `add_connectivity_penalties()` function and documentation so that
  it is designed specifically for symmetric connectivity data.
- New `add_asym_connectivity_penalties()` function that is designed
  specifically for asymmetric connectivity data. This function has been
  created to help ensure that asymmetric connectivity data are handled
  correctly. For instance, using asymmetric connectivity data with
  `add_connectivity_penalties()` function in previous versions of the package
  sometimes resulted in the data being incorrectly treated as symmetric data.
  Additionally, this function uses an updated mathematical formulation
  for handling asymmetric connectivity so that it provides similar
  results to the _Marxan_ software (#323).
- Update `marxan_problem()` function so that it can be used with asymmetric
  connectivity data. This is now possible because there are dedicated functions
  for symmetric and asymmetric connectivity.
- Update publication record.
- Update URLs in publication record so that they pass CRAN checks.

# prioritizr 7.1.1.12

- Update publication record.

# prioritizr 7.1.1.11

- Update publication record.

# prioritizr 7.1.1.10

- Fix broken links in documentation.
- Update publication vignette.
- Improve documentation for the `zones` parameter of the
  `add_connectivity_penalties()` function.

# prioritizr 7.1.1.9

- Update documentation for `eval_ferrier_importance()` (#220). Although this
  function is now recommended for general use, the documentation
  contained an outdated warning and so the warning has now been removed.

# prioritizr 7.1.1.8

- Update publication record.
- Fix bug so that the `eval_n_summary()` function now returns a table with
  the column name `"n"` (instead of `"cost"`) for the number
  of selected planning units (#219).
- Update reference index for package website.
- Fix minor typos in vignettes.

# prioritizr 7.1.1.7

- Minimum version numbers are now provided for all R package dependencies
  (excepting base R packages) (#217).
- The _data.table_ package is now listed as a suggested (optional) dependency.
  This is because it is only used by the `marxan_problem()` for importing
  _Marxan_ data files.

# prioritizr 7.1.1.6

- Update publication record.

# prioritizr 7.1.1.5

- The _Tasmania tutorial_ has been reworked into the _Getting started_ tutorial. This tutorial now provides short introduction to using the package.
- The _Salt Spring Island tutorial_ has been reworked into the _Connectivity tutorial_. This tutorial now explores different approaches for incorporating connectivity.
- The _prioritizr_ vignette has been renamed to the _Package overview_ vignette.
- New _Calibrating trade-offs tutorial_ showcasing methods for running calibration analyses. It outlines blended and hierarchical approaches for generating a set of different prioritizations based on different parameters. It also covers different approaches for selecting a candidate prioritization based on different trade-offs.

# prioritizr 7.1.1.4

- Update tests to reduce run time and pass given slightly different results
  with new Gurobi version (9.5.0).
- Update built-in `sim_pu_sf` and `sim_pu_zones_sf` data given class
  updates to the _sf_ package (compatible with version 1.0.3+).
- Update example for `write_problem()` function.

# prioritizr 7.1.1.3

- Update publication record.

# prioritizr 7.1.1.2

- Update publication record.

# prioritizr 7.1.1.1

- Fix URL in vignette.

# prioritizr 7.1.1

- Release candidate for CRAN.

# prioritizr 7.1.0.4

- Update `eval_ferrier_importance()` function with verified code.
- Update documentation with information about Ferrier importance scores.
- Brandon Edwards added to package author list.
- Update Gurobi Installation guide vignette.
- Update benchmark vignette.
- Update `presolve_check()` function to throw warning when
  really high values specified in `add_neighbor_constraints()`.

# prioritizr 7.1.0.3

Update `add_cbc_solver()` function so that it can use a starting solution to reduce run time (via the `start_solution` parameter).

# prioritizr 7.1.0.2

- Fix examples that fail package checks.
- Remove _xtable R_ package from Suggests because it is no longer used.

# prioritizr 7.1.0.1

- Update publication vignette.

# prioritizr 7.1.0.0

- New `add_linear_constraint()` function to add arbitrary constraints.
- Update `add_min_shortfall_objective()` and
  `add_min_largest_shortfall_objective()` functions to handle targets with
  a target threshold value of zero.
- Minor improvements to the documentation. These include moving mathematical
  details to dedicated sections, providing more links in the See also sections,
  fixing text formatting for the `eval_connectivity_summary()` function,
  and tweaking the header in the README.
- Update publication vignette.

# prioritizr 7.0.1.5

- Update publication vignette.

# prioritizr 7.0.1.4

- Update documentation and examples for `problem()` function.
- Update publication vignette.

# prioritizr 7.0.1.3

- New solver benchmark vignette.

# prioritizr 7.0.1.2

- Update `add_gurobi_solver()` function so that it doesn't print excess
  debugging information (accidentally introduced in previous version 7.0.1.1).

# prioritizr 7.0.1.1

- Update `add_gurobi_solver()` function to support the `node_file_start`
  parameter for the Gurobi software. This functionality is useful solving large
  problems on systems with limited memory (#139).

# prioritizr 7.0.1

- Release candidate for CRAN.
- Update DESCRIPTION with more information on the package usage.
- Update DESCRIPTION with details on rcbc R package installation.

# prioritizr 7.0.0.8

- New `write_problem()` function to save the mixed integer programming
  representation of a conservation planning problem to a file. This
  function is useful for manually executing optimization solvers.

# prioritizr 7.0.0.7

- Fix typo in `rij_matrix()` function documentation (#189).
- Update publication vignette.

# prioritizr 7.0.0.6

- Update `add_gurobi_solver()` function to allow specification of a starting
  solution (#187). This functionality is useful for conducting a boundary
  penalty parameter calibration exercise. Specifically, users can specify the
  starting solution for a given penalty value based on the solution
  obtained using a smaller penalty value.
- Bug fix: `solve` now assigns layer names based on zone names for solutions in
  format.

# prioritizr 7.0.0.5

- Update methods for calculating solver runtime.

# prioritizr 7.0.0.4

- Update publication record.
- Bug fix: `time_limit` and `verbose` parameters for `add_cbc_solver()` now work
  as expected.

# prioritizr 7.0.0.3

- Update `add_gurobi_solver()` function to report timings following the same
  methods as the other solvers.

# prioritizr 7.0.0.2

- Update `add_lpsymphony_solver()` function to be more memory efficient (#183).
- Added _slam R_ package to dependencies to enable more memory efficient
  usage of the _lpsymphony R_ package (#183).

# prioritizr 7.0.0.1

- Update unit tests to solve a greater proportion of them using continuous
  integration services (#181).
- Bug fix: `add_cbc_solver()` is now preferred over all other open source
  solvers.
- Bug fix: `add_cbc_solver()` would sometimes return incorrect solutions to
  problems with equality constraints.

# prioritizr 7.0.0.0

- Remove unused _shiny R_ package integration and dependencies (#141).

# prioritizr 6.0.0.2

- New `add_cbc_solver()` function to generate solutions using the open source
  CBC solver via the rcbc R package (https://github.com/dirkschumacher/rcbc).
- Update `add_rsymphony_solver()` and `add_lpsymphony_solver()` functions to
  have a default `time_limit` argument set as the maximum machine integer for
  consistency.
- Update `add_rsymphony_solver()`, `add_lpsymphony_solver()`, and
  `add_gurobi_solver()` functions to require `logical` (`TRUE`/`FALSE`)
  arguments for the `first_feasible` parameter.
- Update documentation for solvers to provide more detailed information.
- Update publication record.
- Update `add_default_solver()` function so that it prefers
  `add_lpsymphony_solver()` over `add_rsymphony_solver()`, and
  `add_cbc_solver()` over all open source solvers.
- Bug fix: previous versions of the package reported that the `gap` parameter
  for the `add_rsymphony_solver()` and `add_lpsymphony_solver()` corresponded
  to the maximum absolute difference from the optimal objective value.
  This was an error due to misunderstanding the *SYMPHONY* documentation.
  Under previous versions of the package, the `gap` parameter actually
  corresponded to a relative optimality gap expressed
  as a percentage (such that`gap = 10` indicates that solutions must be at
  least 10% from optimality). We have now fixed this error and the documentation
  described for the `gap` parameter is correct. We apologize for any
  inconvenience this may have caused.

# prioritizr 6.0.0.1

- Update Solution format section documentation for evaluation functions
  (i.e. all functions starting with `eval_`) to mention that
  the argument to `solution` should only contain columns that correspond to
  the solution (#176).
- Add examples using `sf` data to documentation for importance
  evaluation functions (#176).
- Add more helpful error messages when invalid `solution` arguments are
  supplied to the evaluation functions (#176).
- Add functionality to calculate importance scores using the Ferrier method
  with `sf` planning unit data.
- Fix broken link in `add_manual_targets()` documentation.
- Fix typo in equation for rarity weighted richness documentation.
- New `add_min_largest_shortfall()` objective function.

# prioritizr 6.0.0.0

- New `eval_cost()` function to calculate the cost of a solution.
- New `eval_boundary()` function to calculate the exposed boundary length
  associated with a solution.
- New `eval_connectivity()` function to calculate the connectivity associated
  with a solution.
- Deprecated `feature_representation()` function. It is now superseded by the
  `eval_feature_representation()` function.
- New `eval_feature_representation()` function to assess how well each
  feature is represented by a solution. This function is similar to the
  deprecated `eval_feature_representation()` function, except that it
  follows conventions for other evaluation functions (e.g. `eval_cost`).
- New `eval_target_representation()` function to assess how well each
  target is met by a solution. This function is similar to the
  `eval_feature_representation()`, except that it corresponds to the targets
  in a conservation planning problem.
- Rename `ferrier_score` function as `eval_ferrier_importance()` function for
  consistency.
- Rename `replacement_cost` function as `eval_replacement_importance()` function
  for consistency.
- Rename `rarity_weighted_richness` function as
  `eval_rare_richness_importance()` function for consistency.
- Fix comparability issues with _Matrix_ package (version 1.3-0) (#172).
- Add NEWS to build process (#173).
- Update publication vignette.

# prioritizr 5.0.3.2

- Add Schuster et al. 2020 to documentation to provide information on
  solver benchmarks (#170).

# prioritizr 5.0.3.1

- Fix `add_locked_out_constraints()` function to enable a single planning unit
  from being locked out of multiple zones (when data are specified in raster
  format).

# prioritizr 5.0.3

- Release candidate for CRAN.

# prioritizr 5.0.2.7

- Update publication record vignette.
- Fix URLs for CRAN checks.

# prioritizr 5.0.2.6

- Implement GitHub Actions continuous integration (i.e. update tests and
  README).
- Update `problem()` function to reduce memory consumption for sparse
  matrix arguments (#164).
- Update Tasmania vignette to remove superfluous warnings (#168).
- Update publication record vignette.
- Fix compatibility issues between the testthat R package and the gurobi R
  package in package tests.

# prioritizr 5.0.2.5

- New `add_cplex_solver()` function to generate solutions using
  [IBM CPLEX](https://www.ibm.com/products/ilog-cplex-optimization-studio)
  (via the cplexAPI package).

# prioritizr 5.0.2.4

- Update publication record vignette.
- Update `add_gap_portfolio()` documentation to note that it only works for
  problems with binary decisions (#159).
- Fix target calculations in `add_loglinear_targets()` and
  `loglinear_interpolation()` functions. Previously they used a natural
  logarithm for log-linear interpolation. To follow target setting approaches
  outlined by Rodrigues et al. (2004), they now use the decadic logarithm (i.e.
  `log10()`).

# prioritizr 5.0.2.3

- Update publication record vignette.

# prioritizr 5.0.2.2

- Update publication record vignette.

# prioritizr 5.0.2.1

- Update documentation for `ferrier_score()` function. It no longer incorrectly
  states that these scores can be calculated using CLUZ and now states
  that this functionality is experimental until the formulation can be double
  checked.

# prioritizr 5.0.2

- Release candidate for CRAN.

# prioritizr 5.0.1.7

- Fix tests for updated datasets in the prioritizrdata R package.
- Fix small typos in documentation.
- Update citation for Scriven et al. (2020) in the Publication Record vignette.
- Update Salt Spring Island vignette with Ferrier method for calculating
  irreplaceability scores and adjust for changes in cost data.
- Update examples to run with CRAN checks (i.e. `--run-donttest`).

# prioritizr 5.0.1.6

- Fix `feature_representation()` bug incorrectly throwing error with vector
  planning unit data (e.g. sf-class data).

# prioritizr 5.0.1.5

- Fix typo causing `rij_matrix()` to throw an error for large raster data
  (#151).
- Fix "Non-file package-anchored link(s) in documentation object" warnings in
  R-devel checks.

# prioritizr 5.0.1.4

- New `add_linear_penalties()` to add penalties that penalize planning units
  according to a linear metric.
- Update `connectivity_matrix()` documentation to provide an example of how
  to generate connectivity matrices that account for functional connectivity.
- Add more information to the documentation for the `solve()` function.
- Add links to the documentation for the `solve()` function to the Salt Spring
  Island and Tasmania vignettes.

# prioritizr 5.0.1.3

- Update `compile()` to throw warning when compiling problems that include
  feature weights and an objective function that does not use feature weights.

# prioritizr 5.0.1.2

- Add Schuster _et al._ (2020) to publication record.
- Update Hanson _et al._ (2020) in publication record.
- Update Flower _et al._ (2020) in publication record.

# prioritizr 5.0.1.1

- Update `add_gurobi_solver()` function to provide more options for controlling
  the pre-solve step when solving a problem.

# prioritizr 5.0.1

- Release candidate for CRAN.

# prioritizr 5.0.0.1

- New `ferrier_score()` function to compute irreplaceability scores following
  Ferrier _et al_ (2000).

# prioritizr 5.0.0.0

- Add full support for _sf_ package (#6).
- Add Flower _et al._ (2020), Hanson _et al._ (2020), and
  Visalli _et al._ (2020) to publication record (#131).
- New `proximity_matrix()` function to generate matrices indicating which
  planning units are within a certain distance of each other (#6).
- Rename `connected_matrix()` function to `adjacency_matrix()` function to
  follow the naming conventions of other spatial association functions (#6).
- New `add_extra_portfolio()`, `add_top_portfolio()`, `add_gap_portfolio()`
  functions to provide specific options for generating portfolios (#134).
- Enhance `intersecting_units` and `fast_extract` functions to use the
  _exactextractr_ and _fasterize_ packages to speed up raster data extraction
  (#130).
- Fix compatibility issues with upcoming version of tibble (3.0.0).
- Fix bug in `boundary_matrix()` function when handling `SpatialPolygon`
  planning unit data that contain multiple polygons (e.g. a single planning unit
  contains to two separate islands) (#132).
- Remove _velox_ package dependency since it may be archived on CRAN (#130).
- Deprecate `set_number_of_threads()`, `get_number_of_threads()`, and
  `is.parallel()` functions since they are no longer used with new data
  extraction methods.
- Deprecate `add_pool_portfolio()` function because the new
  `add_extra_portfolio()` and `add_top_portfolio()` functions provide this
  functionality (#134).
- Built-in datasets are now saved with latest workspace version
  (i.e. version 3).

# prioritizr 4.1.5.2

- Fix bug in `add_rsymphony_solve()r` and `add_lpsymphony_solver()` throwing an
  an infeasible error message for feasible problems containing continuous or
  semi-continuous variables.

# prioritizr 4.1.5.1

- Add Lin _et al._ (in press) to publication record.

# prioritizr 4.1.5

- Release candidate for CRAN.

# prioritizr 4.1.4.4

- Fix warnings in R-devel CRAN checks related to documentation.

# prioritizr 4.1.4.3

- Add Williams _et al._ (in press) to publication record.

# prioritizr 4.1.4.2

- Add Rodewald _et al._ (2019) to publication record.
- Update in press version of Rodewald _et al._ (2019).
- Make error message for `presolve_check()` function more informative (#124).

# prioritizr 4.1.4.1

- Add Scriven _et al._ (in press) to publication record.

# prioritizr 4.1.4

- Release candidate for CRAN.

# prioritizr 4.1.3.3

- Fix `rij_matrix()` so that amounts are calculated correctly for
  vector-based planning unit data.
- Fix documentation for `fast_extract()`.

# prioritizr 4.1.3.2

- Add Rodewald _et al._ (in press) to publication record.
- Update reference for Bombi _et al._ (2019) in publication record.

# prioritizr 4.1.3.1

- Fix typo in README.

# prioritizr 4.1.3

- Release candidate for CRAN.

# prioritizr 4.1.2.8

- Update `add_locked_in_constraints()` and `add_locked_out_constraints()`
  functions so that they no longer throw an unnecessary warning when
  when they are added to multi-zone problems using raster data with `NA` values.
- Update documentation for `add_locked_in_constraints()` and
  `add_locked_out_constraints()` functions to provide recommended practices
  for raster data.
- Update documentation for constraints missing "See also" and "Value" sections.

# prioritizr 4.1.2.7

- Fix issue with `rarity_weighted_richness()` returning incorrect scores when
  the feature data contains one feature that has zeros amounts in all planning
  units (e.g. the `tas_features` object in the _prioritizrdata_ R package;
  #120).
- Fix issue with `add_gurobi_solver()` returning solution statuses that are
  slightly larger than one (e.g. 1+1.0e-10) when solving problems with
  proportion-type decisions (#118).

# prioritizr 4.1.2.6

- Update `replacement_cost()` function to use parallel processing to speed up
  calculations (#119).
- New `add_manual_bounded_constraints()` function to apply lower and upper
  bounds on planning units statuses in a solution (#118).

# prioritizr 4.1.2.5

- Update `add_gurobi_solver()`, `add_lpsymphony_solver()`, and
  `add_rsymphony_solver()` functions so that they will not return solutions with
  values less than zero or greater than one when solving problems with
  proportion-type decisions. This issue is the result of inconsistent precision
  when performing floating point arithmetic (#117).
- Update `add_locked_in_constraints()` and `add_locked_out_constraints()`
  functions to provide a more helpful error message the `locked_in`/`locked_out`
  argument refers to a column with data that are not logical (i.e.
  `TRUE`/`FALSE`; #118).

# prioritizr 4.1.2.4

- Update `solve()` function to throw a more accurate and helpful error
  message when no solutions are found (e.g. due to problem infeasibility or
  solver time limits).
- Standardize error messages so that none of them end in a full stop.

# prioritizr 4.1.2.3

- Rename `add_max_phylo_objective()` function to
  `add_max_phylo_div_objective()`.
- New `add_max_phylo_end_objective()` function to maximize the phylogenetic
  endemism of species adequately represented in a prioritization (#113).
- Add `add_max_phylo_end_objective()`, `replacement_cost()`, and
  `rarity_weighted_richness()` functions to the Prioritizr vignette.
- Update simulated phylogeny dataset (`sim_phylogeny`).
- Update examples for `add_max_phylo_div_objective()` function.
- Prettify equations in the documentation for objective functions.

# prioritizr 4.1.2.2

- New `irreplaceability` manual entry to document functions for calculating
  irreproducibility scores.
- New `replacement_cost()` function to calculate irreproducibility scores
  for each planning unit in a solution using the replacement cost method (#26).
- New `rarity_weighted_richness()` function to calculate irreproducibility
  scores for each planning unit in a solution using rarity weighted richness
  scores (#26).
- Updated _Salt Spring Island_ vignette with a section on calculating and
  interpreting irreplaceability scores.

# prioritizr 4.1.2.1

- Fix typo.
- Fix broken links to _Gurobi_ academic licenses.
- Fix compiler warnings thrown during package installation.
- Skip tests on CRAN's Windows system to reduce CRAN check times.
- Skip plotting data in examples during testing to reduce CRAN check times.
- Throw warning message if both the _prioritizr_ and _oppr_packages are
  loaded at the same time.

# prioritizr 4.1.2

- Release candidate for CRAN (rejected).

# prioritizr 4.1.1.2

- Fix example throwing an error during CRAN checks.

# prioritizr 4.1.1.1

- Add Bombi _et al._ (in press) to publication record.

# prioritizr 4.1.1.0

- Fix broken link in main vignette.

# prioritizr 4.1.1

- Release candidate for CRAN.

# prioritizr 4.1.0.1

- New `add_min_shortfall_objective()` function to find solutions that minimize
  target shortfalls.
- Add new `add_min_shortfall_objective()` function to main vignette.
- Fix `problem()` tests so that they work when no solvers are installed.

# prioritizr 4.1.0.0

- The `feature_representation()` function now requires missing (`NA`) values for
  planning unit statuses in a solution for planning units that have missing
  (`NA`) cost data.

# prioritizr 4.0.4.1

- New `presolve_check()` function to investigate potential sources of numerical
  instability before trying to solve a problem. The manual entry for this
  function discusses common sources of numerical instability and approaches
  for fixing them.
- The `solve()` function will now use the `presolve_check()` function to
  verify that problems do not have obvious sources of numerical instability
  before trying to solve them. If a problem is likely to have numerical
  instability issues then this function will now throw an error (unless
  the `solve(x, force = TRUE)`).
- The `add_rsymphony_solver()` function now uses sparse matrix formats so that
  attempts can be made to solve large problems with SYMPHONY---though it is
  unlikely that _SYMPHONY_ will be able to solve such problems in a feasible
  period of time.
- Fix warnings thrown by the _tibble R_ package when calling
  `tibble::as.tibble()` instead of `tibble::as_tibble()`.
- Add example for calculating feature representation a solution in tabular
  format output by `solve()` (#110).
- Fix several typos in documentation.
- Thrown warnings are now immediately visible.
- Update references in the publication record vignette.
- Specify English (US) in the DESCRIPTION file.

# prioritizr 4.0.4

- Release candidate for CRAN.

# prioritizr 4.0.3.1

- Retain debugging symbols to conform with CRAN policies.

# prioritizr 4.0.3

- Release candidate for CRAN.

# prioritizr 4.0.2.16

- Add new citations.

# prioritizr 4.0.2.15

- Fix typos in documentation for `add_boundary_penalties()` and
  `add_connectivity_penalties()` function (#106).

# prioritizr 4.0.2.14

- Fix bug where use of `add_rsymphony_solver()` and `add_lpsymphony_solver()`
  sometimes returned infeasible solutions when subjected to a
  time limit (#105).

# prioritizr 4.0.2.13

- Fix assorted bugs in the render, setter, and getter parameter functions for
  `ConservationProblem-class` objects. These methods were implemented to be
  used in future interactive applications and are not currently used in the
  package. As a consequence, these bugs do not affect the correctness of
  any results.

# prioritizr 4.0.2.12

- Fix `bad error message` error being thrown when input rasters are not
  comparable (i.e. same coordinate reference system, extent, resolutions, and
  dimensionality) (#104).
- Add Domisch _et al._ (2019) to publication record vignette.

# prioritizr 4.0.2.11

- Fix issue `solve()` printing annoying text about `tbl_df` (#75).

# prioritizr 4.0.2.10

- Tweak `add_max_features_objective()` example code.

# prioritizr 4.0.2.9

- Update publication record vignette.

# prioritizr 4.0.2.8

- Fix bug where the `add_neighbor_constraints()` and
  `add_contiguity_constraints()` functions used more memory than they actually
  needed (#102). This is because the argument validation code converted sparse
  matrix objects (i.e. `dgCMatrix`) to base objects (i.e. `matrix`) class
  temporarily. This bug only meant inefficient utilization of computer
  resources---it did not affect the correctness of any results.

# prioritizr 4.0.2.7

- New `add_mandatory_allocation_constraints()` function. This function can be
  used to ensure that every planning unit is allocated to a management zone in
  the solution. It is useful when developing land-use plans where every single
  parcel of land must be assigned to a specific land-use zone.
- Add the `add_mandatory_allocation_constraints()` to the Management Zones and
  Prioritizr vignettes.
- Fix bug in the `$find(x)` method for `Collection` prototypes that caused
  it to throw an error incorrectly. This method was not used in earlier versions
  of this package.

# prioritizr 4.0.2.6

- Fix bug the `feature_representation()` function that caused the "amount_held"
  column to have NA values instead of the correct values. This bug only
  affected problems with multiple zones.

# prioritizr 4.0.2.5

- Fix bug in argument validation code for the `category_layer()` function that
  it this function to incorrectly throw an error claiming that the input
  argument to `x` was invalid when it was in fact valid. This bug is
  encountered when different layers the argument to `x` have non-NA values in
  different cells.

# prioritizr 4.0.2.4

- Update instructions for activating _Gurobi_ licenses on remote machines (#98).

# prioritizr 4.0.2.3

- The `add_contiguity_constraints()` function now uses sparse matrix formats
  internally for single-zone problems. This means that the constraints
  can be applied to single-zoned problem with many more planning units.

# prioritizr 4.0.2.2

- The `add_connectivity_penalties()` function now uses sparse matrix formats
  internally for single-zone problems. This means that connectivity penalties
  can be applied to single-zoned problem with many more planning units.

# prioritizr 4.0.2.1

- Update warning text when compiling problems that contain (i) objective
  functions that do not use targets and (ii) targets (#93).
- Update documentation for the `add_max_utility_objective()` and
  `add_max_cover_objective()` functions to make it clearer that they
  do not use targets (#94).

# prioritizr 4.0.2

- Release candidate for CRAN.

# prioritizr 4.0.1.6
- Fix bug in `add_locked_in_constraints()` and `add_locked_out_constraints()`
  that incorrectly threw an error when using `logical` locked data
  (i.e. `TRUE`/`FALSE`) because it incorrectly thought that valid inputs were
  invalid.
- Fix bug in `add_locked_in_constraints()`, `add_locked_out_constraints()`,
  and `add_manual_locked_constraints()` where solving the same problem object
  twice resulted in incorrect planning units being locked in or out of the
  solution (#92).
- Added unit tests for objectives, constraints, decisions, targets, and
  penalties to ensure that solving problems twice does not result in
  different solutions.

# prioritizr 4.0.1.5
- Fix bug in `feature_abundances()` that caused the solve function to throw an
  error when attempting to solve problems with a single feature.
- Fix bug in `add_cuts_portfolio()` that caused the portfolio to return
  solutions that were not within the specified optimality gap when using the
  _Gurobi_ solver.
- Add the ability to specify the search pool method and number of solutions to
  the `add_pool_portfolio()` function.

# prioritizr 4.0.1.4
- The `feature_representation()` function now allows `numeric` solutions with
  attributes (e.g. when output by the `solve()` function) when calculating
  representation statistics for problems with `numeric` planning unit data
  (#91).
- The `add_manual_targets()` function threw a warning when some features had
  targets equal to zero. This resulted in an excessive amount of warnings. Now,
  warnings are thrown for targets that are less then zero.
- The `problem()` function sometimes incorrectly threw a warning that feature
  data had negative values when the data actually did not contain negative
  values. This has now been addressed.

# prioritizr 4.0.1.3
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

- Fix `add_max_cover_objective()` in _prioritizr_ vignette (#90).

# prioritizr 4.0.1.1

- The `add_relative_targets()` documentation now makes it clear that locked out
  planning units are included in the calculations for setting targets (#89).
- The `add_loglinear_targets()` function now includes a `feature_abundances()`
  parameter for specifying the total amount of each feature to use when
  calculating the targets (#89).

# prioritizr 4.0.1

- Release candidate for CRAN.

# prioritizr 4.0.0.12

- New `feature_abundances()` function to calculate the total amount of each
  feature in the planning units (#86).

# prioritizr 4.0.0.11

- Fix some equations in the documentation (#83).

# prioritizr 4.0.0.10

- Add version requirements for _assertthat_ and _tibble_ packages (#82).

# prioritizr 4.0.0.9

- Fix minor typos in the _Gurobi installation guide_ and update the
  _Management zones tutorial_.

# prioritizr 4.0.0.8

- Add instructions for setting up the _Gurobi_ Academic license on a computer
  that it is not connected to a university computer network using a computer
  that is on an academic network (#81). For example, these instructions could be
  used to set up _Gurobi_ on a cloud-based system using a laptop computer that
  is connected a university's wireless network.

# prioritizr 4.0.0.7

- The `add_cuts_portfolio()` function uses the _Gurobi_ solution pool to
  generate unique solutions within a specified gap of optimality when tasked
  with solving problems with _Gurobi_ (version 8.0.0+; #80).

# prioritizr 4.0.0.6

- New `add_pool_portfolio()` function to generate a portfolio of solutions using
  the _Gurobi_ solution pool (#77).

# prioritizr 4.0.0.5

- The `boundary_matrix()` function now has the experimental functionality to
  use GEOS STR trees to speed up processing (#74).
- Solutions obtained from _Gurobi_ that contain binary-type decisions are
  explicitly rounded to the nearest integer. This is because _Gurobi_ can output
  solutions to binary problems that contain values which not exactly zero or
  one (e.g. 0.9999997 using default settings; #78).

# prioritizr 4.0.0.4

- New `feature_representation()` function to how well features are represented
  in solutions (#73).

# prioritizr 4.0.0.3

- The _prioritizrdata_ package has been listed under Suggests.
- The vignettes in the _prioritizrdata_ package have been moved to this package
  to make them easier to find.

# prioritizr 4.0.0.2

- Fix issue with the `solve()` function printing superfluous text (#75).

# prioritizr 4.0.0.1

- Minor improvements to the documentation for the `problem()` function.

# prioritizr 4.0.0.0

- Added functionality to build and solve problems with multiple management
  zones (#14).
- New built-in datasets `sim_pu_zones_stack`, `sim_pu_zones_polygons`,
  and `sim_features_zones` for exploring conservation problems with
  multiple management zones.
- New `zones` function and `Zones` class to organize data with multiple
  zones.
- The `problem()` function now accepts `Zone` objects as arguments for
  `feature` to create problems with multiple zones.
- The `add_relative_targets()` and `add_absolute_targets()` functions for adding
  targets to problems can be used to specify targets for each feature in
  each zone.
- New `add_manual_targets()` function for creating targets that pertain to
  multiple management zones.
- The `solve()` function now returns a `list` of solutions when generating
  a portfolio of solutions.
- The `add_locked_in_constraints()` and `add_locked_out_constraints()`
  functions for specifying which planning units are locked in or out
  now accept `matrix` arguments for specifying which zones are locked
  in or out.
- New `add_manual_locked_constraints()` function to manually specify which
  planning units should or shouldn't be allocated to specific zones in
  solutions.
- All functions for adding constraints and penalties now have
  parameters that specify how they should treat planning units allocate to
  different zones (using the `zones` parameter) and specify how they
  they should be applied (using the `data` parameter. All of these functions
  have default arguments that mean that problems with a single zone
  should have the same optimal solution as problems created in the earlier
  version of the package.
- The `add_feature_weights()` function can be used to weight different
  the representation of each feature in each zone.
- The `binary_stack()`, `category_layer()`, and `category_vector()` functions
  have been provided to help work with data for multiple management zones.
- New _Management zones_ vignette on building and solving problems with
  multiple management zones.
- Added mention of zones functionality to package DESCRIPTION, summary (in
  `?prioritizr`), and README.
- The _Quick Start Guide_ and _Prioritizr Basics_ vignettes have been
  consolidated into the _prioritizr_ vignette.
- The `marxan_problem()` has been updated with more comprehensive documentation
  and to provide more helpful error messages. For clarity, it will now only
  work with tabular data in the standard _Marxan_ format.

# prioritizr 3.0.3.6

- Fix typo in README and update documentation for `add_boundary_penalties()`
  (#62).

# prioritizr 3.0.3.5

- Fix bug where `add_locked_in_constraints()` and `add_locked_out_constraints()`
  throw an exception when used with semi-continuous-type decisions (#59).
- Exception in `compile()` thrown when the same planning unit is locked in and
  locked out now prints the planning unit indices in a readable format.

# prioritizr 3.0.3.4

- Fix bug where `add_locked_in_constraints()` and `add_locked_out_constraints()`
  are ignored when using proportion-type decisions (#58).

# prioritizr 3.0.3.3

- Fix bug in `predefined_optimization_problem()` which incorrectly recognized
  some inputs as invalid when they were in fact valid.
- Addressed NOTE in `R CMD check` related to proto in Depends.

# prioritizr 3.0.3.2

- Moved proto package from Imports to Depends in DESCRIPTION

# prioritizr 3.0.3.1

- Depends on R version 3.4.0 (avoids 'patchlevel 0' NOTE/WARNING in checks)

# prioritizr 3.0.3

- Release candidate for CRAN.

# prioritizr 3.0.2.3

- Unit tests that fail when using _lpsymphony_ due to a bug in _lpsymphony_
  are now skipped (partially addressing #40).

# prioritizr 3.0.2.2

- `add_lpsymphony_solver()` now throws warnings to alert users to potentially
  incorrect solutions (partially addressing #40).

# prioritizr 3.0.2.1

- Vignette sizes have been reduced.

# prioritizr 3.0.2

- Release candidate for CRAN. Release postponed due issues on Travis CI.

# prioritizr 3.0.1.1

- Unit tests for `add_*_objectives` now pass when executed with slow solvers
  (partially addressing #40).
- `compile()` now works when no solvers are installed (#41).
- Gap arguments in `add_*_solvers` are now unbounded and can accept values
  larger than 1 (#44).

# prioritizr 3.0.1

- Release candidate for CRAN.

# prioritizr 3.0.0.0

- The `add_max_cover_objective()` function has been renamed to the
  `add_max_utility_objective()`, because the formulation does not follow the
  historical formulation of the maximum coverage reserve selection problem
  (#38).
- The `add_max_cover_objective()` function now follows the historical maximum
  coverage objective. This fundamentally changes `add_max_cover_objective()`
  function and breaks compatibility with previous versions (#38).
- Update `add_lpsymphony_solver()` examples and tests to skip on Linux
  operating systems.
- Add tests to unit tests that were being skipped in new version of _testthat_
  package.

# prioritizr 2.0.4.1

- Fix bug with `add_lpsymphony_solver()` causing error when attempting to solve
  problems.

# prioritizr 2.0.4

- Release candidate for CRAN. Release postponed due to bug report.

# prioritizr 2.0.3.1

- Fix bug when solving problems with `numeric` vector data that caused an error.
- Fix bug in compiling problems with `numeric` vector input with rij data
  containing NA values.
- Added unit tests for solving problems with various input formats.
- Updated package sizes reported in cran-comments.

# prioritizr 2.0.3

- Initial release candidate for CRAN. Release postponed due to bug report.

# prioritizr 2.0.2.9

- Added vignette to record publications that use _prioritizr_ (#35).

# prioritizr 2.0.2.8

- Unit tests now compatible with development version of _testthat_ (#34).

# prioritizr 2.0.2.7

- Fix bug in `apply_boundary_penalties()` and `add_connectivity_penalties()`
  causing the function to throw an error when the number of boundaries/edges is
  less than the number of planning units.

# prioritizr 2.0.2.6

- Makevars now compatible with Mac OSX Sierra (#33).

# prioritizr 2.0.2.5

- Fix bug in `boundary_matrix()` calculations (#30).

# prioritizr 2.0.2.4

- Minor tweaks to vignettes.

# prioritizr 2.0.2.3

- Add logo to README files and package website (#31).

# prioritizr 2.0.2.2

- Broad-scale improvements to documentation.
- Fix documentation for `add_max_phylo_objective()` (#24).
- Update Gurobi Installation vignette.
- Remove _prioritizrdata_ from package Suggests.
- Add _shiny_ and _xtable_ to Suggests for rendering parameters.
- Added code for `ScalarParameter` and `ArrayParameter` prototypes to check t
  that  functions for generating widgets have their dependencies installed.
- URLs for _lpsymphony_ on Bioconductor now use the package's DOI.
- Add more comprehensive tests to portfolios.
- Fix bug when `numeric` planning unit data and portfolios that caused the
  `solve()` to throw an error.
- Remove R-devel from AppVeyor testing because it fails for unknown
  reasons.

# prioritizr 2.0.2.1

- Removed shiny functions for now to prep for CRAN release.
- Rebuilt website and documentation.

# prioritizr 2.0.2.0

- Included vignette on Gurobi solver installation and testing.

# prioritizr 2.0.1.0

- Fixed bug where `Spatial*DataFrame` input to `marxan_problem()` would always
  use the first column in the attribute table for the cost data. **This bug is
  serious** so analysis that used `Spatial*DataFrame` inputs in
  `marxan_problem()` should be rerun.
- Added functionality to use feature abundance/occurrence data stored as
  columns in the planning unit data when constructing `problem()` objects.

# prioritizr 2.0.0.2

- Skip `add_cuts_portfolio()` on Travis.

# prioritizr 2.0.0.1

- Skip `add_cuts_portfolio()` and `add_shuffle_portfolio()` tests on
  CRAN.

# prioritizr 2.0.0.0

- This version breaks compatibility with previous releases because
  solutions in `data.frame` and `Spatial*DataFrame` objects
  are now stored in columns named "solution_*" (e.g. "solution_1")
  to store multiple solutions.
- Added support for multiple solutions (#23).
- Solutions now contain additional information in stored in the object's
  attributes (#24). See README.Rmd for examples on accessing this
  information.
- Tidy examples in _add_gurobi_solver.R_, _add_lpsymphony_solver.R_,
  _add_rsymphony_solver.R_, and _solvers.R_.
- Add logical `verbose` argument to all solvers. This replaces the `verbosity`
  argument in `add_lpsymphony_solver()` and `add_rsymphony_solver()`.
- The verbosity of information presented when solving problems using
  `add_lpsymphony_solver()` and `add_rsymphony_solver()` is reduced.
- Assorted spelling mistakes have been fixed.

# prioritizr 1.0.2.3

- `ConservationProblem$print()` now only prints the first three species names
  and a count of the total number of features. This update means that
  `ConservationProblem` objects with lots of features can now safely be printed
  without polluting the R console.
- Attempt to make equations in help files prettier.
- Fix bug where _lpsymphony_ and _Rsymphony_ solvers would return solutions
  containing NA values if they did not find a feasible solution within
  the argument to `time_limit`.

# prioritizr 1.0.2.2

- Fix #19.

# prioritizr 1.0.2.1

- Fix #20.

# prioritizr 1.0.2.0

- Passes CRAN checks on Winbuilder.
- Added roxygen2 to Suggests for Travis CI.

# prioritizr 1.0.1.6

- Simplify vignette workflow. Vignettes can now be compiled by using
  `devtools::build_vignettes()`. Earlier versions needed the vignettes to be
  compiled using the _Makefile_ to copy files around to avoid tangled R code
  causing failures during R CMD CHECK. Although no longer needed, the vignettes
  can still be compiled using the shell command `make vigns` if
  desired.
- The _README.Rmd_ now lives in the top-level directory following standard
  practices. It should now be complied using `rmarkdown::render("README.Rmd")`
  or using the shell command `make readme`. Note that the figures for
  `README.md` can be found in the directory `man/figures`.
- The example for `prshiny` will now only be run if executed during an
  interactive R session. Prior to this R CMD CHECK would hang.
- UTF-8 math characters in vignettes have been replaced with with MathJax
  compatible latex expressions.
- R code in the vignettes has been linted to follow the package's style guide.
- Fix example in vignette _quick_start.Rmd_ showing how to run
  `marxan_problem()` using input `data.frame()` objects.
- Fix bug in vignette _quick_start.Rmd_ counting number of selected planning
  units
- Make the _data.table_ package automatically installed when _prioritizr_ is
  installed (#18).
- Move _shiny_, _shinydashboard_, and _leaflet_ packages to Imports to avoid
  polluting users environment.
- Update preliminary versions of the shiny apps to call functions from other
  packages explicitly.
- _README.Rmd_ tweaks to make it look prettier on website
- Lint objective function definition files
- Remove "\text" latex sequences from objective function definition files
  because CRAN doesn't support _amsmath_ extensions in equations.
- Update examples in objective function files to only show relevant objectives
- Added _rmarkdown_ package to Suggests following recommended practices.

# prioritizr 1.0.1.5

- Enable 64 bit Armadillo flag. This increases the maximum size of problems
  that can be solved.
- Disable bound-checks in Armadillo matrix operations. This should reduce
  processing time when running the `compile()` function.

# prioritizr 1.0.1.4

- Fix bug in `problem.data.frame` that meant that it did not check for missing
  values in `rij$pu`.

# prioritizr 1.0.1.3

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

- Fix #21.

# prioritizr 1.0.1.1

- Add roxygen2 to package SUGGESTS for building vignettes.

# prioritizr 1.0.1.0

- Fix issue where `parallel::detectCores()` returns `NA` on some systems
  preventing users from using the Gurobi solver--even when one thread is
  specified.

# prioritizr 1.0.0.5

- Fix building issue due to incorrect file order in DESCRIPTION.

# prioritizr 1.0.0.4

- Compatibility with R 3.4.0.
- Replace `structure(NULL, ...)` with `structure(list(), ...)`.
- Register compiled library files.
- Remove duplicate definition of `new_waiver()`.
- Tests check if prioritizrdata package not installed and skip if it isn't.

# prioritizr 1.0.0.3

- Fix missing links in documentation
- Fix typos in roxygen2 parameters
- Move `add_default_decisions()` and `add_default_solver()` to own help file
- Make `add_default_objectives()` and `add_default_targets()` private functions

# prioritizr 1.0.0.2

- Fix #13

# prioritizr 1.0.0.1

- Fix #8.
- Fix bug in `add_corridor_constraints()` that fails to actually add the
  constraints with argument to `connectivity` is a list.
- Fix bug in `make install` command so that it now actually installs the
  package.
- Fix link to Joe's website in the package's website.

# prioritizr 1.0.0.0

- R interface fully functional.

# prioritizr 0.1.2.9

- Package re-implementation.

# prioritizr 0.1.2

- Prepare for CRAN submission.
- Add continuous integration.
- Fixed various bugs.
- Introduce maximum target coverage model.
- Add full vignette in addition to quickstart guide.

# prioritizr 0.1.1

- Initial package version.
