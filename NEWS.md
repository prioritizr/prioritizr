# prioritizr 4.0.0.12

- New `feature_abundances` function to calculate the total amount of each 
  feature in the planning units (#86).

# prioritizr 4.0.0.11

- Fix some equations in the documentation (#83).

# prioritizr 4.0.0.10

- Add version requirements for _assertthat_ and _tibble_ (#82).

# prioritizr 4.0.0.9

- Fix minor typos in the _Gurobi_ Installation Guide and update the Management
  Zones tutorial.

# prioritizr 4.0.0.8

- Add instructions for setting up the _Gurobi_ Academic license on a computer
  that it is not connected to a university computer network using a computer
  that is on an academic network (#81). For example, these instructions could be
  used to set up _Gurobi_ on a cloud-based system using a laptop computer that
  is connected a university's wireless network.

# prioritizr 4.0.0.7

- The `add_cuts_portfolio` function uses the _Gurobi_ solution pool to generate
  unique solutions within a specified gap of optimality when tasked with
  solving problems with _Gurobi_ (version 8.0.0+; #80).

# prioritizr 4.0.0.6

- New `add_pool_portfolio` function to generate a portfolio of solutions using
  the _Gurobi_ solution pool (#77).

# prioritizr 4.0.0.5

- The `boundary_matrix` function now has the experimental functionality to
  use GEOS STR trees to speed up processing (#74).
- Solutions obtained from _Gurobi_ that contain binary-type decisions are
  explicitly rounded to the nearest integer. This is because _Gurobi_ can output
  solutions to binary problems that contain values which not exactly zero or
  one (e.g. 0.9999997 using default settings; #78).

# prioritizr 4.0.0.4

- New `feature_representation` function to how well features are represented in
  solutions (#73).

# prioritizr 4.0.0.3

- The _prioritizrdata_ package has been listed under Suggests.
- The vignettes in the _prioritizrdata_ package have been moved to this package
  to make them easier to find.

# prioritizr 4.0.0.2

- Fix issue with the `solve` function printing superfluous text (#75).

# prioritizr 4.0.0.1

- Minor improvements to the documentation for the `problem` function.

# prioritizr 4.0.0.0

- Added functionality to build and solve problems with multiple management
  zones (#14).
    - New built-in data sets `sim_pu_zones_stack`, `sim_pu_zones_polygons`,
      and `sim_features_zones` for exploring conservation problems with
      multiple management zones.
    - New `zones` function and `Zones` class to organize data with multiple
      zones.
    - The `problem` function now accepts `Zone` objects as arguments for
      `feature` to create problems with multiple zones.
    - The `add_relative_targets` and `add_absolute_targets` functions for adding
      targets to problems can be used to specify targets for each feature in
      each zone.
    - New `add_manual_targets` function for creating targets that pertain to
      multiple management zones.
    - The `solve` function now returns a `list` of solutions when generating
      a portfolio of solutions.
    - The `add_locked_in_constraints` and `add_locked_out_constraints`
      functions for specifying which planning units are locked in or out
      now accept `matrix` arguments for specifying which zones are locked
      in or out.
    - New `add_manual_locked_constraints` function to manually specify which
      planning units should or shouldn't be allocated to specific zones in
      solutions.
    - All functions for adding constraints and penalties now have
      parameters that specify how they should treat planning units allocate to
      different zones (using the `zones` parameter) and specify how they
      they should be applied (using the `data` parameter. All of these functions
      have default arguments that mean that problems with a single zone
      should have the same optimal solution as problems created in the earlier
      version of the package.
    - The `add_feature_weights` function can be used to weight different
      the representation of each feature in each zone.
    - The `binary_stack`, `category_layer`, and `category_vector` functions have
      been provided to help work with data for multiple management zones.
    - New _Management zones_ vignette on building and solving problems with
      multiple management zones.
    - Added mention of zones functionality to package DESCRIPTION, summary (in
      `?prioritizr`), and README.
- The _Quick Start Guide_ and _Prioritizr Basics_ vignettes have been
  consolidated into the _prioritizr_ vignette.
- The `marxan_problem` has been updated with more comprehensive documentation
  and to provide more helpful error messages. For clarity, it will now only
  work with tabular data in the standard _Marxan_ format.

# prioritizr 3.0.3.6

- Fix typo in README and update documentation for `add_boundary_penalties`
  (#62).

# prioritizr 3.0.3.5

- Fix bug where `add_locked_in_constraints` and `add_locked_out_constraints`
  throw an exception when used with semi-continuous-type decisions (#59).
- Exception in `compile` thrown when the same planning unit is locked in and
  locked out now prints the planning unit indices in a readable format.

# prioritizr 3.0.3.4

- Fix bug where `add_locked_in_constraints` and `add_locked_out_constraints`
  are ignored when using proportion-type decisions (#58).

# prioritizr 3.0.3.3

- Fix bug in `predefined_optimization_problem` which incorrectly recognized
  some inputs as invalid when they were in fact valid.
- Addressed NOTE in `R CMD check` related to proto in Depends.

# prioritizr 3.0.3.2

- Moved proto package from Imports to Depends in DESCRIPTION

# prioritizr 3.0.3.1

- Depends on R version 3.4.0 (avoids 'patchlevel 0' NOTE/WARNING in checks)

# prioritizr 3.0.3 (released)

- Release candidate for CRAN.

# prioritizr 3.0.2.3

- Unit tests that fail when using _lpsymphony_ due to a bug in _lpsymphony_
  are now skipped (partially addressing #40).

# prioritizr 3.0.2.2

- `add_lpsymphony_solver` now throws warnings to alert users to potentially
  incorrect solutions (partially addressing #40).

# prioritizr 3.0.2.1

- Vignette sizes have been reduced.

# prioritizr 3.0.2

- Release candidate for CRAN. Release postponed due issues on Travis CI.

# prioritizr 3.0.1.1

- Unit tests for `add_*_objectives` now pass when executed with slow solvers
  (partially addressing #40).
- `compile` now works when no solvers are installed (#41).
- Gap arguments in `add_*_solvers` are now unbounded and can accept values
  larger than 1 (#44).

# prioritizr 3.0.1 (released)

- Release candidate for CRAN.

# prioritizr 3.0.0.0

- The `add_max_cover_objective` function has been renamed to the
  `add_max_utility_objective`, because the formulation does not follow the
  historical formulation of the maximum coverage  reserve selection problem
  (#38).
- The `add_max_cover_objective` function now follows the historical maximum
  coverage objective. This fundamentally changes `add_max_cover_objective`
  function and breaks compatibility with previous version of _prioritizr_ (#38).
- Modify _lpsymphony_ examples and tests to skip on Linux operating systems.
- Add tests to unit tests that were being skipped in new version of _testthat_.

# prioritizr 2.0.4.1

- Fix bug with `add_lpsymphony_solver` causing error when attempting to solve
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

- Fix bug in `apply_boundary_penalties` and `add_connectivity_penalties`
  causing the function to throw an error when the number of boundaries/edges is
  less than the number of planning units.

# prioritizr 2.0.2.6

- Makevars now compatible with Mac OSX Sierra (#33).

# prioritizr 2.0.2.5

- Fix bug in `boundary_matrix` calculations (#30).

# prioritizr 2.0.2.4

- Minor tweaks to vignettes.

# prioritizr 2.0.2.3

- Add logo to README files and package website (#31).

# prioritizr 2.0.2.2

- Broad-scale improvements to documentation.
- Fix documentation for `add_max_phylo_objective` (#24).
- Update Gurobi Installation vignette.
- Remove _prioritizrdata_ from package Suggests.
- Add _shiny_ and _xtable_ to Suggests for rendering parameters.
- Added code for `ScalarParameter` and `ArrayParameter` prototypes to check t
  that  functions for generating widgets have their dependencies installed.
- URLs for _lpsymphony_ on Bioconductor now use the package's DOI.
- Add more comprehensive tests to portfolios.
- Fix bug when `numeric` planning unit data and portfolios that caused the
  `solve` to throw an error.
- Remove R-devel from AppVeyor testing because it fails for unreproducible
  reasons.

# prioritizr 2.0.2.1

- Removed shiny functions for now to prep for CRAN release.
- Rebuilt website and documentation.

# prioritizr 2.0.2.0

- Included vignette on Gurobi solver installation and testing.

# prioritizr 2.0.1.0

- Fixed bug where `Spatial*DataFrame` input to `marxan_problem`  would always
  use the first column in the attribute table for the cost data. **This bug is
  serious** so analysis that used `Spatial*DataFrame` inputs in
  `marxan_problem` should be rerun.
- Added functionality to use feature abundance/occurrence data stored as
  columns in the planning unit data when constructing `problem` objects.

# prioritizr 2.0.0.2

- Skip `add_cuts_portfolio` on Travis.

# prioritizr 2.0.0.1

- Skip `add_cuts_portfolio` and `add_shuffle_portfolio` tests on
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
  argument in `add_lpsymphony_solver` and `add_rsymphony_solver`.
- The verbosity of information presented when solving problems using
  `add_lpsymphony_solver` and `add_rsymphony_solver` is reduced.
- Assorted spelling mistakes have been fixed.

# prioritizr 1.0.2.3

- `ConservationProblem$print()` now only prints the first three species names
  and a count of the total number of features. This update means that
  `ConservationProblem` objects with lots of features can now safely be printed
  without polluting the R console.
- Attempt to make equations in help files prettier.
- Fix bug where lpsymhpony and Rsymphony solvers would return solutions
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
- Fix example in vignette _quick_start.Rmd_ showing how to run `marxan_problem`
  using input `data.frame` objects.
- Fix bug in vignette _quick_start.Rmd_ counting number of selected planning
  units
- Make the _data.table_ package automatically installed when _prioritizr_ is
  installed. Address issue #18.
- Move _shiny_, _shinydashboard_, and _leaflet_ packages to Imports to avoid
  polluting users environment.
- Update preliminary versions of the shiny apps to call functions from other
  packages explicitly.
- _README.Rmd_ tweaks to make it look prettier on website
- Lint objective function definition files
- Remove "\text" latex sequences from objective function definition files
  because CRAN doesn't support _amsmath_ extensions in equations.
- Update examples in objective function files to only show relevant objectives
- Added _rmarkdown_ package to Suggests following [recommended
  practices](https://www.rforge.net/doc/packages/knitr/vignette_engines.html)

# prioritizr 1.0.1.5

- Enable 64 bit Armadillo flag. This increases the maximum size of problems
  that can be solved.
- Disable bound-checks in Armadillo matrix operations. This should reduce
  processing time when running the `priortizr::compile` function.

# prioritizr 1.0.1.4

- Fix bug in `problem.data.frame` that meant that it did not check for missing
  values in `rij$pu`.

# prioritizr 1.0.1.3

- Fix bugs `add_absolute_targets` and add_relative_targets` related to their
  standardGeneric being incorrectly defined
- Reduce installation size using Dirk Eddelbuettel's awesome advice:
  http://dirk.eddelbuettel.com/blog/2017/08/14#009_compact_shared_libraries
- Fix bug in `add_corridor_targets` when argument  `connectivities` is a `list`.
  The elements in the list are assumed to be `dsCMatrix` objects
  (aka symmetric sparse matrices in a compressed format) and are coerced
  to `dgCMatrix` objects to reduce computational burden. There was a typo,
  however, and so the objects were coerced to `dgCmatrix` and not `dgCMatrix`.
  This evidently was ok in earlier versions of the RcppArmadillo and/or
  Matrix packages but not in the most recent versions.

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
- Remove duplicate definition of `new_waiver` from internal.R.
- Tests check if prioritizrdata package not installed and skip if it isn't.

# prioritizr 1.0.0.3

- Fix missing links in documentation
- Fix typos in roxygen2 parameters
- Move `add_default_decisions` and `add_default_solver` to own help file
- Make `add_default_objectives` and `add_default_targets` private functions

# prioritizr 1.0.0.2

- Fix #13

# prioritizr 1.0.0.1

- Fix #8.
- Fix bug in `add_corridor_constraints` that fails to actually add the
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
