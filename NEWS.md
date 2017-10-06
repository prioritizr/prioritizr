# prioritizr 2.0.1.0 (unreleased)

- Fixed bug where `Spatial*DataFrame` input to `marxan_problem`  would always
  use the first column in the attribute table for the cost data. **This bug is
  serious** so analysis that used `Spatial*DataFrame` inputs in
  `marxan_problem` should be rerun.
- Added functionality to use feature abundance/occurrence data stored as
  columns in the planning unit data when constructing `problem` objects.

# prioritizr 2.0.0.2 (unreleased)

- Skip `add_cuts_portfolio` on Travis.

# prioritizr 2.0.0.1 (unreleased)

- Skip `add_cuts_portfolio` and `add_shuffle_portfolio` tests on
  CRAN.

# prioritizr 2.0.0.0 (unreleased)

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

# prioritizr 1.0.2.3 (unreleased)

- `ConservationProblem$print()` now only prints the first three species names
  and a count of the total number of features. This update means that
  `ConservationProblem` objects with lots of features can now safely be printed
  without polluting the R console.
- Attempt to make equations in help files prettier.
- Fix bug where lpsymhpony and Rsymphony solvers would return solutions
  containing NA values if they did not find a feasible solution within
  the argument to `time_limit`.

# prioritizr 1.0.2.2 (unreleased)

- Fix #19.

# prioritizr 1.0.2.1 (unreleased)

- Fix #20.

# prioritizr 1.0.2.0 (unreleased)

- Passes CRAN checks on Winbuilder.
- Added roxygen2 to Suggests for Travis CI.

# prioritizr 1.0.1.6 (unreleased)

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

# prioritizr 1.0.1.5 (unreleased)

- Enable 64 bit Armadillo flag. This increases the maximum size of problems
  that can be solved.
- Disable bound-checks in Armadillo matrix operations. This should reduce
  processing time when running the `priortizr::compile` function.

# prioritizr 1.0.1.4 (unreleased)

- Fix bug in `problem.data.frame` that meant that it did not check for missing
  values in `rij$pu`.

# prioritizr 1.0.1.3 (unreleased)

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

# prioritizr 1.0.1.2 (unreleased)

- Fix #21.

# prioritizr 1.0.1.1 (unreleased)

- Add roxygen2 to package SUGGESTS for building vignettes.

# prioritizr 1.0.1.0 (unreleased)

- Fix issue where `parallel::detectCores()` returns `NA` on some systems
  preventing users from using the Gurobi solver--even when one thread is
  specified.

# prioritizr 1.0.0.5 (unreleased)

- Fix building issue due to incorrect file order in DESCRIPTION.

# prioritizr 1.0.0.4 (unreleased)

- Compatibility with R 3.4.0.
- Replace `structure(NULL, ...)` with `structure(list(), ...)`.
- Register compiled library files.
- Remove duplicate definition of `new_waiver` from internal.R.
- Tests check if prioritizrdata package not installed and skip if it isn't.

# prioritizr 1.0.0.3 (unreleased)

- Fix missing links in documentation
- Fix typos in roxygen2 parameters
- Move `add_default_decisions` and `add_default_solver` to own help file
- Make `add_default_objectives` and `add_default_targets` private functions

# prioritizr 1.0.0.2 (unreleased)

- Fix #13

# prioritizr 1.0.0.1 (unreleased)

- Fix #8.
- Fix bug in `add_corridor_constraints` that fails to actually add the
  constraints with argument to `connectivity` is a list.
- Fix bug in `make install` command so that it now actually installs the
  package.
- Fix link to Joe's website in the package's website.

# prioritizr 1.0.0.0 (unreleased)

- R interface fully functional.

# prioritizr 0.1.2.9 (unreleased)

- Package re-implementation.

# prioritizr 0.1.2

- Prepare for CRAN submission.
- Add continuous integration.
- Fixed various bugs.
- Introduce maximum target coverage model.
- Add full vignette in addition to quickstart guide.

# prioritizr 0.1.1 (unreleased)

- Initial package version.
