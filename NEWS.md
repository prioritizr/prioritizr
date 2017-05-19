# prioritizr 1.0.1.0 (unreleased)

- fix issue where `parallel::detectCores()` returns `NA` on some systems
  preventing users from using the Gurobi solver--even when one thread is
  specified

# prioritizr 1.0.0.5 (unreleased)

- fix building issue due to incorrect file order in DESCRIPTION

# prioritizr 1.0.0.4 (unreleased)

- compatibility with R 3.4.0
- replace `structure(NULL, ...)` with `structure(list(), ...)`
- register compiled library files
- remove duplicate definition of `new_waiver` from internal.R
- tests check if prioritizrdata package not installed and skip if it isn't

# prioritizr 1.0.0.3 (unreleased)

- fix missing links in documentation
- fix typos in roxygen2 parameters
- move `add_default_decisions` and `add_default_solver` to own help file
- make `add_default_objectives` and `add_default_targets` private functions

# prioritizr 1.0.0.2 (unreleased)

- fix #13

# prioritizr 1.0.0.1 (unreleased)

- fix #8
- fix bug in `add_corridor_constraints` that fails to actually add the constraints with argument to `connectivity` is a list
- fix bug in `make install` command so that it now actually installs the package
- fix link to Joe's website in the package's website

# prioritizr 1.0.0.0 (unreleased)

- R interface fully functional

# prioritizr 0.1.2.9 (unreleased)

- package re-implementation

# prioritizr 0.1.2

- prepare for CRAN submission
- add continuous integration
- fixed various bugs
- introduce maximum target coverage model
- add full vignette in addition to quickstart guide

# prioritizr 0.1.1 (unreleased)

- initial package version
