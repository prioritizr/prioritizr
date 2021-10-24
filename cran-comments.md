Dear CRAN maintainers,

Thank you for reviewing this submission. It aims to re-instate the package on CRAN after it was archived on 2021-10-06. Specifically, it fixes the issue that previously resulted in archival. The issue was that a couple of the examples needed certain packages to be installed, and these packages were only listed under the Suggests field of the DESCRIPTION. Thus checking the examples without these packages resulted in errors (i.e. because they are not available on all platforms or given the `noSuggests` checks). To address the issue, the relevant parts of the examples have been encapsulated in `\dontrun{}` markup. This approach was used because the examples can potentially take a long time to run (> 30 seconds). Additionally, this submission contains several bug fixes and improvements to the documentation (see NEWS.md).

Cheers,

Richard Schuster

## R CMD check results

0 errors | 0 warnings | 2 notes

## CRAN check notes

* checking CRAN incoming feasibility ... NOTE

  Suggests or Enhances not in mainstream repositories:
    gurobi, rcbc

  **The _gurobi_ and _rcbc_ R packages provide interfaces to optimization software. Although they are not available on CRAN, we provide instructions for installing these R packages in the DESCRIPTION file and the package documentation (see `?add_gurobi_solver`, `?add_cbc_solver`). Additioanlly, comprehensive instructions for installing the _gurobi_ R package are also provided in the Gurobi Installation Guide vignette (see `vignette('gurobi_installation', package = "prioritizr")`).**

* Package was archived on CRAN

  CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2021-10-06 as check problems were not
    corrected in time.

  **As previously mentioned, this version has been updated to fix the issues that resulted in archival.**

* Possibly misspelled words in DESCRIPTION:
  Gurobi (16:49, 18:5)
  MILP (7:18)
  optimality (12:5)
  pre (11:62)

  **These words are spelled correctly.**

* checking package dependencies ... NOTE
  Packages suggested but not available for checking: 'gurobi', 'rcbc'

  **The _gurobi_ and _rcbc_ R packages provide interfaces to optimization software. Although they are not available on CRAN, we provide instructions for installing these R packages in the package documentation (see `?add_gurobi_solver`, `?add_cbc_solver`). Comprehensive instructions for installing the _gurobi_ R package are also provided in the Gurobi Installation Guide vignette (see `vignette('gurobi_installation', package = "prioritizr")`). The DESCRIPTION file also provides information on installing these R packages.**

## Previous notes from CRAN maintainers

* Thanks, please omit the redundant "in R".

  **Thank you for this suggestion! The prioritizr R package was first released on CRAN in 2016. Since then it has been cited in nearly 30 scientific publications (see the publication record vignette), and has a worldwide community spanning over 100 countries (based on website tracking analytics). Although the "in R" text in the title is redundant, we worry that changing the title at this point in time would confuse the user base and invalidate previous work that has cited this package. As such, we would prefer to keep the current title.**

## Test environments

* [Ubuntu 20.04, R-release](https://github.com/prioritizr/prioritizr/actions?query=workflow%3AUbuntu)
* [Ubuntu 20.04, R-devel](https://github.com/prioritizr/prioritizr/actions?query=workflow%3AUbuntu)
* [Mac OSX 10.15, R-release](https://github.com/prioritizr/prioritizr/actions?query=workflow%3A%22Mac+OSX%22)
* [Windows Server 2019, R-release](https://github.com/prioritizr/prioritizr/actions?query=workflow%3AWindows)
* Windows Server 2008 (x64), R-devel (win-builder)

## Downstream dependencies

There are no existing packages that depend on this package.
