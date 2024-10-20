Dear CRAN volunteers,

Thank you for reviewing this submission. It contains an update to
the prioritizr R package. Specifically, the update contains assorted minor improvements, bug fixes, and updates to the package documentation.

Cheers,

Richard Schuster

## R CMD check results

0 errors | 0 warnings | 2 notes

## CRAN check notes

* checking CRAN incoming feasibility ... NOTE

  Suggests or Enhances not in mainstream repositories:
    gurobi, rcbc, cplexAPI

  **The _cplexAPI_, _gurobi_, and _rcbc_ R packages provide interfaces to optimization software. Although they are not available on CRAN, we provide instructions for installing these R packages in the DESCRIPTION file and the package documentation (see `?add_cplex_solver`, `?add_gurobi_solver`, `?add_cbc_solver`). Additionally, comprehensive instructions for installing the _gurobi_ R package are also provided in the Gurobi Installation Guide vignette (see `vignette('gurobi_installation', package = "prioritizr")`).**

  **We also wish to justify our inclusion of the archived _cplexAPI_ R package as an optional dependency. The _prioritizr_ R package aims to provide users with the ability to solve optimization problems using a variety of different software, and the _cplexAPI_ R package provides an interface to the IBM CPLEX software. Although compiler warnings resulted in the archival of the _cplexAPI_ R package, we have confirmed that it still works correctly and the source code remains publicly available (https://github.com/cran/cplexAPI). Thus we are confident that users will be able to use the _cplexAPI_ R package. Although the _Rcplex_ R package -- which is available on CRAN -- aims to provide a similar interface to the IBM CPLEX software, it is not a suitable replacement. This is because the _Rcplex_ R package is not compatible with the latest version of the IBM CPLEX software. Despite attempts to contact the maintainer of the _Rcplex_ R package, we have not been successful and so the _Rcplex_ package remains unusable.**

* Found the following (possibly) invalid URLs:
  URL: https://support.gurobi.com/hc/en-us/articles/4534161999889-How-do-I-install-Gurobi-Optimizer
    From: inst/doc/gurobi_installation_guide.html
    Status: 403
    Message: Forbidden

  **I have checked this URL and confirm that it is correct.**

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

The package has two reverse dependencies on CRAN (i.e., prior3D, priorCON). I have checked that this submission is compatible with the latest versions of these packages. Both of these packages pass CRAN package checks with this new submission.
