## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

* checking installed package size ... NOTE
   installed size is  6.6Mb
   sub-directories of 1Mb or more:
     doc    1.2Mb
     libs   3.2Mb

    **The package makes extensive use of C++ code to reduce run time.**

* checking package dependencies ... NOTE
  Packages suggested but not available for checking: 'gurobi', 'rcbc'

    **The package provides the (optional) functionality to solve optimization problems using the gurobi and rcbc R packages. The gurobi R package is distributed with Gurobi software suite (and not available on CRAN), and the rcbc R package is only available on GitHub (https://github.com/dirkschumacher/rcbc). The package documentation provides instructions for installing these R packages.**

## Test environments

* [Ubuntu 20.04, R-release](https://github.com/prioritizr/prioritizr/actions?query=workflow%3AUbuntu)
* [Ubuntu 20.04, R-devel](https://github.com/prioritizr/prioritizr/actions?query=workflow%3AUbuntu)
* [Mac OSX 10.15, R-release](https://github.com/prioritizr/prioritizr/actions?query=workflow%3A%22Mac+OSX%22)
* [Windows Server 2019, R-release](https://github.com/prioritizr/prioritizr/actions?query=workflow%3AWindows)
* [Windows Server 2019, R-devel](https://github.com/prioritizr/prioritizr/actions?query=workflow%3AWindows)
* Windows Server 2008 (x64), R-devel (win-builder)

## Downstream dependencies

There are no existing packages that depend on this package.
