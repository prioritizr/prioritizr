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
  Packages suggested but not available for checking: 'gurobi'

    **The package uses the gurobi R package that is distributed with Gurobi software suite (and not available on CRAN).**

## Test environments

* [Ubuntu 16.04, R-release (travis-ci)](https://travis-ci.org/prioritizr/prioritizr/builds)
* [Ubuntu 16.04, R-devel (travis-ci)](https://travis-ci.org/prioritizr/prioritizr/builds)
* [Mac OSX 10.13.6, R-release (travis-ci](https://travis-ci.org/prioritizr/prioritizr/builds)
* [Windows Server 2012 R2 (x64), R-release (appveyor)](https://ci.appveyor.com/project/jeffreyhanson/prioritizr)
* Windows Server 2008 (x64), R-release (win-builder)
* Windows Server 2008 (x64), R-devel (win-builder)

## Downstream dependencies

There are no existing packages that depend on this package.
