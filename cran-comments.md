# Comments for submission to CRAN

## Test environments

* [Ubuntu 14.04, R-release (travis-ci)](https://travis-ci.org/prioritizr/prioritizr/builds)
* [Ubuntu 14.04, R-devel (travis-ci)](https://travis-ci.org/prioritizr/prioritizr/builds)
* [Mac OSX 10.9.5, R-release (travis-ci](https://travis-ci.org/prioritizr/prioritizr/builds)
* [Mac OSX 10.9.5, R-devel (travis-ci](https://travis-ci.org/prioritizr/prioritizr/builds)
* [Windows Server 2012 R2 (x64), R-release (appveyor)](https://ci.appveyor.com/project/jeffreyhanson/prioritizr)
* [Windows Server 2012 R2 (x64), R-devel 2016-11-02 r71617 (appveyor)](https://ci.appveyor.com/project/jeffreyhanson/prioritizr)
* Windows Server 2008 (x64), R-devel (win-builder)

## Check results

0 errors | 0 warnings | 2 notes

## Notes

* Packages suggested but not available for checking: 'gurobi'

  The 'gurobi' R package is distributed with the Gurobi software suite. The
  package DESCRIPTION contains information on installing this packages.

* checking installed package size
    installed size is  8.1Mb
    sub-directories of 1Mb or more:
      libs   7.2Mb

  The package makes extensive use of compiled code to reduce processing time.

## Downstream dependencies

There are no existing packages that depend on this package.
