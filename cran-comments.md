Dear CRAN maintainers,

I received an email from Prof Brian Ripley informing me that CRAN policies require packages to retain debugging symbols. In earlier versions of this package, debugging symbols were unconditionally stripped from shared object library files. This version addresses this issue.

Cheers,

Richard

## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

* checking installed package size ... NOTE
    installed size is 17.8Mb
    sub-directories of 1Mb or more:
      doc    1.8Mb
      libs  14.2Mb

    **The package makes extensive use of C++ code to reduce run time.**

## Test environments

* [Ubuntu 14.04, R-release (travis-ci)](https://travis-ci.org/prioritizr/prioritizr/builds)
* [Ubuntu 14.04, R-devel (travis-ci)](https://travis-ci.org/prioritizr/prioritizr/builds)
* [Mac OSX 10.9.5, R-release (travis-ci](https://travis-ci.org/prioritizr/prioritizr/builds)
* [Windows Server 2012 R2 (x64), R-release (appveyor)](https://ci.appveyor.com/project/jeffreyhanson/prioritizr)
* Windows Server 2008 (x64), R-release (win-builder)
* Windows Server 2008 (x64), R-devel (win-builder)

## Downstream dependencies

There are no existing packages that depend on this package.
