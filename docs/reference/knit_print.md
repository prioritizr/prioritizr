# Print an object for knitr package.

This function is used to ensure that
[`problem()`](https://prioritizr.net/reference/problem.md) and
[`new_optimization_problem()`](https://prioritizr.net/reference/prioritizr-deprecated.md)
objects are displayed correctly in rmarkdown reports.

## Usage

``` r
knit_print.ConservationProblem(x, ...)

knit_print.MultiConservationProblem(x, ...)

knit_print.OptimizationProblem(x, ...)
```

## Arguments

- x:

  Object.

- ...:

  Not used.

## Value

A `character` vector for knitting.

## Details

This function should not be called directly. It is intended to be used
by the knitr package when displaying objects.
