# Do run example?

Determine if the session is suitable for executing long-running a
example.

## Usage

``` r
do_run_example()
```

## Value

A `logical` value.

## Details

This function will return `TRUE` if the session is interactive.
Otherwise, it will only return `TRUE` if the session does not have
system environmental variables that indicate that the session is being
used for package checks, or for building documentation.

## Examples

``` r
# should examples be run in current environment?
do_run_example()
#> [1] TRUE
```
