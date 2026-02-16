# Waiver

Create a `waiver` object.

## Usage

``` r
new_waiver()
```

## Value

A `Waiver` object.

## Details

This object is used to represent that the user has not manually
specified a setting, and so defaults should be used. By explicitly using
a `new_waiver()`, this means that `NULL` objects can be a valid setting.
The use of a waiver object was inspired by the `ggplot2` package.

## Examples

``` r
# create new waiver object
w <- new_waiver()

# print object
print(w)
#> list()
#> attr(,"class")
#> [1] "Waiver"
```
