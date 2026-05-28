# Standardize unit to density per km²

Standardize number to density per km².

## Usage

``` r
as_per_km2(x, unit)
```

## Arguments

- x:

  `numeric` vector.

- unit:

  `character` vector of spatial units (e.g., `"km2"`, `"acres"`,
  `"hectares"`. For convenience, a single `character` value can be
  specified if all values in `x` are the same unit.

## Value

A `numeric` vector.

## Examples

``` r
as_per_km2(5, "km2")
#> [1] 5
as_per_km2(5, "acres")
#> [1] 1235.522
as_per_km2(c(5, 10), "ha")
#> [1]  500 1000
as_km2(c(5, 10), c("ha", "acres"))
#> [1] 0.05000000 0.04046873
```
