# Standardize unit to km²

Standardize number to km².

## Usage

``` r
as_km2(x, unit)
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
as_km2(5, "km2")
#> [1] 5
as_km2(5, "acres")
#> [1] 0.02023436
as_km2(c(5, 10), "ha")
#> [1] 0.05 0.10
as_km2(c(5, 10), c("ha", "acres"))
#> [1] 0.05000000 0.04046873
```
