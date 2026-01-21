# Fast extract

Extract data from a
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

## Usage

``` r
fast_extract(x, y, ...)

# S4 method for class 'Raster,Spatial'
fast_extract(x, y, fun = "mean", ...)

# S4 method for class 'Raster,sfc'
fast_extract(x, y, fun = "mean", ...)

# S4 method for class 'SpatRaster,sfc'
fast_extract(x, y, fun = "mean", ...)

# S4 method for class 'Raster,sf'
fast_extract(x, y, fun = "mean", ...)

# S4 method for class 'SpatRaster,sf'
fast_extract(x, y, fun = "mean", ...)
```

## Arguments

- x:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object.

- y:

  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object.

- ...:

  not used.

- fun:

  `character` name of statistic to summarize data. Defaults to `"mean"`.
  Available options include `"sum"` or `"mean"`. Defaults to `"mean"`.

## Value

A `matrix` containing the summary amount of each feature within each
planning unit. Rows correspond to different spatial features in the
argument to `y` and columns correspond to different raster layers in the
argument to `x`.

## Details

The performance of this function for large
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
objects can be improved by increasing the GDAL cache size. The default
cache size is 25 MB. For example, the following code can be used to set
the cache size to 4 GB.

    terra::gdalCache(size = 4000)

This function is simply a wrapper that uses
[`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
for polygon geometries, and
[`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
for other geometry types.

## See also

[`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html),
[`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html).

## Examples

``` r
# load data
sim_pu_polygons <- get_sim_pu_polygons()
sim_features <- get_sim_features()

# extract data
result <- fast_extract(sim_features, sim_pu_polygons)

# show result
print(head(result))
#>           [,1]      [,2]      [,3]      [,4]      [,5]
#> [1,] 0.7150548 0.2900901 0.8178213 0.2199663 0.4533809
#> [2,] 0.6990429 0.3052216 0.8064534 0.2713800 0.4413639
#> [3,] 0.6859317 0.3266036 0.7852441 0.3296247 0.4343547
#> [4,] 0.6783193 0.3510977 0.7541320 0.3900085 0.4355628
#> [5,] 0.6782107 0.3750470 0.7131559 0.4468281 0.4474871
#> [6,] 0.6863253 0.3950113 0.6628435 0.4946333 0.4714105
```
