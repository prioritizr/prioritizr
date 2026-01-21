# Binary stack

Convert a single-layer
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object that contains integer values into a multi-layer
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object with cell values denote the presence/absence of a given integer
value. This is methodology is also known as "one-hot encoding".

## Usage

``` r
binary_stack(x, keep_all = TRUE)

# S3 method for class 'Raster'
binary_stack(x, keep_all = TRUE)

# S3 method for class 'SpatRaster'
binary_stack(x, keep_all = TRUE)
```

## Arguments

- x:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object with a single layer that contains integer values.

- keep_all:

  `logical` value indicating if all integers should be kept in the
  output. If `TRUE`, the output will contain a layer for each sequential
  integer between 1 and the maximum value in `x`. If `FALSE`, the output
  will only contain layers for integer values present in `x`. Defaults
  to `TRUE.`

## Value

A
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

## Details

This function is provided to help manage data that encompass multiple
management zones. For instance, this function may be helpful for
preparing raster data for
[`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md)
and
[`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.md)
since they require binary rasters as input arguments. It is essentially
a wrapper for
[`terra::segregate()`](https://rspatial.github.io/terra/reference/segregate.html).
Note that this function assumes `x` contains integer values.

## See also

The
[`category_layer()`](https://prioritizr.net/reference/category_layer.md)
function performs the reverse of this function. Also the
[`terra::segregate()`](https://rspatial.github.io/terra/reference/segregate.html)
function provides similar functionality.

## Examples

``` r
# create raster with categorical values
x <- terra::rast(matrix(c(1, 2, 4, 0, NA, 1), nrow = 3))

# plot the raster
# \dontrun{
plot(x, main = "x")

# }

# convert to binary stack
y <- binary_stack(x)

# plot result
# \dontrun{
plot(y)

# }
```
