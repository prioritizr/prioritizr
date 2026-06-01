# Category layer

Convert a multi-layer
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object into a single-layer
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object where pixel values indicate which input layer had the greatest
value.

## Usage

``` r
category_layer(x)

# S3 method for class 'Raster'
category_layer(x)

# Default S3 method
category_layer(x)
```

## Arguments

- x:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object containing multiple layers.

## Value

A
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object.

## Details

This function is provided to help manage data that encompass multiple
management zones. For instance, this function may be helpful for
interpreting solutions for problems associated with multiple zones that
have binary decisions. It is essentially a wrapper for
[`terra::which.max()`](https://rspatial.github.io/terra/reference/summarize-generics.html).

## See also

The [`binary_stack()`](https://prioritizr.net/reference/binary_stack.md)
function performs the reverse of this function.

## Examples

``` r
# create a binary raster stack
x <- terra::rast(list(
 terra::rast(matrix(c(1, 0, 0, 1, NA, 0), nrow = 3)),
 terra::rast(matrix(c(0, 1, 0, 0, NA, 0), nrow = 3)),
 terra::rast(matrix(c(0, 0, 1, 0, NA, 1), nrow = 3))
))

# plot data
plot(x)


# convert to category layer
y <- category_layer(x)

# plot result
plot(y)
```
