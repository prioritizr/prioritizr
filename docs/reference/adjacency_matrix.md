# Adjacency matrix

Create a matrix showing which planning units are spatially adjacent to
each other.

## Usage

``` r
adjacency_matrix(x, ...)

# S3 method for class 'Raster'
adjacency_matrix(x, directions = 4, ...)

# S3 method for class 'SpatRaster'
adjacency_matrix(x, directions = 4, ...)

# S3 method for class 'SpatialPolygons'
adjacency_matrix(x, ...)

# S3 method for class 'SpatialLines'
adjacency_matrix(x, ...)

# S3 method for class 'SpatialPoints'
adjacency_matrix(x, ...)

# S3 method for class 'sf'
adjacency_matrix(x, ...)

# Default S3 method
adjacency_matrix(x, ...)
```

## Arguments

- x:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  object representing planning units.

- ...:

  not used.

- directions:

  `integer` If `x` is a
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object, the number of directions in which cells should be considered
  adjacent: 4 (rook's case), 8 (queen's case), 16 (knight and one-cell
  queen moves), or "bishop" to for cells with one-cell diagonal moves.

## Value

A
[`Matrix::dsCMatrix`](https://rdrr.io/pkg/Matrix/man/dsCMatrix-class.html)
sparse symmetric matrix. Each row and column represents a planning unit.
Cells values indicate if different planning units are adjacent to each
other or not (using ones and zeros). To reduce computational burden,
cells among the matrix diagonal are set to zero. Furthermore, if the
argument to `x` is a
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object, then cells with `NA` values are set to zero too.

## Details

Spatial processing is completed using
[`sf::st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)
for [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html)
objects, and
[`terra::adjacent()`](https://rspatial.github.io/terra/reference/adjacent.html)
for
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
objects. Note that spatially overlapping planning units are considered
adjacent.

## Notes

In earlier versions (\< 5.0.0), this function was named as the
`connected_matrix` function. It has been renamed to be consistent with
other spatial association matrix functions.

## Examples

``` r
# \dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_pu_polygons <- get_sim_pu_polygons()

# create adjacency matrix using raster data
## crop raster to 9 cells
r <- terra::crop(sim_pu_raster, terra::ext(c(0, 0.3, 0, 0.3)))

## make adjacency matrix
am_raster <- adjacency_matrix(r)

# create adjacency matrix using polygon data
## subset 9 polygons
ply <- sim_pu_polygons[c(1:3, 11:13, 20:22), ]

## make adjacency matrix
am_ply <- adjacency_matrix(ply)

# plot data and the adjacency matrices

## plot raster and adjacency matrix
plot(r, main = "raster", axes = FALSE)

Matrix::image(am_raster, main = "adjacency matrix")


## plot polygons and adjacency matrix
plot(ply[, 1], main = "polygons")

Matrix::image(am_ply, main = "adjacency matrix")


# }
```
