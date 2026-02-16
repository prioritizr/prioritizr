# Proximity matrix

Create a matrix showing which planning units are within a certain
spatial proximity to each other.

## Usage

``` r
proximity_matrix(x, distance)

# S3 method for class 'Raster'
proximity_matrix(x, distance)

# S3 method for class 'SpatRaster'
proximity_matrix(x, distance)

# S3 method for class 'SpatialPolygons'
proximity_matrix(x, distance)

# S3 method for class 'SpatialLines'
proximity_matrix(x, distance)

# S3 method for class 'SpatialPoints'
proximity_matrix(x, distance)

# S3 method for class 'sf'
proximity_matrix(x, distance)

# Default S3 method
proximity_matrix(x, distance)
```

## Arguments

- x:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  object representing planning units.

- distance:

  `numeric` distance threshold. Planning units that are further apart
  from each other than this threshold are not treated as being within
  proximity of each other.

## Value

A
[`Matrix::dsCMatrix`](https://rdrr.io/pkg/Matrix/man/dsCMatrix-class.html)
symmetric sparse matrix object. Each row and column represents a
planning unit. Cells values indicate if the pair-wise distances between
different planning units are within the distance threshold or not (using
ones and zeros). To reduce computational burden, cells among the matrix
diagonal are set to zero. Furthermore, if the argument to `x` is a
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object, then cells with missing (`NA`) values are set to zero too.

## Details

Proximity calculations are performed using
[`sf::st_is_within_distance()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html).

## See also

Proximity matrix data might need rescaling to improve optimization
performance, see
[`rescale_matrix()`](https://prioritizr.net/reference/rescale_matrix.md)
to perform these calculations.

## Examples

``` r
# \dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_pu_polygons <- get_sim_pu_polygons()
sim_pu_lines <- get_sim_pu_lines()
sim_pu_points <- get_sim_pu_points()

# create proximity matrix using raster data
## crop raster to 9 cells to provide a small example
r <- terra::crop(sim_pu_raster, c(0, 0.3, 0, 0.3))

## make proximity matrix using a distance threshold of 2
cm_raster <- proximity_matrix(r, distance = 2)

# create proximity matrix using polygon data
## subset 9 polygons to provide a small example
ply <- sim_pu_polygons[c(1:3, 11:13, 20:22), ]

## make proximity matrix using a distance threshold of 2
cm_ply <- proximity_matrix(ply, distance = 2)

# create proximity matrix using line data
## subset 9 lines to provide a small example
lns <- sim_pu_lines[c(1:3, 11:13, 20:22), ]

## make proximity matrix
cm_lns <- proximity_matrix(lns, distance = 2)

## create proximity matrix using point data
## subset 9 points to provide a small example
pts <- sim_pu_points[c(1:3, 11:13, 20:22), ]

# make proximity matrix
cm_pts <- proximity_matrix(pts, distance = 2)

## plot raster and proximity matrix
plot(r, main = "raster", axes = FALSE)

Matrix::image(cm_raster, main = "proximity matrix")


## plot polygons and proximity matrix
plot(ply[, 1], main = "polygons", axes = FALSE)

Matrix::image(cm_ply, main = "proximity matrix")


## plot lines and proximity matrix
plot(lns[, 1], main = "lines", axes = FALSE)

Matrix::image(cm_lns, main = "proximity matrix")


## plot points and proximity matrix
plot(pts[, 1], main = "points", axes = FALSE)

Matrix::image(cm_pts, main = "proximity matrix")

# }
```
