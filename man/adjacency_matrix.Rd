% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjacency_matrix.R
\name{adjacency_matrix}
\alias{adjacency_matrix}
\alias{adjacency_matrix.Raster}
\alias{adjacency_matrix.SpatRaster}
\alias{adjacency_matrix.SpatialPolygons}
\alias{adjacency_matrix.SpatialLines}
\alias{adjacency_matrix.SpatialPoints}
\alias{adjacency_matrix.sf}
\alias{adjacency_matrix.default}
\title{Adjacency matrix}
\usage{
adjacency_matrix(x, ...)

\method{adjacency_matrix}{Raster}(x, directions = 4, ...)

\method{adjacency_matrix}{SpatRaster}(x, directions = 4, ...)

\method{adjacency_matrix}{SpatialPolygons}(x, ...)

\method{adjacency_matrix}{SpatialLines}(x, ...)

\method{adjacency_matrix}{SpatialPoints}(x, ...)

\method{adjacency_matrix}{sf}(x, ...)

\method{adjacency_matrix}{default}(x, ...)
}
\arguments{
\item{x}{\code{\link[terra:rast]{terra::rast()}} or \code{\link[sf:sf]{sf::sf()}} object representing planning units.}

\item{...}{not used.}

\item{directions}{\code{integer} If \code{x} is a
\code{\link[terra:rast]{terra::rast()}} object, the number of directions
in which cells should be considered adjacent: 4 (rook's case), 8 (queen's
case), 16 (knight and one-cell queen moves), or "bishop" to for cells
with one-cell diagonal moves.}
}
\value{
A \code{\link[Matrix:dsCMatrix-class]{Matrix::dsCMatrix}} sparse symmetric matrix.
Each row and column represents a planning unit.
Cells values indicate if different planning units are
adjacent to each other or not (using ones and zeros).
To reduce computational burden, cells among the matrix diagonal are
set to zero. Furthermore, if the argument to \code{x} is a
\code{\link[terra:rast]{terra::rast()}} object, then cells with \code{NA} values are set to
zero too.
}
\description{
Create a matrix showing which planning units are spatially adjacent to
each other.
}
\details{
Spatial processing is completed using
\code{\link[sf:geos_binary_pred]{sf::st_intersects()}} for \code{\link[sf:sf]{sf::sf()}} objects,
and \code{\link[terra:adjacent]{terra::adjacent()}} for \code{\link[terra:rast]{terra::rast()}} objects.
Note that spatially overlapping planning units are considered
adjacent.
}
\section{Notes}{

In earlier versions (< 5.0.0), this function was named as the
\code{connected_matrix} function. It has been renamed to be consistent
with other spatial association matrix functions.
}

\examples{
\dontrun{
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

}
}
