% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boundary_matrix.R
\name{boundary_matrix}
\alias{boundary_matrix}
\alias{boundary_matrix.Raster}
\alias{boundary_matrix.SpatRaster}
\alias{boundary_matrix.SpatialPolygons}
\alias{boundary_matrix.SpatialLines}
\alias{boundary_matrix.SpatialPoints}
\alias{boundary_matrix.sf}
\alias{boundary_matrix.default}
\title{Boundary matrix}
\usage{
boundary_matrix(x, ...)

\method{boundary_matrix}{Raster}(x, ...)

\method{boundary_matrix}{SpatRaster}(x, ...)

\method{boundary_matrix}{SpatialPolygons}(x, ...)

\method{boundary_matrix}{SpatialLines}(x, ...)

\method{boundary_matrix}{SpatialPoints}(x, ...)

\method{boundary_matrix}{sf}(x, ...)

\method{boundary_matrix}{default}(x, ...)
}
\arguments{
\item{x}{\code{\link[terra:rast]{terra::rast()}} or \code{\link[sf:sf]{sf::sf()}} object representing planning units.}

\item{...}{not used.}
}
\value{
A \code{\link[Matrix:dsCMatrix-class]{Matrix::dsCMatrix}} symmetric sparse matrix object.
Each row and column represents a planning unit.
Cell values indicate the shared boundary length between different pairs
of planning units. Values along the matrix diagonal indicate the
total perimeter associated with each planning unit.
}
\description{
Generate a matrix describing the amount of shared boundary length
between different planning units, and the total amount of boundary length
for each planning unit.
}
\details{
This function assumes the data are in a coordinate
system where Euclidean distances accurately describe the proximity
between two points on the earth. Thus spatial data in a
longitude/latitude coordinate system (i.e.,
\href{https://spatialreference.org/ref/epsg/4326/}{WGS84})
should be reprojected to another coordinate system before using this
function. Note that for \code{\link[terra:rast]{terra::rast()}} objects
boundaries are missing for cells that have  missing (\code{NA}) values in all
cells.
}
\section{Notes}{

In earlier versions, this function had an extra \code{str_tree} parameter
that could be used to leverage STR query trees to speed up processing
for planning units in vector format.
Although this functionality improved performance, it was not
enabled by default because the underlying function
(i.e., \code{rgeos:gUnarySTRtreeQuery()}) was documented as experimental.
The \code{boundary_matrix()} function has since been updated so that it will
use STR query trees to speed up processing for planning units in vector
format (using \code{\link[terra:sharedPaths]{terra::sharedPaths()}}).

Also, note that in previous versions, cell values along the matrix
diagonal indicated the perimeter associated with planning units
that did not contain any neighbors. This has now changed such
that values along the diagonal now correspond to the total
perimeter associated with each planning unit.
}

\examples{
\dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_pu_polygons <- get_sim_pu_polygons()

# subset data to reduce processing time
r <- terra::crop(sim_pu_raster, c(0, 0.3, 0, 0.3))
ply <- sim_pu_polygons[c(1:3, 11:13, 20:22), ]

# create boundary matrix using raster data
bm_raster <- boundary_matrix(r)

# create boundary matrix using polygon data
bm_ply <- boundary_matrix(ply)

# plot raster data
plot(r, main = "raster", axes = FALSE)

# plot boundary matrix
# here each row and column corresponds to a different planning unit
Matrix::image(bm_raster, main = "boundary matrix")

# plot polygon data
plot(ply[, 1], main = "polygons", axes = FALSE)

# plot boundary matrix
# here each row and column corresponds to a different planning unit
Matrix::image(bm_ply, main = "boundary matrix")
}
}
\seealso{
Boundary matrix data might need rescaling to improve optimization
performance, see \code{\link[=rescale_matrix]{rescale_matrix()}} to perform these calculations.
}
