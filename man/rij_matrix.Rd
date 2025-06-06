% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rij_matrix.R
\name{rij_matrix}
\alias{rij_matrix}
\alias{rij_matrix,Raster,Raster-method}
\alias{rij_matrix,Spatial,Raster-method}
\alias{rij_matrix,sf,Raster-method}
\alias{rij_matrix,SpatRaster,SpatRaster-method}
\alias{rij_matrix,sf,SpatRaster-method}
\title{Feature by planning unit matrix}
\usage{
rij_matrix(x, y, ...)

\S4method{rij_matrix}{SpatRaster,SpatRaster}(x, y, memory, idx, ...)

\S4method{rij_matrix}{sf,SpatRaster}(x, y, fun, memory, idx, ...)

\S4method{rij_matrix}{Raster,Raster}(x, y, memory, idx, ...)

\S4method{rij_matrix}{sf,Raster}(x, y, fun, memory, idx, ...)

\S4method{rij_matrix}{Spatial,Raster}(x, y, fun, memory, idx, ...)
}
\arguments{
\item{x}{\code{\link[terra:rast]{terra::rast()}} or \code{\link[sf:sf]{sf::sf()}} object representing planning units.}

\item{y}{\code{\link[terra:rast]{terra::rast()}}  object.}

\item{...}{not used.}

\item{memory}{\code{logical} should calculations be performed using a method
that prioritizes reduced memory consumption over speed?
This is useful when processing particularly large raster datasets.
If \code{TRUE}, then calculations are performed by processing each raster layer
in \code{y} in a sequential manner.
If \code{FALSE}, then calculations are performed by processing all raster layers
in \code{y} together.
If \code{NA}, then the memory requirements will be estimated and, if required,
processing will be performed using the method that reduces memory
consumption.
Defaults to \code{NA}.}

\item{idx}{\code{integer} vector containing planning unit
indices. Defaults to \code{NULL} such that the indices are computed
automatically based on \code{x}.}

\item{fun}{\code{character} for summarizing values inside each planning unit.
This parameter is only used when the argument to \code{x} is a
\code{\link[sf:sf]{sf::sf()}} object.
Defaults to \code{"sum"}.}
}
\value{
A \code{\link[Matrix:dgCMatrix-class]{Matrix::dgCMatrix}} sparse matrix object.
The sparse matrix represents the spatial intersection between the
planning units and the features. Rows correspond to features,
and columns correspond to planning units. Values correspond to the amount
(or presence/absence) of the feature in the planning unit. For example,
the amount of the third species in the second planning unit would be
stored in the third column and second row.
}
\description{
Generate a matrix showing the amount of each feature in each planning
unit (also known as an \emph{rij} matrix).
}
\details{
Generally, processing \code{\link[sf:sf]{sf::st_sf()}} data takes much longer to process than
\code{\link[terra:rast]{terra::rast()}} data.
As such, it is recommended to use \code{\link[terra:rast]{terra::rast()}} data for planning units
where possible.
The performance of this function for large \code{\link[terra:rast]{terra::rast()}} datasets
can be improved by increasing the GDAL cache size.
The default cache size is 25 MB.
For example, the following code can be used to set the cache size to 4 GB.

\if{html}{\out{<div class="sourceCode">}}\preformatted{terra::gdalCache(size = 4000)
}\if{html}{\out{</div>}}
}
\examples{
\dontrun{
# load data
sim_pu_raster <- get_sim_pu_raster()
sim_pu_polygons <- get_sim_pu_polygons()
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_features <- get_sim_features()

# create rij matrix using raster layer planning units
rij_raster <- rij_matrix(sim_pu_raster, sim_features)
print(rij_raster)

# create rij matrix using polygon planning units
rij_polygons <- rij_matrix(sim_pu_polygons, sim_features)
print(rij_polygons)

# create rij matrix using raster planning units with multiple zones
rij_zones_raster <- rij_matrix(sim_zones_pu_raster, sim_features)
print(rij_zones_raster)
}
}
