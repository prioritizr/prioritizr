#' @importClassesFrom tibble tbl_df
#' @export

#' @export
if (!methods::isClass("sf"))
  methods::setOldClass("sf")
NULL

#' @export
if (!methods::isClass("SpatialPolygonsDataFrame"))
  methods::setOldClass("SpatialPolygonsDataFrame")
NULL

#' @export
if (!methods::isClass("SpatialLinesDataFrame"))
  methods::setOldClass("SpatialLinesDataFrame")
NULL

#' @export
if (!methods::isClass("SpatialPointsDataFrame"))
  methods::setOldClass("SpatialPointsDataFrame")
NULL

#' @export
if (!methods::isClass("Raster"))
  methods::setOldClass("Raster")
NULL
