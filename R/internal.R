
#' @export
methods::setOldClass('Waiver')

#' Waiver
#' 
#' Create a \code{waiver} object. 
#' 
#' @details This object is used to represent that the user has not manually 
#' specified a setting, and so defaults should be used. By explictly 
#' using a \code{waiver()}, this means that \code{NULL} objects can be a 
#' valid setting. The use of a "waiver" object was inspired by the
#' \emph{ggplot2} package.
#' 
#' @return \code{object} of class \code{Waiver}.
#'
#' @noRd
waiver <- function() structure(NULL, class = "Waiver")

#' @export
methods::setOldClass('Id')

#' Identifier
#'
#' Generate a new unique identifier.
#'
#' @details Identifiers are made using the \code{\link[uuid]{UUIDgenerate}}.
#'
#' @return \code{Id} object.
#' 
#' @seealso \code{\link[uuid]{UUIDgenerate}}.
#'
#' @aliases Id
#'
#' @noRd
new_id <- function() {
  x <- uuid::UUIDgenerate()
  class(x) <- c('Id', class(x))
  x
}

#' Log-linear interpolation
#' 
#' Loglinearly interpolate values. 
#'
#' @param x \code{numeric} \emph{x} values to interpolate emph{y} values at.
#'
#' @param coordinate_one_x \code{numeric} value for lower \emph{x}-coordinate.
#'
#' @param coordinate_one_y \code{numeric} value for lower \emph{y}-coordinate.
#'
#' @param coordinate_two_x \code{numeric} value for upper \emph{x}-coordinate.
#'
#' @param coordinate_two_y \code{numeric} value for upper \emph{y}-coordinate.
#'
#' @details Values are log-linearly interpolated at the \emph{x}-coordinates 
#'   specified in \code{x} using the lower and upper coordnate arguments to 
#'   define the line. Values lesser or greater than these numbers are assigned
#'   the minimum and maximum \emph{y} coordinates.
#'
#' @return \code{numeric} values.
#'
#' @example
#' x <- seq(0, 1000)
#' y <- loglinear_interpolate(x, 200, 100, 900, 15)
#' plot(y~x)
#' points(x=c(200, 900), y=c(100, 15), pch=18, col='red', cex=2)
#'
#' @noRd
loglinear_interpolate <- function(x, coordinate_one_x, coordinate_one_y,
                                  coordinate_two_x, coordinate_two_y) {
  assertthat::assert_that(is.numeric(x), isTRUE(all(is.finite(x))),
                          assertthat::is.scalar(coordinate_one_x),
                          assertthat::is.scalar(coordinate_one_y),
                          assertthat::is.scalar(coordinate_two_x),
                          assertthat::is.scalar(coordinate_two_y),
                          coordinate_one_x < coordinate_two_x)
  out <- rep(NA_real_, length(x))
  out[x <= coordinate_one_x] <- coordinate_one_y
  out[x >= coordinate_two_x] <- coordinate_two_y
  between.pos <- which(is.na(out))
  out[between.pos] <- approx(x=log(c(coordinate_one_x, coordinate_two_x)),
                             y=c(coordinate_one_y, coordinate_two_y),
                             xout=log(x[between.pos]), method='linear')$y
  out
}

#' Check
#'
#' Check that the output from \code{\link[assert_that]{see_if}}
#' is valid.
#' 
#' @param x \code{character} or \code{object}
#' 
#' @details This object will return an error if the argument to \code{x}
#'   is \code{FALSE}, and for the error message, will show the error
#'   message attached to the object.
#' 
#' @return invisible \code{TRUE}.
#'
#' @noRd
check_that <- function(x) {
  if (!isTRUE(x))
    stop(attr(x, 'msg')[1])
  invisible(TRUE)
}

#' As triplet dataframe
#'
#' Convert a sparse matrix to a triplet \code{data.frame}.
#'
#' @param x \code{Matrix} object.
#'
#' @noRd
as_triplet_dataframe <- function(x) {
  data.frame(i=x@i, j=x@j, x=x@x)
}


#' Parallel extract
#'
#' Extract data from a \code{\link[raster]{Raster-class}} object using 
#' a \code{\link[sp]{Spatial-class}} object using parallel processing.
#'
#' @param x \code{\link[raster]{Raster-class}} object.
#'
#' @param y \code{\link[sp]{Spatial-class}} object.
#'
#' @param fun \code{function} to compute values.
#'
#' @param ... additional arguments passed to \code{\link[raster]{extract}}.
#'
#' @details This function is essentially a wrapper for 
#'   \code{\link[raster]{extract}}. To enable parallel processing,
#'   use the \code{\link{set_number_of_threads}} function.
#'
#' @return \code{data.frame}, \code{matrix}, or \code{list} object 
#'   depending on the arguments.
#'
#' @noRd
parallelized_extract <- function(x, y, fun=mean, ...) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, 'Raster'), inherits(y, 'Spatial'),
      inherits(fun, 'function'), raster::compareCRS(x@crs, y@proj4string),
      rgeos::gIntersects(as(raster::extent(x[[1]]), 'SpatialPolygons'),
        as(raster::extent(y), 'SpatialPolygons')), is.parallel())
  # data processing
  args <- list(...)
  parallel::clusterExport(.pkgenv$cluster, c('x', 'y', 'fun', 'args'),
      envir=environment())
  m <- plyr::llply(distribute_load(length(y)), .parallel=TRUE,
    function(i) {
      return(do.call(raster::extract, 
        append(list(x=x, y=y[i,], fun=fun), args)))
    })
  parallel::clusterEvalQ(.pkgenv$cluster, 
    {rm('x', 'y', 'fun', 'args')})
  # combine parallel runs
  if (inherits(m[[1]], c('matrix', 'data.frame'))) {
    m <- do.call(rbind, m)
  } else {
    m <- do.call(append, m)
  }
  # return result
  return(m)
}

#' Velox extract
#'
#' This function is a wrapper for \code{\link{velox}{VeloxRaster-extract}}.
#' 
#' @param x \code{\link[raster]{Raster-class}} object.
#'
#' @param y \code{\link[sp]{Spatial-class}} object.
#'
#' @param fun \code{function} to compute values.
#'
#' @param df \code{logical} should results be returned as a \code{data.frame}?
#'
#' @param ... not used.
#'
#' @return \code{matrix} or \code{data.frame} depending on arguments.
#'
#' @noRd
velox_extract <- function(x, y, fun, df=FALSE, ...) {
  assertthat::assert_that(inherits(x, 'Raster'), inherits(y, 'SpatialPolygons'),
    inherits(fun, 'function'), assertthat::is.flag(df))
  m <- velox::velox(x)$extract(y, fun)
  if (df) {
    m <- cbind(data.frame(ID=seq_along(length(y))), as.data.frame(m))
    names(m) <- c('ID', names(x))
  }
  return(m)
}
