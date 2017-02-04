#' @include internal.R
NULL

#' Simulate data
#'
#' Simulate spatially auto-correlated data using random fields.
#'
#' @param x \code{\link[raster]{RasterLayer-class}} object to use as
#    a template.
#'
#' @param n \code{integer} number of species to simulate.
#'
#' @param model \code{\link[RandomFields]{RP}} model object
#'   to use for simulating data.
#'
#' @param transform \code{function} to transform values output
#'   from the random fields simulation.
#'
#' @param ... additional arguments passed to
#'   \code{\link[RandomFields]{RFsimulate}}.
#'
#' @return \code{\link[raster]{RasterStack-class}} object with a
#'   layer for each species.
#' 
#' @seealso \code{\link[RandomFields]{RFsimulate}},
#'   \code{\link{simulate_cost}}, \code{\link{simulate_species}}.
#'
#' @export
simulate_data <- function(x, n, model, transform=identity, ...) {
  # assert valid arguments
  if (!'RandomFields' %in% rownames(installed.packages()))
    stop('the "RandomFields" package needs to be installed to simulate data')
  assertthat::assert_that(
    inherits(x, 'RasterLayer'),
    assertthat::is.number(n),
    is.finite(raster::cellStats(x, 'max')),
    inherits(model, 'RMmodel'),
    inherits(transform, 'function'))
  # generate values for rasters
  coords <- as(x, 'SpatialPoints')@coords
  mtx <- RandomFields::RFsimulate(model=model, x=coords[,1], y=coords[,2],
                                  n=n, spConform=FALSE, ...)
  # convert to matrix if not a matrix
  if (!inherits(mtx, 'matrix'))
    mtx <- matrix(mtx, ncol=1)
  # generate populate rasters with values
  stk <- raster::stack(lapply(seq_len(ncol(mtx)), function(i) {
    r <- x
    r[raster::Which(!is.na(r))] <- transform(mtx[,i])
    r
  }))
  # return raster stack with simulated distributions
  return(stk)
}

#' Simulate species habitat suitabilities
#'
#' Generates a random set of species using random field models. By default,
#' the output will contain values between zero and one.
#' 
#' @inheritParams simulate_data
#'
#' @return \code{\link[raster]{RasterStack-class}} object.
#'
#' @seealso \code{\link{simulate_data}}.
#'
#' @export
simulate_species <- function(x, n=1, model=RandomFields::RMgauss(),
                             transform=plogis, ...) {
  simulate_data(x=x, n=n, model=model, transform=transform, ...)
}

#' Simulate cost data
#'
#' This function generates cost layers using random field models. By default,
#' it returns spatially autocorrelated integer values.
#'
#' @inheritParams simulate_data
#'
#' @return \code{\link[raster]{RasterStack-class}} object.
#'
#' @seealso \code{\link{simulate_data}}.
#'
#' @export
simulate_cost <- function(x, n=1, 
    model=RandomFields::RPpoisson(
            RandomFields::RMtruncsupport(
              radius=raster::xres(x)*10,
              RandomFields::RMgauss())), 
    transform=identity, ...) {
  simulate_data(x, n=n, model=model, transform=transform, ...)
}

