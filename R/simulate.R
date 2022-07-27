#' @include internal.R
NULL

#' Simulate data
#'
#' Simulate spatially auto-correlated data.
#'
#' @details
#' Data are simulated based on a Gaussian random field.
#'
#' @param x [`RasterLayer-class`] object to use as
#'    a template.
#'
#' @param n `integer` number of layers to simulate.
#'   Defaults to 1.
#'
#' @param intensity `numeric` average value of simulated data.
#'   Defaults to 0.
#'
#' @param sd `numeric` standard deviation of simulated data.
#'   Defaults to 1.
#'
#' @param scale `numeric` strength of spatial auto-correlation.
#'   Defaults to 0.5.
#'
#' @param transform `function` transformation function.
#'   Defaults to [identity()], such that values remain unchanged follow
#    transformation.
#'
#' @return [`RasterStack-class`] object.
#'
#' @seealso [simulate_cost()], [simulate_species()].
#'
#' @examples
#' \dontrun{
#' # create raster
#' r <- raster(ncol = 10, nrow = 10, xmn = 0, xmx = 1, ymn = 0, ymx = 1)
#' values(r) <- 1
#'
#' # simulate data using a Gaussian field
#' d <- simulate_data(r, n = 1)
#'
#' # plot simulated data
#' plot(d, main = "simulated data")
#' }
#' @export
simulate_data <- function(x, n = 1, intensity = 0, sd = 1, scale = 0.5,
                          transform = identity) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "RasterLayer"),
    is.finite(raster::cellStats(x, "max", na.rm = TRUE)),
    assertthat::is.number(n),
    assertthat::noNA(n),
    assertthat::is.number(intensity),
    assertthat::noNA(intensity),
    assertthat::is.number(scale),
    assertthat::noNA(scale),
    inherits(transform, "function")
  )
  # generate values for rasters
  coords <- methods::as(x, "SpatialPoints")@coords
  # main processing
  mu <- rep(intensity, nrow(coords))
  p <- nrow(coords)
  chol_d <- chol(exp(-scale * as.matrix(stats::dist(coords))))
  mtx <- t(
    matrix(stats::rnorm(n = n * p, sd = sd), ncol = p) %*%
    chol_d + rep(mu, rep(n, p))
  )
  # ensure matrix output
  if (!is.matrix(mtx)) {
    mtx <- matrix(mtx, ncol = 1)
  }
  # generate populate rasters with values
  stk <- raster::stack(lapply(seq_len(ncol(mtx)), function(i) {
    r <- x[[1]]
    r[raster::Which(!is.na(r))] <- transform(mtx[, i])
    r
  }))
  # return raster stack with simulated data
  stk
}

#' Simulate species habitat suitability data
#'
#' Generates simulated species data using Gaussian random fields.
#' Specifically, it outputs spatially auto-correlated raster data with
#' values between zero and one.
#'
#' @inheritParams simulate_data
#'
#' @return [`RasterStack-class`] object.
#'
#' @seealso [simulate_data()].
#'
#' @examples
#' \dontrun{
#' # create raster
#' r <- raster(ncol = 10, nrow = 10, xmn = 0, xmx = 1, ymn = 0, ymx = 1)
#' values(r) <- 1
#'
#' # simulate data for 4 species
#' spp <- simulate_species(r, 4)
#'
#' # plot simulated species
#' plot(spp, main = "simulated species distributions")
#' }
#'
#' @export
simulate_species <- function(x, n = 1, scale = 0.5) {
  simulate_data(
    x = x,
    n = n,
    intensity = 0,
    sd = 1.5,
    scale = scale,
    transform = stats::plogis
  )
}

#' Simulate cost data
#'
#' Generates simulated cost data using Gaussian random fields.
#' Specifically, t returns spatially auto-correlated data with integer values.
#'
#' @inheritParams simulate_data
#'
#' @param intensity `numeric` average value of simulated data.
#'   Defaults to 100.
#'
#' @param sd `numeric` standard deviation of simulated data.
#'   Defaults to 60.
#'
#' @param scale `numeric` strength of spatial auto-correlation.
#'   Defaults to 60.
#'
#' @return [`RasterStack-class`] object.
#'
#' @seealso [simulate_data()].
#'
#' @examples
#' \dontrun{
#' # create raster
#' r <- raster(ncol = 10, nrow = 10, xmn = 0, xmx = 1, ymn = 0, ymx = 1)
#' values(r) <- 1
#'
#' # simulate data
#' cost <- simulate_cost(r)
#'
#' # plot simulated species
#' plot(cost, main = "simulated cost data")
#' }
#'
#' @export
simulate_cost <- function(x, n = 1, intensity = 100, sd = 60, scale = 60) {
  simulate_data(
    x,
    n = n,
    intensity = intensity,
    sd = sd,
    scale = scale,
    transform = function(x) {
      x[x < 0] <- 0
      x + 1
    }
  )
}
