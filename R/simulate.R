#' @include internal.R
NULL

#' Simulate data
#'
#' Simulate spatially auto-correlated data using Gaussian random fields.
#'
#' @param x [raster::raster()] raster object to use as a template.
#'
#' @param n `integer` number of layers to simulate.
#'   Defaults to 1.
#'
#' @param scale `numeric` parameter to control level of spatial
#'   auto-correlation.
#'   Defaults to 0.5.
#'
#' @param intensity `numeric` average value of simulated data.
#'   Defaults to 0.
#'
#' @param sd `numeric` standard deviation of simulated data.
#'   Defaults to 1.
#'
#' @param transform `function` transform values output from the simulation.
#'   Defaults to the [identity()] function such that values remain the same
#'   following transformation.
#'
#' @return [raster::stack()] object.
#'
#' @examples
#' \dontrun{
#' # create raster
#' r <- raster(ncol = 10, nrow = 10, xmn = 0, xmx = 1, ymn = 0, ymx = 1)
#' values(r) <- 1
#'
#' # simulate data using a Gaussian field
#' x <- simulate_data(r, n = 1, scale = 0.2)
#'
#' # plot simulated data
#' plot(x, main = "simulated data")
#' }
#' @export
simulate_data <- function(x, n = 1, scale = 0.5, intensity = 0, sd = 1,
                          transform = identity) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "Raster"),
    assertthat::is.number(n),
    is.finite(raster::cellStats(x, "max")[[1]]),
    assertthat::is.number(scale),
    assertthat::noNA(scale),
    assertthat::is.number(intensity),
    assertthat::noNA(intensity),
    assertthat::is.number(sd),
    assertthat::noNA(sd),
    is.function(transform),
    requireNamespace("fields", quietly = TRUE))

  # create object for simulation
  obj <- fields::circulantEmbeddingSetup(
    grid = list(
      x = seq(0, 5, length.out = raster::nrow(x)),
      y = seq(0, 5, length.out = raster::ncol(x))
    ),
    Covariance = "Exponential",
    aRange = scale
  )

  # generate populate rasters with values
  r <- raster::stack(lapply(seq_len(n), function(i) {
    ## populate with simulated values
    v <- c(t(fields::circulantEmbedding(obj)))
    v <- transform(v + stats::rnorm(length(v), mean = intensity, sd = sd))
    r <- raster::setValues(x[[1]], v[seq_len(raster::ncell(x))])
    ## apply mask for consistency
    r <- raster::mask(r, x[[1]])
    ## return result
    r
  }))

  # return result
  r
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
#' Specifically, it returns spatially auto-correlated data with integer values.
#'
#' @inheritParams simulate_data
#'
#' @param intensity `numeric` average value of simulated data.
#'   Defaults to 100.
#'
#' @param sd `numeric` standard deviation of simulated data.
#'   Defaults to 20.
#'
#' @param scale `numeric` strength of spatial auto-correlation.
#'   Defaults to 2.5.
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
simulate_cost <- function(x, n = 1, intensity = 100, sd = 20, scale = 2.5) {
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
