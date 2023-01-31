#' @include internal.R
NULL

#' Simulate data
#'
#' Simulate spatially auto-correlated data using Gaussian random fields.
#'
#' @param x [terra::rast()] object to use as a template.
#'
#' @param n `integer` number of layers to simulate.
#'   Defaults to 1.
#'
#' @param scale `numeric` parameter to control level of spatial
#'   auto-correlation in the simulated data.
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
#' @family simulations
#'
#' @return A [terra::rast()] object.
#'
#' @examples
#' \dontrun{
#' # create raster
#' r <- terra::rast(
#'   ncols = 10, nrows = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 1
#' )
#'
#' # simulate data using a Gaussian field
#' x <- simulate_data(r, n = 1, scale = 0.2)
#'
#' # plot simulated data
#' plot(x, main = "simulated data")
#' }
#' @export
simulate_data <- function(x, n, scale, intensity, sd, transform) {
  rlang::check_required(x)
  assert(is_inherits(x, c("SpatRaster", "Raster")))
  UseMethod("simulate_data")
}

#' @rdname simulate_data
#' @method simulate_data Raster
#' @export
simulate_data.Raster <- function(x, n = 1, scale = 0.5, intensity = 0,
                                 sd = 1, transform = identity) {
  .Deprecated(msg = raster_pkg_deprecation_notice)
  raster::stack(
    simulate_data.SpatRaster(
      x = terra::rast(x),
      n = n, scale = scale,
      intensity = intensity,
      sd = sd,
      transform = transform
    )
  )
}

#' @rdname simulate_data
#' @method simulate_data SpatRaster
#' @export
simulate_data.SpatRaster <- function(x, n = 1, scale = 0.5, intensity = 0,
                                     sd = 1, transform = identity) {
  # assert valid arguments
  assert(
    inherits(x, "SpatRaster"),
    assertthat::is.count(n),
    terra::global(x, "notNA")[[1]][[1]] > 0,
    assertthat::is.number(scale),
    assertthat::noNA(scale),
    assertthat::is.number(intensity),
    assertthat::noNA(intensity),
    assertthat::is.number(sd),
    assertthat::noNA(sd),
    is.function(transform),
    is_installed("fields")
  )

  # create object for simulation
  obj <- fields::circulantEmbeddingSetup(
    grid = list(
      x = seq(0, 5, length.out = terra::nrow(x)),
      y = seq(0, 5, length.out = terra::ncol(x))
    ),
    Covariance = "Exponential",
    aRange = scale
  )

  # generate populate rasters with values
  r <- terra::rast(lapply(seq_len(n), function(i) {
    ## populate with simulated values
    v <- c(t(fields::circulantEmbedding(obj)))
    v <- transform(v + stats::rnorm(length(v), mean = intensity, sd = sd))
    r <- terra::setValues(x[[1]], v[seq_len(terra::ncell(x))])
    ## apply mask for consistency
    r <- terra::mask(r, x[[1]])
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
#' @inherit simulate_data return
#'
#' @family simulations
#'
#' @examples
#' \dontrun{
#' # create raster
#' r <- terra::rast(
#'   ncols = 10, nrows = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 1
#' )
#'
#' # simulate data for 4 species
#' spp <- simulate_species(r, 4)
#'
#' # plot simulated species
#' plot(spp, main = "simulated species distributions")
#' }
#'
#' @export
simulate_species <- function(x, n, scale) {
  rlang::check_required(x)
  assert(is_inherits(x, c("SpatRaster", "Raster")))
  UseMethod("simulate_species")
}

#' @rdname simulate_species
#' @method simulate_species Raster
#' @export
simulate_species.Raster <- function(x, n = 1, scale = 0.5) {
  .Deprecated(msg = raster_pkg_deprecation_notice)
  raster::stack(
    simulate_species.SpatRaster(
      x = terra::rast(x),
      n = n,
      scale = scale
    )
  )
}

#' @rdname simulate_species
#' @method simulate_species SpatRaster
#' @export
simulate_species.SpatRaster <- function(x, n = 1, scale = 0.5) {
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
#' @param scale `numeric` parameter to control level of spatial
#'   auto-correlation in the simulated data.
#'   Defaults to 2.5.
#'
#' @family simulations
#'
#' @inherit simulate_data return
#'
#' @examples
#' \dontrun{
#' # create raster
#' r <- terra::rast(
#'   ncols = 10, nrows = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 1
#' )
#'
#' # simulate data
#' cost <- simulate_cost(r)
#'
#' # plot simulated species
#' plot(cost, main = "simulated cost data")
#' }
#'
#' @export
simulate_cost <- function(x, n, intensity, sd, scale) {
  rlang::check_required(x)
  assert(is_inherits(x, c("SpatRaster", "Raster")))
  UseMethod("simulate_cost")
}

#' @rdname simulate_cost
#' @method simulate_cost Raster
#' @export
simulate_cost.Raster <- function(x, n = 1, intensity = 100,
                                 sd = 20, scale = 2.5) {
  .Deprecated(msg = raster_pkg_deprecation_notice)
  raster::stack(
    simulate_cost.SpatRaster(
      x = terra::rast(x),
      n = n,
      intensity = intensity,
      sd = sd,
      scale = scale
    )
  )
}

#' @rdname simulate_cost
#' @method simulate_cost SpatRaster
#' @export
simulate_cost.SpatRaster <- function(x, n = 1, intensity = 100,
                                     sd = 20, scale = 2.5) {
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
