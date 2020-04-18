#' @include internal.R fast_extract.R
NULL

#' Feature by planning unit matrix
#'
#' Generate a matrix showing the amount of each feature in each planning
#' unit (also known as an \emph{rij} matrix).
#'
#' @param x \code{\link[raster]{Raster-class}},
#'   \code{\link[sp]{Spatial-class}}, or \code{\link[sf]{sf}} object
#'   representing the planning units.
#'
#' @param y \code{\link[raster]{Raster-class}} object representing the
#'   features.
#'
#' @param fun \code{character} for summarizing values inside each planning unit.
#'   This parameter is only used when the argument to \code{x} is a
#'   \code{\link[sp]{Spatial-class}} or \code{\link[sf]{sf}} object.
#'   Defaults to \code{"sum"}.
#'
#' @param ... not used.
#'
#' @details
#'   Generally, processing vector (i.e. \code{\link[sp]{Spatial-class}} or
#'   \code{\link[sf]{sf}}) data takes much
#'   longer to process then \code{\link[raster]{Raster-class}} data,
#'   so it is recommended to use \code{\link[raster]{Raster-class}} data
#'   for planning units where possible.
#'
#' @return \code{\link[Matrix]{dgCMatrix-class}} sparse matrix object.
#'   The sparse matrix represents the spatial intersection between the
#'   planning units and the features. Rows correspond to planning units,
#'   and columns correspond to features. Values correspond to the amount
#'   of the feature in the planning unit. For example, the amount of the
#'   third species in the second planning unit would be stored in the
#'   third column and second row.
#'
#' @name rij_matrix
#'
#' @exportMethod rij_matrix
#'
#' @aliases rij_matrix,Raster,Raster-method rij_matrix,Spatial,Raster-method rij_matrix,sf,Raster-method
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_polygons, sim_pu_sf, sim_pu_zones_stack)
#'
#' # create rij matrix using raster layer planning units
#' rij_raster <- rij_matrix(sim_pu_raster, sim_features)
#' print(rij_raster)
#'
#' # create rij matrix using polygon (Spatial) planning units
#' rij_polygons <- rij_matrix(sim_pu_polygons, sim_features)
#' print(rij_polygons)
#'
#' # create rij matrix using polygon (sf) planning units
#' rij_sf <- rij_matrix(sim_pu_sf, sim_features)
#' print(rij_sf)
#'
#' # create rij matrix using raster stack planning units
#' rij_zones_raster <- rij_matrix(sim_pu_zones_stack, sim_features)
#' print(rij_zones_raster)
#' @export
methods::setGeneric("rij_matrix",
                    signature = methods::signature("x", "y"),
                    function(x, y, ...) standardGeneric("rij_matrix"))

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{Raster,Raster}(x, y, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "Raster", y = "Raster"),
  function(x, y, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "Raster"), inherits(y, "Raster"),
      raster::nlayers(x) > 0, raster::nlayers(y) > 0,
      is_comparable_raster(x, y),
      no_extra_arguments(...))
    # set included cells
    if (raster::nlayers(x) == 1) {
      included <- raster::Which(!is.na(x), cells = TRUE)
    } else {
      included <- raster::Which(max(!is.na(x)) > 0, cells = TRUE)
    }
    # data processing
    if (raster::canProcessInMemory(x, n = raster::nlayers(y) *
                                          raster::nlayers(x) + 1)) {
      # if the all the features can be fit into memory then processes
      # them all in memory
      m <- y[]
      if (!is.matrix(m)) {
        m <- matrix(m[included], nrow = 1)
        m[is.na(m)] <- 0
        m <- methods::as(m, "dgCMatrix")
      } else {
        m <- m[included, , drop = FALSE]
        m[is.na(m)] <- 0
        m <- Matrix::t(methods::as(m, "dgCMatrix"))
      }
    } else {
      # othewise, process each feature separately
        m <- lapply(seq_len(raster::nlayers(y)), .parallel = FALSE,
          function(i) {
            m <- matrix(y[[i]][][included], nrow = 1)
            m[is.na(m)] <- 0
            m <- methods::as(m, "dgCMatrix")
          })
      m <- Reduce(rbind, m[-1], m[[1]])
    }
    # return result
    return(m)
})

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{Spatial,Raster}(x, y, fun, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "Spatial", y = "Raster"),
  function(x, y, fun = "sum", ...) {
    rij_matrix(sf::st_as_sf(x), y, fun = fun, ...)
})

#' @name rij_matrix
#' @usage \S4method{rij_matrix}{sf,Raster}(x, y, fun, ...)
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "sf", y = "Raster"),
  function(x, y, fun = "sum", ...) {
    m <- fast_extract(x = y, y = x, fun = fun)
    m[is.na(m[])] <- 0
    m <- methods::as(m, "dgCMatrix")
    colnames(m) <- names(y)
    return(Matrix::t(m))
})
