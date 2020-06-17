#' @include internal.R Penalty-proto.R marxan_boundary_data_to_matrix.R
NULL

#' Add linear penalties
#'
#' Add penalties to a conservation planning \code{\link{problem}} to penalize
#' solutions that select planning units according to a specific metric
#' (e.g. anthropogenic impact).
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param penalty \code{numeric} penalty value that is used to scale the
#'   importance avoiding planning units with high \code{data} values.
#'   Higher \code{penalty} value can be used to obtain solutions that
#'   are strongly averse to selecting places with high \code{data}
#'   values, and smaller \code{penalty} values can be used to obtain solutions
#'   that avoid places with high \code{data} values. Note that negative
#'   \code{penalty} values can be used to obtain solutions that prefer places
#'   with high \code{data} values. Additionally, when adding these
#'   penalties to problems with multiple zones, the argument to \code{penalty}
#'   must have a value for each zone.

#' @param data \code{character}, \code{numeric},
#'   \code{\link[raster]{Raster-class}}, \code{matrix}, or \code{Matrix} object
#'   containing the data used to penalize solutions. Planning units that are
#'   associated with higher data values are penalized more strongly
#'   in the solution. See the Details section for more information.
#'
#' @details This function penalizes solutions that have higher values according
#'   to a specific metric. The argument to \code{data} can be specified in
#'   several different ways:
#'
#'   \describe{
#'
#'   \item{\code{character}}{field (column) name(s) that contain the data for
#'     penalizing planning units. This type of argument is only
#'     compatible if the planning units in the argument to \code{x} are a
#'     \code{\link[sp]{Spatial-class}}, \code{\link[sf]{sf}}, or
#'     \code{data.frame} object. The fields (columns) must have \code{numeric}
#'     values, and must not contain any missing (\code{NA}) values.
#'     For problems involving multiple zones, the argument to \code{data} must
#'     contain a field name for each zone.}
#'
#'   \item{\code{numeric}}{\code{vector} containing the data for penalizing
#'     each planning unit. These values must not contain any missing
#'     (\code{NA}) values. Note that this type of argument is only available
#'     for planning units that contain a single zone.}
#'
#'   \item{\code{\link[raster]{Raster-class}}}{containing the data for
#'     penalizing planning units. This type of argument is only
#'     compatible if the planning units in the argument to \code{x} are
#'     \code{\link[sp]{Spatial-class}}, \code{\link[sf]{sf}}, or
#'     or \code{\link[raster]{Raster-class}} (i.e. they are in a spatially
#'     referenced format).
#'     If the planning unit data are a \code{\link[sp]{Spatial-class}} or
#'     \code{\link[sf]{sf}} object, then the
#'     penalty \code{data} are calculated by overlaying the planning units with
#'     the argument to \code{data} and calculating the sum of the values.
#'     If the planning unit data are in the
#'     \code{\link[raster]{Raster-class}} then the penalty \code{data} are
#'     calculated by extracting the cell values (note that the
#'     planning unit data and the argument to code{data} must have exactly
#'     the same dimensionality, extent, and missingness).
#'     For problems involving multiple zones, the argument to \code{data} must
#'     contain a layer for each zone.
#'     If a planning unit only overlaps with missing \code{NA} values
#'     or doesn't overlap with any pixels in the argument to \code{data},
#'     then an error will be thrown.}
#'
#'   \item{\code{matrix}, \code{Matrix}}{where columns correspond to
#'     different planning units and rows correspond to different zones.
#'     These values must not contain any missing (\code{NA}) values.}
#'
#'   }
#'
#'  The linear penalties are calculated using the following equations.
#'  Let \eqn{I} denote the set of planning units
#'  (indexed by \eqn{i}), \eqn{Z} the set of management zones (indexed by
#'  \eqn{z}), and \eqn{X_{iz}}{Xiz} the decision variable for allocating
#'  planning unit \eqn{i} to zone \eqn{z} (e.g. with binary
#'  values one indicating if planning unit is allocated or not). Also, let
#'  \eqn{P_z} represent the penalty scaling value for zones
#'  \eqn{z \in Z}{z in Z} (argument to \code{penalty}), and
#'  \eqn{D_{iz}}{Diz} the penalty data for allocating planning unit
#'  \eqn{i \in I}{i in I} to zones \eqn{z \in Z}{z in Z} (argument to
#'  \code{data} if supplied as a matrix object). The penalties are calculated
#'
#'  \deqn{
#'  \sum_{i}^{I} \sum_{z}^{Z} -P_z \times D_{iz} \times X_{iz}
#'  }{
#'  sum_i^I sum_z^Z (-Pz * Diz * Xiz)
#'  }
#'
#'  Note that when the problem objective is to maximize some measure of
#'  benefit and not minimize some measure of cost, the term \eqn{P_z} is
#'  replaced with \eqn{-P_z}.
#'
#' @inherit add_linear_penalties return seealso
#'
#' @examples
#' # TODO
#'
#' @name add_linear_penalties
#'
#' @exportMethod add_linear_penalties
#'
#' @aliases add_linear_penalties,ConservationProblem,ANY,Matrix-method add_linear_penalties,ConservationProblem,ANY,matrix-method add_linear_penalties,ConservationProblem,ANY,dgCMatrix-method add_linear_penalties,ConservationProblem,ANY,character-method add_linear_penalties,ConservationProblem,ANY,numeric-method add_linear_penalties,ConservationProblem,ANY,Raster-method
NULL

#' @export
methods::setGeneric("add_linear_penalties",
  signature = methods::signature("x", "penalty", "data"),
  function(x, penalty, data)
    standardGeneric("add_linear_penalties"))

#' @name add_linear_penalties
#' @usage \S4method{add_linear_penalties}{ConservationProblem,ANY,character}(x, penalty, data)
#' @rdname add_linear_penalties
methods::setMethod("add_linear_penalties",
  methods::signature("ConservationProblem", "ANY", "character"),
  function(x, penalty, data) {
    # validate arguments
    assertthat::assert_that(
      inherits(x$data$cost, c("data.frame", "Spatial", "sf")),
      is.numeric(penalty), assertthat::noNA(penalty)
      assertthat::noNA(data), number_of_zones(x) == length(data),
      all(data %in% names(x$data$cost)))
    # extract planning unit data
    d <- x$data$cost
    if (inherits(d, "Spatial")) {
      d <- as.matrix(d@data[, data, drop = FALSE])
    } else if (inherits(d, "sf")) {
      d <- as.matrix(sf::st_drop_geometry(d)[, data, drop = FALSE])
    } else {
      d <- as.matrix(d[, data, drop = FALSE])
    }
    # additional checks
    assertthat::assert_that(
      is.numeric(d),
      msg = paste("argument to data correspond to non-numeric fields in",
                  "the planning unit data associated with x"))
    assertthat::assert_that(
      assertthat::noNA(d),
      msg = paste("argument to data correspond to fields with NA values in",
                  "the planning unit data associated with x"))
    # add penalties
    add_linear_penalties(x, penalty, d)
})

#' @name add_linear_penalties
#' @usage \S4method{add_linear_penalties}{ConservationProblem,ANY,numeric}(x, penalty, data)
#' @rdname add_linear_penalties
methods::setMethod("add_linear_penalties",
  methods::signature("ConservationProblem", "ANY", "numeric"),
  function(x, penalty, data) {
    assertthat::assert_that(
      is.numeric(penalty), assertthat::noNA(penalty)
      assertthat::noNA(data), number_of_total_units(x) == length(data))
    add_linear_penalties(x, penalty, matrix(x, nrow = 1))
})

#' @name add_linear_penalties
#' @usage \S4method{add_linear_penalties}{ConservationProblem,ANY,matrix}(x, penalty, data)
#' @rdname add_linear_penalties
methods::setMethod("add_linear_penalties",
  methods::signature("ConservationProblem", "ANY", "matrix"),
  function(x, penalty, data) {
    assertthat::assert_that(
      is.numeric(penalty), assertthat::noNA(penalty)
      assertthat::noNA(c(data)), number_of_total_units(x) == ncol(data),
      number_of_zones(x) == nrow(data))
    add_linear_penalties(x, penalty, methods::as(data, "dgCMatrix"))
})

#' @name add_linear_penalties
#' @usage \S4method{add_linear_penalties}{ConservationProblem,ANY,Matrix}(x, penalty, data)
#' @rdname add_linear_penalties
methods::setMethod("add_linear_penalties",
  methods::signature("ConservationProblem", "ANY", "Matrix"),
  function(x, penalty, data) {
    assertthat::assert_that(
      is.numeric(penalty), assertthat::noNA(penalty)
      assertthat::noNA(c(data)),
      number_of_total_units(x) == ncol(data), number_of_zones(x) == nrow(data))
    add_linear_penalties(x, penalty, methods::as(data, "dgCMatrix"))
})

#' @name add_linear_penalties
#' @usage \S4method{add_linear_penalties}{ConservationProblem,ANY,Raster}(x, penalty, data)
#' @rdname add_linear_penalties
methods::setMethod("add_linear_penalties",
  methods::signature("ConservationProblem", "ANY", "Raster"),
  function(x, penalty, data) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ConservationProblem"),
      inherits(data, "Raster"),
      is.numeric(penalty), assertthat::noNA(penalty)
      number_of_zones(x) == raster::nlayers(data),
      number_of_zones(x) == length(penalty),
      inherits(x$data$cost, c("sf", "Spatial", "Raster")))
    # extract penalty data
    if (inherits(x$data$cost, c("sf", "Spatial"))) {
      d <- fast_extract(data, x$data$cost, fun = "sum")
    } else {
      assertthat::assert_that(is_comparable_raster(x$data$cost, data[[1]]))
      d <- t(as.matrix(raster::as.data.frame(data)))
    }
    # add penalties
    add_linear_penalties(x, penalty, methods::as(data, "dgCMatrix"))
})

#' @name add_linear_penalties
#' @usage \S4method{add_linear_penalties}{ConservationProblem,ANY,dgCMatrix}(x, penalty, data)
#' @rdname add_linear_penalties
methods::setMethod("add_linear_penalties",
  methods::signature("ConservationProblem", "ANY", "dgCMatrix"),
  function(x, penalty, data) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ConservationProblem"),
      is.numeric(penalty), isTRUE(all(is.finite(penalty))),
      is.numeric(data@x), all(is.finite(data@x),
      number_of_total_units(x) == ncol(data),
      number_of_zones(x) == nrow(data)))
    # add penalties
    x$add_penalty(pproto(
      "LinearPenalty",
      Penalty,
      name = "Linear penalties",
      data = list(data = data),
      parameters = parameters(numeric_parameter_array("penalty", penalty)),
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
                                inherits(y, "ConservationProblem"))
        # extract parameters
        p <- self$parameters$get("penalty")
        if (min(abs(p)) > 1e-50) {
          # extract data
          indices <- y$planning_unit_indices()
          d <- self$get_data("data")[, indices, drop = FALSE]
          # apply penalties
          rcpp_apply_linear_penalties(x$ptr, p, m)
        }
        invisible(TRUE)
    }))
})
