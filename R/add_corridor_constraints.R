#' @include internal.R Constraint-proto.R
NULL

#' Add corridor constraints
#'
#' It is important to maintain connectivity throughout a reserve network.
#' However, some areas are more difficult for species to traverse then other
#' areas. As a consequence, even though a reserve may protect a contiguous
#' section of land, some species may not be able to disperse throughout
#' the network if some of the land is a high barrier to dispersal. This
#' function adds constraints to ensure that all planning units used to
#' represent features in the conservation problem are connected by planning
#' units that have high connectivity.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param connectivities \code{object} used to calculate the the connectivity
#'   between planning units. See the Details section for more information.
#'
#' @param thresholds \code{numeric} value representing the minimum
#'   connectivity required between planning units for them to be
#'   considered connected for a given feature. Threshold values
#'   are expressed as a proportion of the total range of connectivity
#'   observed among all planning units for each feature. If a higher
#'   threshold is used, then planning units with lower conductances,
#'   and in turn, lower connectivity will not be considered
#'   to have strong enough connectivity to form corridors to link
#'   planning units. If \code{thresholds} is a single number then it is used
#'   as the threshold for each feature. Otherwise, if \code{threshold} is a
#'   \code{vector} then it specifies the threshold for each feature.
#'
#' @param ... additional arguments passed to \code{\link{fast_extract}} if
#'   the planning units in argument to \code{x} inherit from a
#'   \code{\link[raster]{RasterStack-class}} object.
#'
#' @details
#'   This function adds constraints to a conservation planning problem to ensure
#'   that all planning units used to represent a given feature are connected to
#'   each other. To achieve this, each planning unit is associated with
#'   conductance values that describe the ease at which individuals from
#'   each feature can disperse through it. Higher conductance values indicate
#'   that individuals can move planning units with greater ease. The
#'   connectivity between two planning units is calculated as the average
#'   conductance of the two planning units. After the connectivity values have
#'   been calculated, the threshold is applied to determine which planning
#'   units are "connected enough" to be used for linking planning units
#'   occupied by conspecifics. \strong{Adding these constraints to a problem
#'   will dramatically increase the amount of time required to solve it.}
#'
#'   The argument to \code{y} can be used to specify the
#'   the connectivity between different planning units in several different
#'   ways:
#'  \describe{
#'    \item{\code{character} \code{vector}}{If the planning units in
#'      argument to \code{x} inherit from a \code{\link[sp]{Spatial-class}}
#'      object then the argument to code{y} can refer to the
#'      names of the columns in the attribute table that contain the
#'      conductance values for each planning unit for each feature.
#'      It is assumed that the order of the column names in argument to
#'      \code{y} matches the order of the features in the
#'      argument to \code{x}.}
#'
#'    \item{\code{\link[raster]{RasterStack-class}} object}{Each band
#'      corresponds
#'      to each feature in the argument to \code{x}. The cells in each band
#'      denote the conductance of an area. For a given feature, the
#'      conductance of each planning unit is calculated by overlaying the
#'      planning units in argument to code{x} with the raster data in
#'      argument to \code{conductance}. Note that
#'      if the planning units in argument to \code{x} inherit from a
#'      \code{\link[raster]{Raster-class}} object, then the argument to
#'      code{conductance} must have the same spatial properties as the
#'      planning units (i.e. coordinate system, extent, resolution).}
#'
#'   \item{\code{list} of \code{\link[Matrix]{dsCMatrix-class}} matrices}{
#'      Each element in the list corresponds to a different feature. Each
#'      row and column refers to a different planning unit, and the cell
#'      values denote the connectivity between the two planning units. Note
#'      that the connectivity between planning units is assumed to be
#'      symmetric.}
#'  }
#'
#' @return \code{\link{ConservationProblem-class}} object.
#'
#' @seealso \code{\link{constraints}}.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features)
#'
#' # create a basic problem
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1)
#'
#' # create problem with added corridor constraints to ensure that
#' # planning units used to represent features are connected by
#' # planning units with habitat that is suitable for that feature
#' p2 <- p1 %>% add_corridor_constraints(sim_features, 0.5)
#'
#' \donttest{
#' # solve problems
#' s <- stack(solve(p1), solve(p2))
#'
#' # plot solutions
#' plot(s, main = c("basic solution", "solution with corridors"))
#'
#' }
#'
#' @export
add_corridor_constraints <- function(x, connectivities, thresholds,
                                     ...) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    inherits(connectivities, c("character", "Raster", "list")),
    is.numeric(thresholds), isTRUE(all(is.finite(thresholds))),
    isTRUE(all(thresholds >= 0)),
    (length(thresholds) == 1) || (length(thresholds) == x$number_of_features()))
  if (inherits(connectivities, "character"))
    assertthat::assert_that(inherits(x$get_data("cost"), "Spatial"),
      "data" %in% methods::slotNames(x$get_data("cost")),
      all(connectivities %in% names(x$get_data("cost"))))
  if (inherits(connectivities, "Raster"))
    assertthat::assert_that(
        raster::nlayers(connectivities) == x$number_of_features())
  if (inherits(connectivities, "list"))
    assertthat::assert_that(
        length(connectivities) == x$number_of_features(),
        all(vapply(connectivities, inherits, logical(1), "dsCMatrix")),
        all(vapply(connectivities, ncol, integer(1)) ==
            x$number_of_planning_units()),
        all(vapply(connectivities, nrow, integer(1)) ==
            x$number_of_planning_units()))
  # if thresholds is a single number than repeat it for each feature
  if (length(thresholds) == 1)
    thresholds <- rep(thresholds, x$number_of_features())
  # create parameters
  p <- parameters(binary_parameter("apply constraint?", 1),
                  proportion_parameter_array("thresholds", thresholds,
                    x$feature_names()))
  # create data
  d <- list(args = list(...))
  if (inherits(connectivities, "list")) {
    for (i in seq_along(connectivities))
      class(connectivities[[i]]) <- "dgCMatrix"
    d$connectivity_matrices <- connectivities
  } else {
    d$conductances <- connectivities
  }
  # create new constraint object
  x$add_constraint(pproto(
    "CorridorConstraint",
    Constraint,
    name = "Corridor constraint",
    compressed_formulation = FALSE,
    parameters = p,
    data = d,
    calculate = function(self, x) {
      # generate connectivity data
      if (is.Waiver(self$get_data("connectivity_matrices"))) {
        # obtain cost data
        cost <- x$get_data("cost")
        # obtain args for connectivity matrix
        args <- self$get_data("args")
        # obtain boundary data
        if (is.Waiver(x$get_data("boundary_matrix")))
          x$set_data("boundary_matrix", boundary_matrix(cost))
        args$boundary_data <- x$get_data("boundary_matrix")
        # obtain included data
        if (inherits(cost, "Raster") && is.Waiver(x$get_data("included"))) {
          x$set_data("included", raster::Which(!is.na(cost), cells = TRUE))
          args$included <- x$get_data("included")
        }
        # generate connectivity matrices for each feature
        cd <- self$get_data("conductances")
        if (inherits(cd, "Raster")) {
          itr <- seq_len(raster::nlayers(cd))
        } else {
          itr <- seq_along(cd)
        }
        cm <- lapply(
          itr,
          function(i) {
            x <- do.call(connectivity_matrix,
                         append(list(x = cost, y = cd[[i]]), args))
            class(x) <- "dgCMatrix"
            return(x)
          })
        # store list of connectivity matrices
        self$set_data("connectivity_matrices", cm)
        # return success
        invisible(TRUE)
      }
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      if (self$parameters$get("apply constraint?") > 0) {
        # extract list of connectivity matrices
        cm <- self$get_data("connectivity_matrices")
        # extract thresholds
        thresholds <- self$parameters$get("thresholds")[[1]]
        # convert thresholds to relative based on connectivity
        thresholds <- vapply(
          seq_along(cm),
          function(i) min(cm[[i]]@x) + (abs(diff(range(cm[[i]]@x))) *
                                        thresholds[[i]]),
          numeric(1))
        # apply the constraints
        rcpp_apply_corridor_constraints(x$ptr, cm, thresholds)

      }
    }))
}
