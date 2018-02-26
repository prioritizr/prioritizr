#' @include internal.R Constraint-proto.R intersecting_units.R
NULL

#' Add locked in constraints
#'
#' Add constraints to ensure that they are prioritized in the solution.
#' For example, it may be desirable to lock in planning units that are
#' inside existing protected areas so that the solution fills in the gaps in the
#' existing reserve network. If specific planning units should be locked out
#' of a solution, use \code{\link{add_locked_out_constraints}}. For problems
#' with non-binary planning unit allocations (e.g. proportions), the
#' \code{\link{add_locked_manual_constraints}} function can be used to lock
#' planning unit allocations to a specific value.
#'
#' @usage add_locked_in_constraints(x, locked_in)
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param locked_in Object that determines which planning units that should be
#'   locked in. See details for more information.
#'
#' @details The locked planning units can be specified in several different
#'  ways:
#'
#'   \describe{
#'
#'     \item{\code{integer}}{\code{vector} of indices pertaining to which
#'       planning units should be locked in the solution. This argument is only
#'       compatible with problems that contain a single zone.}
#'
#'     \item{\code{logical}}{\code{vector} containing \code{TRUE} and/or
#'       \code{FALSE} values that indicate which planning units should be locked
#'       in the solution. This argument is only compatible with problems that
#'       contain a single zone.}
#'
#'     \item{\code{matrix}}{containing \code{logical} \code{TRUE} and/or
#'       \code{FALSE} values which indicate if certain planning units are
#'       should be locked to a specific zone in the solution. Each row
#'       corresponds to a planning unit, each column corresponds to a zone, and
#'       each cell indicates if the planning unit should be locked to a given
#'       zone. Thus each row should only contain at most a single \code{TRUE}
#'       value.}
#'
#'     \item{\code{character}}{field (column) name(s) that indicate if planning
#'       units should be locked in the solution. This type of argument is only
#'       compatible if the planning units in the argument to \code{x} are a
#'       \code{\link[sp]{Spatial-class}} or \code{data.frame} object. The fields
#'       (columns) must have \code{logical}  (i.e. \code{TRUE} or \code{FALSE})
#'       values indicating if the planning unit is to be locked in the solution.
#'       For problems containing multiple zones, this argument should contain
#'       a field (column) name for each management zone.}
#'
#'     \item{\code{\link[raster]{Raster-class}}}{planning units in \code{x}
#'       that intersect with non-zero and non-\code{NA} raster cells are locked
#'       in the solution. For problems that contain multiple zones, the
#'       \code{\link[raster]{Raster-class}} object must contain a layer
#'       for each zone. Note that for multi-band arguments, each pixel must
#'       only contain a non-zero value in a single band.}
#'
#'  }
#'
#' @return \code{\link{ConservationProblem-class}} object.
#'
#' @examples
#' # create basic problem
#' p1 <- problem(sim_pu_polygons, sim_features, "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2)
#'
#' # create problem with added locked in constraints using integers
#' p2 <- p1 %>% add_locked_in_constraints(which(sim_pu_polygons$locked_in))
#'
#' # create problem with added locked in constraints using a field name
#' p3 <- p1 %>% add_locked_in_constraints("locked_in")
#'
#' # create problem with added locked in constraints using raster data
#' p4 <- p1 %>% add_locked_in_constraints(sim_locked_in_raster)
#'
#' # create problem with added locked in constraints using spatial polygons data
#' locked_in <- sim_pu_polygons[sim_pu_polygons$locked_in == 1, ]
#' p5 <- p1 %>% add_locked_in_constraints(locked_in)
#' \donttest{
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#' s4 <- solve(p4)
#' s5 <- solve(p5)
#'
#' # plot solutions
#' par(mfrow = c(3,2), mar = c(0, 0, 4.1, 0))
#' plot(s1, main = "none locked in")
#' plot(s1[s1$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s2, main = "locked in (integer input)")
#' plot(s2[s2$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s3, main = "locked in (character input)")
#' plot(s3[s3$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s4, main="locked in (raster input)")
#' plot(s4[s4$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s5, main="locked in (polygon input)")
#' plot(s5[s5$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' }
#'
#' @seealso \code{\link{constraints}}.
#'
#' @name add_locked_in_constraints
#'
#' @exportMethod add_locked_in_constraints
#'
#' @aliases add_locked_in_constraints,ConservationProblem,character-method add_locked_in_constraints,ConservationProblem,numeric-method add_locked_in_constraints,ConservationProblem,Raster-method add_locked_in_constraints,ConservationProblem,Spatial-method

#'
#' @export
methods::setGeneric("add_locked_in_constraints",
                    signature = methods::signature("x", "locked_in"),
                    function(x, locked_in)
                      standardGeneric("add_locked_in_constraints"))

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,numeric}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "numeric"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_in, c("integer", "numeric")),
        isTRUE(all(is.finite(locked_in))),
        isTRUE(all(round(locked_in) == locked_in)),
        isTRUE(max(locked_in) <= x$number_of_total_units()),
        isTRUE(min(locked_in) >= 1))
    # create parameters
    p <- parameters(binary_parameter("apply constraint?", 1L))
    # create new constraint object
    x$add_constraint(pproto(
      "LockedInConstraint",
      Constraint,
      name = "Locked in planning units",
      data = list(locked_in = as.integer(locked_in)),
      parameters = p,
      calculate = function(self, x) {
        assertthat::assert_that(inherits(x, "ConservationProblem"))
        if (is.Waiver(self$get_data("locked_in_indices"))) {
          if (inherits(x$get_data("cost"), "Raster")) {
            # get locked in units
            units <- self$get_data("locked_in")
            # if cost layer is a raster convert cell indices to relative
            # indices based on which cells in the cost layer are
            # finite (ie. not NA)
            units <- match(units, raster::Which(!is.na(x$get_data("cost")),
                                                cells = TRUE))
            units <- units[!is.na(units)]
            self$set_data("locked_in_indices", units)
          } else {
            self$set_data("locked_in_indices", self$get_data("locked_in"))
          }
        }
        invisible(TRUE)
      },
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
          inherits(y, "ConservationProblem"))
        if (self$parameters$get("apply constraint?") == 1) {
          # apply constraint
          invisible(rcpp_apply_locked_in_constraints(x$ptr,
            self$get_data("locked_in_indices")))
        }
        invisible(TRUE)
      }))
  }
)

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,character}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "character"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      assertthat::is.string(locked_in),
      inherits(x$data$cost, c("data.frame", "Spatial")),
      inherits(x$data$cost, "data.frame") ||
        isTRUE("data" %in% methods::slotNames(x$data$cost)),
      isTRUE(locked_in %in% names(x$data$cost)),
      isTRUE(inherits(x$data$cost[[locked_in]], "logical")))
    # add constraint
    add_locked_in_constraints(x, which(x$data$cost[[locked_in]]))
  }
)

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,Spatial}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_in, "Spatial"))
    # add constraints
    add_locked_in_constraints(x, intersecting_units(x$data$cost, locked_in))
  }
)

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,Raster}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "Raster"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(locked_in, "Raster"),
      isTRUE(raster::cellStats(locked_in, "sum") > 0))
    add_locked_in_constraints(x, intersecting_units(x$data$cost, locked_in))
  }
)
