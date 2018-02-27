#' @include internal.R pproto.R ConservationProblem-proto.R zones.R
NULL

#' Add Manually Specified Locked Constraints
#'
#' Add constraints to ensure that the solution allocates a specific
#' amount of certain planning units to different management zones. This
#' function offers more fine-grained control than the
#' \code{\link{add_locked_in_constraints}} and
#' \code{\link{add_locked_out_constraints}} functions.
#'
#' @usage add_locked_manual_constraints(x, locked)
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param locked \code{data.frame} or \code{\link[tibble]{tibble}} object. See
#'   the Details section for more information.
#'
#' @details The argument to \code{locked} must contain the following columns:
#'
#'   \describe{
#'
#'     \item{\code{"pu"}}{\code{integer} planning unit identifier.}
#'
#'     \item{\code{"zone"}}{\code{character} names of zones. Note that this
#'       argument is optional for arguments to \code{x} that involve a single
#'       zone.}
#'
#'     \item{\code{"status"}}{\code{numeric} values indicating how much
#'       of each planning unit should be allocated to each zone in the
#'       solution.}
#'  }
#'
#'
#' @return \code{\link{ConservationProblem-class}} object.
#'
#' @examples
#' # TODO
#'
#' @seealso \code{\link{constraints}}.
#'
#' @name add_locked_manual_constraints
#'
#' @exportMethod add_locked_manual_constraints
#'
#' @aliases add_locked_manual_constraints,ConservationProblem,data.frame-method add_locked_manual_constraints,ConservationProblem,tbl_df-method
#'
#' @export
methods::setGeneric("add_locked_manual_constraints",
                    signature = methods::signature("x", "locked"),
                    function(x, locked)
                      standardGeneric("add_locked_manual_constraints"))

#' @name add_locked_manual_constraints
#' @usage \S4method{add_locked_manual_constraints}{ConservationProblem,data.frame}(x, locked)
#' @rdname add_locked_manual_constraints
methods::setMethod("add_locked_manual_constraints",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, locked) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
                            inherits(locked, "data.frame"))
    # add constraints
    add_locked_manual_constraints(x, tibble::as.tibble(locked))
})

#' @name add_locked_manual_constraints
#' @usage \S4method{add_locked_manual_constraints}{ConservationProblem,tbl_df}(x, locked)
#' @rdname add_locked_manual_constraints
methods::setMethod("add_locked_manual_constraints",
  methods::signature("ConservationProblem", "tbl_df"),
  function(x, locked) {
    # define function to validate data
    validate_data <- function(locked) {
      assertthat::assert_that(inherits(x, "ConservationProblem"),
                              inherits(locked, "tbl_df"),
                              assertthat::has_name(locked, "pu"),
                              is.numeric(locked$pu),
                              all(is.finite(locked$pu)),
                              all(locked$pu == round(locked$pu)),
                              assertthat::has_name(locked, "status"),
                              is.numeric(locked$status),
                              all(is.finite(locked$status)))
      if (assertthat::has_name(locked, "zone") || x$number_of_zones() > 1)
        assertthat::assert_that(assertthat::has_name(locked, "zone"),
                                is.character(locked$zone) ||
                                  is.factor(locked$zone),
                                all(as.character(locked$zone) %in%
                                  x$zone_names()))
      return(TRUE)
    }
    # assert valid arguments
    validate_data(locked)
    # set attributes
    if (x$number_of_zones() == 1) {
      if (all(locked$status == 1)) {
        class_name <- "LockedInConstraint"
        constraint_name <- "Locked in planning units"
      } else if (all(locked$status == 0)) {
         class_name <- "LockedOutConstraint"
        constraint_name <- "Locked out planning units"
      } else {
       class_name <- "LockedManualConstraint"
         constraint_name <- "Manually locked planning units"
      }
    } else {
      class_name <- "LockedManualConstraint"
      constraint_name <- "Manually locked planning units"
    }
    # define function to validate changes to data
    vfun <- function(x) !inherits(try(validate_data(x), silent = TRUE),
                                  "try-error")
    # define function to render data
    rfun <- function(x)
      getFromNamespace("rhandsontable", "rHandsontableOutput")(x)
     # add constraints
     x$add_constraint(pproto(
      class_name,
      Constraint,
      name = constraint_name,
      parameters = parameters(misc_parameter("Locked data", locked,
                                             vfun, rfun)),
      calculate = function(self, x) {
        assertthat::assert_that(inherits(x, "ConservationProblem"))
        # get locked data
        locked <- self$parameters$get("Locked data")
        # convert zone names to indices
        if (!assertthat::has_name(locked, "zone"))
          locked$zone <- x$zone_names()[1]
        locked$zone <- match(locked$zone, x$zone_names())
        # remove rows for raster cells that aren't really planning units
        # i.e. contain NA values in all zones
        pu <- x$get_data("cost")
        if (inherits(pu, "Raster")) {
          if (raster::nlayers(pu) == 1) {
            units <- raster::Which(!is.na(pu), cells = TRUE)
          } else {
            units <- raster::Which(max(!is.na(pu)) > 0, cells = TRUE)
          }
          locked$pu <- match(locked$pu, units)
          locked <- locked[!is.na(locked$pu), ]
        }
        self$set_data("locked_std", locked)
        invisible(TRUE)
      },
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
          inherits(y, "ConservationProblem"))
        locked <- self$get_data("locked_std")
        invisible(rcpp_apply_locked_constraints(x$ptr, locked$pu,
                                                 locked$zone,
                                                 locked$status))
      }))
})
