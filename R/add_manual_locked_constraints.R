#' @include internal.R pproto.R ConservationProblem-proto.R zones.R add_manual_targets.R
NULL

#' Add manually specified locked constraints
#'
#' Add constraints to a conservation planning \code{\link{problem}} to ensure
#' that solutions allocate (or do not allocate) specific planning units to
#' specific management zones. This function offers more fine-grained control
#' than the \code{\link{add_locked_in_constraints}} and
#' \code{\link{add_locked_out_constraints}} functions.
#'
#' @usage add_manual_locked_constraints(x, data)
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param data \code{data.frame} or \code{\link[tibble]{tibble}} object. See
#'   the Details section for more information.
#'
#' @details The argument to \code{data} must contain the following fields
#'   (columns):
#'
#'   \describe{
#'
#'   \item{\code{"pu"}}{\code{integer} planning unit identifier.}
#'
#'   \item{\code{"zone"}}{\code{character} names of zones. Note that this
#'     argument is optional for arguments to \code{x} that contain a single
#'     zone.}
#'
#'   \item{\code{"status"}}{\code{numeric} values indicating how much
#'     of each planning unit should be allocated to each zone in the solution.
#'     For example, the \code{numeric} values could be binary values (i.e. zero
#'     or one) for problems containing binary-type decision variables
#'     (using the \code{\link{add_binary_decisions}} function). Alternatively,
#'     the \code{numeric} values could be proportions (e.g. 0.5) for problems
#'     containing proportion-type decision variables (using the
#'     \code{\link{add_proportion_decisions}}).}
#'
#'   }
#'
#' @inherit add_contiguity_constraints return seealso
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_polygons, sim_features, sim_pu_zones_polygons,
#'      sim_features_zones)
#'
#' # create minimal problem
#' p1 <- problem(sim_pu_polygons, sim_features, "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions()
#'
#' # create problem with locked in constraints using add_locked_constraints
#' p2 <- p1 %>% add_locked_in_constraints("locked_in")
#'
#' # create identical problem using add_manual_locked_constraints
#' locked_data <- data.frame(pu = which(sim_pu_polygons$locked_in),
#'                           status = 1)
#'
#' p3 <- p1 %>% add_manual_locked_constraints(locked_data)
#' \donttest{
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#'
#' # plot solutions
#' par(mfrow = c(1,3), mar = c(0, 0, 4.1, 0))
#' plot(s1, main = "none locked in")
#' plot(s1[s1$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s2, main = "add_locked_in_constraints")
#' plot(s2[s2$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s3, main = "add_manual_constraints")
#' plot(s3[s3$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' }
#' # create minimal problem with multiple zones
#' p4 <- problem(sim_pu_zones_polygons, sim_features_zones,
#'               c("cost_1", "cost_2", "cost_3")) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions()
#'
#' # create data.frame with the following constraints:
#' # planning units 1, 2, and 3 must be allocated to zone 1 in the solution
#' # planning units 4, and 5 must be allocated to zone 2 in the solution
#' # planning units 8 and 9 must not be allocated to zone 3 in the solution
#' locked_data2 <- data.frame(pu = c(1, 2, 3, 4, 5, 8, 9),
#'                            zone = c(rep("zone_1", 3), rep("zone_2", 2),
#'                                     rep("zone_3", 2)),
#'                            status = c(rep(1, 5), rep(0, 2)))
#'
#' # print locked constraint data
#' print(locked_data2)
#'
#' # create problem with added constraints
#' p5 <- p4 %>% add_manual_locked_constraints(locked_data2)
#' \donttest{
#' # solve problem
#' s4 <- solve(p4)
#' s5 <- solve(p5)
#'
#' # create two new columns representing the zone id that each planning unit
#' # was allocated to in the two solutions
#' s4$solution <- category_vector(s4@data[, c("solution_1_zone_1",
#'                                            "solution_1_zone_2",
#'                                            "solution_1_zone_3")])
#' s4$solution <- factor(s4$solution)
#'
#' s4$solution_locked <- category_vector(s5@data[, c("solution_1_zone_1",
#'                                                   "solution_1_zone_2",
#'                                                   "solution_1_zone_3")])
#' s4$solution_locked <- factor(s4$solution_locked)
#'
#' # plot solutions
#' spplot(s4, zcol = c("solution", "solution_locked"), axes = FALSE,
#'        box = FALSE)
#' }
#' @seealso \code{\link{constraints}}.
#'
#' @name add_manual_locked_constraints
#'
#' @exportMethod add_manual_locked_constraints
#'
#' @aliases add_manual_locked_constraints,ConservationProblem,data.frame-method add_manual_locked_constraints,ConservationProblem,tbl_df-method
#'
#' @export
methods::setGeneric("add_manual_locked_constraints",
                    signature = methods::signature("x", "data"),
                    function(x, data)
                      standardGeneric("add_manual_locked_constraints"))

#' @name add_manual_locked_constraints
#' @usage \S4method{add_manual_locked_constraints}{ConservationProblem,data.frame}(x, data)
#' @rdname add_manual_locked_constraints
methods::setMethod("add_manual_locked_constraints",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, data) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
                            inherits(data, "data.frame"))
    # add constraints
    add_manual_locked_constraints(x, tibble::as_tibble(data))
})

#' @name add_manual_locked_constraints
#' @usage \S4method{add_manual_locked_constraints}{ConservationProblem,tbl_df}(x, data)
#' @rdname add_manual_locked_constraints
methods::setMethod("add_manual_locked_constraints",
  methods::signature("ConservationProblem", "tbl_df"),
  function(x, data) {
    # define function to validate data
    validate_data <- function(data) {
      assertthat::assert_that(inherits(x, "ConservationProblem"),
                              inherits(data, "tbl_df"),
                              nrow(data) > 0,
                              assertthat::has_name(data, "pu"),
                              is.numeric(data$pu),
                              all(is.finite(data$pu)),
                              all(data$pu == round(data$pu)),
                              max(data$pu) <= number_of_total_units(x),
                              min(data$pu) >= 0,
                              assertthat::has_name(data, "status"),
                              is.numeric(data$status),
                              all(is.finite(data$status)))
      if (assertthat::has_name(data, "zone") || x$number_of_zones() > 1)
        assertthat::assert_that(assertthat::has_name(data, "zone"),
                                is.character(data$zone) ||
                                  is.factor(data$zone),
                                all(as.character(data$zone) %in%
                                  zone_names(x)))
      return(TRUE)
    }
    # assert valid arguments
    validate_data(data)
    # set attributes
    if (x$number_of_zones() == 1) {
      if (all(data$status == 1)) {
        class_name <- "LockedInConstraint"
        constraint_name <- "Locked in planning units"
      } else if (all(data$status == 0)) {
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
      utils::getFromNamespace("rHandsontableOutput", "rhandsontable")(x)
     # add constraints
     x$add_constraint(pproto(
      class_name,
      Constraint,
      name = constraint_name,
      repr = function(self) {
        paste0(self$name, " [", nrow(self$parameters$get("Locked data")),
               " locked units]")
      },
      parameters = parameters(misc_parameter("Locked data", data,
                                             vfun, rfun)),
      calculate = function(self, x) {
        assertthat::assert_that(inherits(x, "ConservationProblem"))
        # get locked data
        data <- self$parameters$get("Locked data")
        # convert zone names to indices
        if (!assertthat::has_name(data, "zone"))
          data$zone <- x$zone_names()[1]
        data$zone <- match(as.character(data$zone), x$zone_names())
        # remove rows for raster cells that aren't really planning units
        # i.e. contain NA values in all zones
        pu <- x$get_data("cost")
        if (inherits(pu, "Raster")) {
          if (raster::nlayers(pu) == 1) {
            units <- raster::Which(!is.na(pu), cells = TRUE)
          } else {
            units <- raster::Which(max(!is.na(pu)) > 0, cells = TRUE)
          }
          data$pu <- match(data$pu, units)
          data <- data[!is.na(data$pu), ]
        }
        self$set_data("data_std", data)
        invisible(TRUE)
      },
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
          inherits(y, "ConservationProblem"))
        data <- self$get_data("data_std")
        invisible(rcpp_apply_locked_constraints(x$ptr, c(data$pu),
                                                c(data$zone),
                                                data$status))
      }))
})
