#' @include internal.R pproto.R ConservationProblem-proto.R zones.R add_manual_targets.R
NULL

#' Add manually specified bounds constraints
#'
#' Add constraints to a conservation planning [problem()] to ensure
#' that the planning unit values (e.g. proportion, binary) in a solution
#' range between specific lower and upper bounds. This function offers more
#' fine-grained control than the [add_manual_locked_constraints()]
#' function and is is most useful for problems involving proportion-type
#' or semi-continuous decisions.
#'
#' @usage add_manual_bounded_constraints(x, data)
#'
#' @param x [problem()] (i.e. [`ConservationProblem-class`]) object.
#'
#' @param data `data.frame` or [tibble::tibble()] object.
#'   See the Data format section for more information.
#'
#' @section Data format:
#' The argument to `data` must contain the following fields (columns):
#'
#' \describe{
#'
#' \item{pu}{`integer` planning unit identifier.}
#'
#' \item{zone}{`character` names of zones. Note that this
#'   argument is optional for arguments to `x` that contain a single
#'   zone.}
#'
#' \item{lower}{`numeric` values indicating the minimum
#'   value that each planning unit can be allocated to in each zone
#'   in the solution.}
#'
#' \item{upper}{`numeric` values indicating the maximum
#'   value that each planning unit can be allocated to in each zone
#'   in the solution.}
#'
#' }
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
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # create problem with locked in constraints using add_locked_constraints
#' p2 <- p1 %>% add_locked_in_constraints("locked_in")
#'
#' # create identical problem using add_manual_bounded_constraints
#' bounds_data <- data.frame(pu = which(sim_pu_polygons$locked_in),
#'                           lower = 1, upper = 1)
#'
#' p3 <- p1 %>% add_manual_bounded_constraints(bounds_data)
#' \dontrun{
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
#' plot(s3, main = "add_bounds_constraints")
#' plot(s3[s3$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' }
#' # create minimal problem with multiple zones
#' p4 <- problem(sim_pu_zones_polygons, sim_features_zones,
#'               c("cost_1", "cost_2", "cost_3")) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5,
#'                                   ncol = 3)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # create data.frame with the following constraints:
#' # planning units 1, 2, and 3 must be allocated to zone 1 in the solution
#' # planning units 4, and 5 must be allocated to zone 2 in the solution
#' # planning units 8 and 9 must not be allocated to zone 3 in the solution
#' bounds_data2 <- data.frame(pu = c(1, 2, 3, 4, 5, 8, 9),
#'                            zone = c(rep("zone_1", 3), rep("zone_2", 2),
#'                                     rep("zone_3", 2)),
#'                            lower = c(rep(1, 5), rep(0, 2)),
#'                            upper = c(rep(1, 5), rep(0, 2)))
#'
#' # print bounds data
#' print(bounds_data2)
#'
#' # create problem with added constraints
#' p5 <- p4 %>% add_manual_bounded_constraints(bounds_data2)
#' \dontrun{
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
#' s4$solution_bounded <- category_vector(s5@data[, c("solution_1_zone_1",
#'                                                    "solution_1_zone_2",
#'                                                    "solution_1_zone_3")])
#' s4$solution_bounded <- factor(s4$solution_bounded)
#'
#' # plot solutions
#' spplot(s4, zcol = c("solution", "solution_bounded"), axes = FALSE,
#'        box = FALSE)
#' }
#' @seealso [constraints].
#'
#' @name add_manual_bounded_constraints
#'
#' @exportMethod add_manual_bounded_constraints
#'
#' @aliases add_manual_bounded_constraints,ConservationProblem,data.frame-method add_manual_bounded_constraints,ConservationProblem,tbl_df-method
#'
#' @export
methods::setGeneric("add_manual_bounded_constraints",
                    signature = methods::signature("x", "data"),
                    function(x, data)
                      standardGeneric("add_manual_bounded_constraints"))

#' @name add_manual_bounded_constraints
#' @usage \S4method{add_manual_bounded_constraints}{ConservationProblem,data.frame}(x, data)
#' @rdname add_manual_bounded_constraints
methods::setMethod("add_manual_bounded_constraints",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, data) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
                            inherits(data, "data.frame"))
    # add constraints
    add_manual_bounded_constraints(x, tibble::as_tibble(data))
})

#' @name add_manual_bounded_constraints
#' @usage \S4method{add_manual_bounded_constraints}{ConservationProblem,tbl_df}(x, data)
#' @rdname add_manual_bounded_constraints
methods::setMethod("add_manual_bounded_constraints",
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
                              assertthat::has_name(data, "lower"),
                              is.numeric(data$lower),
                              all(is.finite(data$lower)),
                              assertthat::has_name(data, "upper"),
                              is.numeric(data$upper),
                              all(is.finite(data$upper)),
                              all(data$upper >= data$lower))
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
    # define function to validate changes to data
    vfun <- function(x) !inherits(try(validate_data(x), silent = TRUE),
                                  "try-error")
    # define function to render data
    rfun <- function(x)
      utils::getFromNamespace("rHandsontableOutput", "rhandsontable")(x)
     # add constraints
     x$add_constraint(pproto(
      "BoundsManualConstraint",
      Constraint,
      name = "Manually bounded planning units",
      repr = function(self) {
        paste0(self$name, " [", nrow(self$parameters$get("Bound data")),
               " bound units]")
      },
      parameters = parameters(misc_parameter("Bound data", data,
                                             vfun, rfun)),
      calculate = function(self, x) {
        assertthat::assert_that(inherits(x, "ConservationProblem"))
        # get bound data
        data <- self$parameters$get("Bound data")
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
        invisible(rcpp_apply_bounded_constraints(x$ptr, c(data$pu),
                                                 c(data$zone),
                                                 data$lower,
                                                 data$upper))
      }))
})
