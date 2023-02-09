#' @include internal.R ConservationProblem-class.R zones.R add_manual_targets.R
NULL

#' Add manually specified locked constraints
#'
#' Add constraints to a conservation planning [problem()] to ensure
#' that solutions allocate (or do not allocate) specific planning units to
#' specific management zones. This function offers more fine-grained control
#' than the [add_locked_in_constraints()] and
#' [add_locked_out_constraints()] functions.
#'
#' @usage add_manual_locked_constraints(x, data)
#'
#' @param x [problem()] object.
#'
#' @param data `data.frame` or [tibble::tibble()] object.
#'   See the Data format section for more information.
#'
#' @section Data format:
#' The argument to `data` should be a `data.frame` with the following fields
#' (columns):
#'
#' \describe{
#'
#' \item{pu}{`integer` planning unit identifier.}
#'
#' \item{zone}{`character` names of zones. Note that this
#'   argument is optional for arguments to `x` that contain a single
#'   zone.}
#'
#' \item{status}{`numeric` values indicating how much
#'   of each planning unit should be allocated to each zone in the solution.
#'   For example, the `numeric` values could be binary values (i.e., zero
#'   or one) for problems containing binary-type decision variables
#'   (using the [add_binary_decisions()] function). Alternatively,
#'   the `numeric` values could be proportions (e.g., 0.5) for problems
#'   containing proportion-type decision variables (using the
#'   [add_proportion_decisions()]).}
#'
#'  }
#'
#' @inherit add_contiguity_constraints return
#'
#' @seealso
#' See [constraints] for an overview of all functions for adding constraints.
#'
#' @family constraints
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_features <- get_sim_features()
#' sim_pu_zones_polygons <- get_sim_zones_pu_polygons()
#' sim_features_zones <- get_sim_zones_features()
#'
#' # create minimal problem
#' p1 <-
#'   problem(sim_pu_polygons, sim_features, "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with locked in constraints using add_locked_constraints
#' p2 <- p1 %>% add_locked_in_constraints("locked_in")
#'
#' # create identical problem using add_manual_locked_constraints
#' locked_data <- data.frame(
#'   pu = which(sim_pu_polygons$locked_in),
#'   status = 1
#' )
#'
#' p3 <- p1 %>% add_manual_locked_constraints(locked_data)
#'
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#'
#' # create object with all solutions
#' s4 <- sf::st_sf(
#'   tibble::tibble(
#'     s1 = s1$solution_1,
#'     s2 = s2$solution_1,
#'     s3 = s3$solution_1
#'   ),
#'   geometry = sf::st_geometry(s1)
#' )
#'
#' # plot solutions
#' plot(
#'   s4,
#'   main = c(
#'     "none locked in",
#'     "add_locked_in_constraints",
#'     "add_manual_constraints"
#'   )
#' )
#'
#' # create minimal problem with multiple zones
#' p5 <-
#'   problem(
#'     sim_pu_zones_polygons, sim_features_zones,
#'     c("cost_1", "cost_2", "cost_3")
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create data.frame with the following constraints:
#' # planning units 1, 2, and 3 must be allocated to zone 1 in the solution
#' # planning units 4, and 5 must be allocated to zone 2 in the solution
#' # planning units 8 and 9 must not be allocated to zone 3 in the solution
#' locked_data2 <- data.frame(
#'   pu = c(1, 2, 3, 4, 5, 8, 9),
#'   zone = c(rep("zone_1", 3), rep("zone_2", 2),rep("zone_3", 2)),
#'   status = c(rep(1, 5), rep(0, 2))
#' )
#'
#' # print locked constraint data
#' print(locked_data2)
#'
#' # create problem with added constraints
#' p6 <- p5 %>% add_manual_locked_constraints(locked_data2)
#'
#' # solve problem
#' s5 <- solve(p5)
#' s6 <- solve(p6)
#'
#' # create two new columns representing the zone id that each planning unit
#' # was allocated to in the two solutions
#' s5$solution <- category_vector(sf::st_drop_geometry(
#'   s5[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' ))
#' s5$solution <- factor(s5$solution)
#'
#' s5$solution_locked <- category_vector(sf::st_drop_geometry(
#'   s5[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' ))
#' s5$solution_locked <- factor(s5$solution_locked)
#'
#' # plot solutions
#' plot(s5[, c("solution", "solution_locked")], axes = FALSE)
#' }
#'
#' @name add_manual_locked_constraints
#'
#' @exportMethod add_manual_locked_constraints
#'
#' @aliases add_manual_locked_constraints,ConservationProblem,data.frame-method add_manual_locked_constraints,ConservationProblem,tbl_df-method
#'
#' @export
methods::setGeneric(
  "add_manual_locked_constraints",
  signature = methods::signature("x", "data"),
  function(x, data) {
    rlang::check_required(x)
    rlang::check_required(data)
    assert(
      is_conservation_problem(x),
      is.data.frame(data)
    )
    standardGeneric("add_manual_locked_constraints")
  }
)

#' @name add_manual_locked_constraints
#' @usage \S4method{add_manual_locked_constraints}{ConservationProblem,data.frame}(x, data)
#' @rdname add_manual_locked_constraints
methods::setMethod("add_manual_locked_constraints",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is.data.frame(data)
    )
    # add constraints
    add_manual_locked_constraints(x, tibble::as_tibble(data))
  }
)

#' @name add_manual_locked_constraints
#' @usage \S4method{add_manual_locked_constraints}{ConservationProblem,tbl_df}(x, data)
#' @rdname add_manual_locked_constraints
methods::setMethod("add_manual_locked_constraints",
  methods::signature("ConservationProblem", "tbl_df"),
  function(x, data) {
    assert(
      is_conservation_problem(x),
      is.data.frame(data),
      inherits(data, "tbl_df"),
      nrow(data) > 0,
      assertthat::has_name(data, "pu"),
      is.numeric(data$pu),
      all_finite(data$pu),
      is_count_vector(data$pu),
      max(data$pu) <= number_of_total_units(x),
      min(data$pu) >= 0,
      assertthat::has_name(data, "status"),
      is.numeric(data$status),
      all_finite(data$status)
    )
    if (assertthat::has_name(data, "zone") || x$number_of_zones() > 1) {
      assert(
        assertthat::has_name(data, "zone"),
        is_inherits(data$zone, c("character", "factor")),
        all_match_of(as.character(data$zone), zone_names(x))
      )
    }
    # set attributes
    if (x$number_of_zones() == 1) {
      if (all(data$status == 1)) {
        class_name <- "LockedInConstraint"
        constraint_name <- "locked in constraints"
      } else if (all(data$status == 0)) {
         class_name <- "LockedOutConstraint"
        constraint_name <- "locked out constraints"
      } else {
       class_name <- "LockedManualConstraint"
         constraint_name <- "manual locked constraints"
      }
    } else {
      class_name <- "LockedManualConstraint"
      constraint_name <- "manual locked constraints"
    }
    # add constraints
    x$add_constraint(
      R6::R6Class(
        class_name,
        inherit = Constraint,
        public = list(
          name = constraint_name,
          data = list(data = data),
          repr = function(compact = TRUE) {
            paste0(
              self$name, " (", nrow(self$get_data("data")), " planning units)"
            )
          },
          apply = function(x, y) {
            # assert argument is valid
            assert(
              inherits(x, "OptimizationProblem"),
              inherits(y, "ConservationProblem"),
              .internal = TRUE
            )
            # get locked data
            d <- self$get_data("data")
            # convert zone names to indices
            if (!assertthat::has_name(d, "zone"))
              d$zone <- y$zone_names()[1]
            d$zone <- match(as.character(d$zone), y$zone_names())
            # remove rows for raster cells that aren't really planning units
            # i.e., contain NA values in all zones
            pu <- y$get_data("cost")
            if (inherits(pu, c("SpatRaster", "Raster"))) {
              units <- y$planning_unit_indices()
              d$pu <- match(d$pu, units)
              d <- d[!is.na(d$pu), , drop = FALSE]
            }
            # apply constraints
            invisible(
              rcpp_apply_locked_constraints(
                x$ptr, c(d$pu), c(d$zone), d$status
              )
            )
          }
        )
      )$new()
    )
  }
)
