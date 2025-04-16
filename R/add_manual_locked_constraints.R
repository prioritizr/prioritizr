#' @include internal.R ConservationProblem-class.R zones.R add_manual_targets.R
NULL

#' Add manually specified locked constraints
#'
#' Add constraints to a conservation planning problem to ensure
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
#'
#' The argument to `data` should be a `data.frame` with the following columns:
#'
#' \describe{
#'
#' \item{pu}{`integer` planning unit identifiers.
#'   If `x` has `data.frame` planning units,
#'   then these values must refer to values in the `id` column of the planning
#'   unit data.
#'   Alternatively, if `x` has [sf::st_sf()] or `matrix` planning units,
#'   then these values must refer to the row numbers of the planning unit data.
#'   Additionally, if `x` has `numeric` vector planning units,
#'   then these values must refer to the element indices of the planning unit
#'   data.
#'   Finally, if `x` has [terra::rast()] planning units,
#'   then these values must refer to cell indices.}
#'
#' \item{zone}{`character` names of zones. Note that this
#'   argument is optional for arguments to `x` that contain a single
#'   zone.}
#'
#' \item{status}{`numeric` status values. These values indicate how much
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
#' sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
#' sim_zones_features <- get_sim_zones_features()
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
#' ## s1 = none locked in
#' ## s2 = locked in constraints
#' ## s3 = manual locked constraints
#' plot(s4)
#'
#' # create minimal problem with multiple zones
#' p5 <-
#'   problem(
#'     sim_zones_pu_polygons, sim_zones_features,
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
#'   s6[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
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
    assert_required(x)
    assert_required(data)
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
    # assert valid arguments
    assert(
      ## x
      is_conservation_problem(x),
      ## data
      is.data.frame(data),
      inherits(data, "tbl_df"),
      nrow(data) > 0,
      ### data$pu
      assertthat::has_name(data, "pu"),
      is.numeric(data$pu),
      all_finite(data$pu),
      all_is_valid_total_unit_ids(x, data$pu),
      ### data$status
      assertthat::has_name(data, "status"),
      is.numeric(data$status),
      all_finite(data$status)
    )
    ## data$zone
    if (assertthat::has_name(data, "zone") || x$number_of_zones() > 1) {
      assert(
        assertthat::has_name(data, "zone"),
        is_inherits(data$zone, c("character", "factor")),
        all_match_of(as.character(data$zone), zone_names(x))
      )
    }
    # convert planning unit identifiers to indices
    data$idx <- x$convert_total_unit_ids_to_indices(data$pu)
    # add constraints
    internal_add_manual_locked_constraints(
      x,
      data[!is.na(data$idx), , drop = FALSE]
    )
  }
)

#' Internal function for add_manual_locked_constraints()
#'
#' This function is used to add manual locked constraints.
#'
#' @param x [problem()] object.
#'
#' @param data `data.frame` or [tibble::tibble()] object.
#'   See the Data format section for more information.
#'
#' @section Data format:
#' The argument to `data` should be a `data.frame` with the following columns:
#'
#' \describe{
#'
#' \item{idx}{`integer` total unit indices.
#'  These values should refer to `x$total_unit_indices()`}.
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
#' @noRd
internal_add_manual_locked_constraints <- function(
  x, data, call = fn_caller_env()
) {
  assert(
    ## x
    is_conservation_problem(x),
    is.data.frame(data),
    ## data
    inherits(data, "tbl_df"),
    nrow(data) > 0,
    ## data$idx
    assertthat::has_name(data, "idx"),
    is.numeric(data$idx),
    all_finite(data$idx),
    is_count_vector(data$idx),
    ## data$status
    assertthat::has_name(data, "status"),
    is.numeric(data$status),
    all_finite(data$status),
    call = call
  )
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
          # if locked data is missing a zone column,
          # then this is because the problem has only one zone and the
          # user did not specify this information.
          # thus add in a column with the name of this single zone
          if (!assertthat::has_name(d, "zone")) d$zone <- y$zone_names()[[1]]
          # convert zone names to indices
          d$zone <- match(as.character(d$zone), y$zone_names())
          # convert total unit indices to indices that describe their
          # relative position in the MILP formulation
          i <- match(d$idx, y$planning_unit_indices())
          # note that any total units which are not planning units --
          # because they have NA cost values -- will have NA values in i.
          # thus we need to exclude these values when applying the constraints
          keep <- !is.na(i)
          # apply constraints
          invisible(
            rcpp_apply_locked_constraints(
              x$ptr, i[keep], d$zone[keep], d$status[keep]
            )
          )
        }
      )
    )$new()
  )
}
