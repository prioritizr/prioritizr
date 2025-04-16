#' @include internal.R ConservationProblem-class.R zones.R add_manual_targets.R
NULL

#' Add manually specified bound constraints
#'
#' Add constraints to a conservation planning problem to ensure
#' that the planning unit values (e.g., proportion, binary) in a solution
#' range between specific lower and upper bounds. This function offers more
#' fine-grained control than the [add_manual_locked_constraints()]
#' function and is is most useful for problems involving proportion-type
#' or semi-continuous decisions.
#'
#' @usage add_manual_bounded_constraints(x, data)
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
#' \item{lower}{`numeric` lower values. These values indicate the minimum
#'   value that each planning unit can be allocated to in each zone
#'   in the solution.}
#'
#' \item{upper}{`numeric` upper values. These values indicate the maximum
#'   value that each planning unit can be allocated to in each zone
#'   in the solution.}
#'
#' }
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
#' # create identical problem using add_manual_bounded_constraints
#' bounds_data <- data.frame(
#'   pu = which(sim_pu_polygons$locked_in),
#'   lower = 1,
#'   upper = 1
#' )
#'
#' p3 <- p1 %>% add_manual_bounded_constraints(bounds_data)
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
#' ## s3 = manual bounds constraints
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
#' bounds_data2 <- data.frame(
#'   pu = c(1, 2, 3, 4, 5, 8, 9),
#'   zone = c(rep("zone_1", 3), rep("zone_2", 2), rep("zone_3", 2)),
#'   lower = c(rep(1, 5), rep(0, 2)),
#'   upper = c(rep(1, 5), rep(0, 2))
#' )
#'
#' # print bounds data
#' print(bounds_data2)
#'
#' # create problem with added constraints
#' p6 <- p5 %>% add_manual_bounded_constraints(bounds_data2)
#'
#' # solve problem
#' s5 <- solve(p5)
#' s6 <- solve(p6)
#'
#' # create two new columns representing the zone id that each planning unit
#' # was allocated to in the two solutions
#' s5$solution <- category_vector(sf::st_drop_geometry(
#'   s5[, c("solution_1_zone_1","solution_1_zone_2", "solution_1_zone_3")]
#' ))
#' s5$solution <- factor(s5$solution)
#'
#' s5$solution_bounded <- category_vector(sf::st_drop_geometry(
#'   s6[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' ))
#' s5$solution_bounded <- factor(s5$solution_bounded)
#'
#' # plot solutions
#' plot(s5[, c("solution", "solution_bounded")], axes = FALSE)
#' }
#'
#' @name add_manual_bounded_constraints
#'
#' @exportMethod add_manual_bounded_constraints
#'
#' @aliases add_manual_bounded_constraints,ConservationProblem,data.frame-method add_manual_bounded_constraints,ConservationProblem,tbl_df-method
#'
#' @export
methods::setGeneric(
  "add_manual_bounded_constraints",
  signature = methods::signature("x", "data"),
  function(x, data) {
    assert_required(x)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      is.data.frame(data)
    )
    standardGeneric("add_manual_bounded_constraints")
  }
)

#' @name add_manual_bounded_constraints
#' @usage \S4method{add_manual_bounded_constraints}{ConservationProblem,data.frame}(x, data)
#' @rdname add_manual_bounded_constraints
methods::setMethod("add_manual_bounded_constraints",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is.data.frame(data)
    )
    # add constraints
    add_manual_bounded_constraints(x, tibble::as_tibble(data))
  }
)

#' @name add_manual_bounded_constraints
#' @usage \S4method{add_manual_bounded_constraints}{ConservationProblem,tbl_df}(x, data)
#' @rdname add_manual_bounded_constraints
methods::setMethod("add_manual_bounded_constraints",
  methods::signature("ConservationProblem", "tbl_df"),
  function(x, data) {
    # assert validat arguments
    assert(
      ## x
      is_conservation_problem(x),
      ## data
      is.data.frame(data),
      inherits(data, "tbl_df"),
      nrow(data) > 0,
      ## data$pu
      assertthat::has_name(data, "pu"),
      is.numeric(data$pu),
      all_finite(data$pu),
      is_count_vector(data$pu),
      all_is_valid_total_unit_ids(x, data$pu),
      ## data$lower
      assertthat::has_name(data, "lower"),
      is.numeric(data$lower),
      all_finite(data$lower),
      ## data$upper
      assertthat::has_name(data, "upper"),
      is.numeric(data$upper),
      all_finite(data$upper),
      all(data$upper >= data$lower)
    )
    ## data$zone
    if (assertthat::has_name(data, "zone") || x$number_of_zones() > 1) {
      assert(
        assertthat::has_name(data, "zone"),
        is_inherits(data$zone, c("character", "factor")),
        all_match_of(as.character(data$zone), zone_names(x))
      )
    }
    # add constraints
    x$add_constraint(
      R6::R6Class(
        "BoundsManualConstraint",
        inherit = Constraint,
        public = list(
          name = "manual bounded constraints",
          data = list(data = data),
          repr = function(compact = TRUE) {
            paste0(
              self$name, " (", nrow(self$get_data("data")),
              " planning units)"
            )
          },
          apply = function(x, y) {
            assert(
              inherits(x, "OptimizationProblem"),
              inherits(y, "ConservationProblem"),
              .internal = TRUE
            )
            # extract data
            d <- self$get_data("data")
            # if locked data is missing a zone column,
            # then this is because the problem has only one zone and the
            # user did not specify this information.
            # thus add in a column with the name of this single zone
            if (!assertthat::has_name(d, "zone")) d$zone <- y$zone_names()[[1]]
            # convert zone names to indices
            d$zone <- match(as.character(d$zone), y$zone_names())
            # convert total unit identifiers to indices
            d$idx <- y$convert_total_unit_ids_to_indices(data$pu)
            d <- d[!is.na(d$idx), , drop = FALSE]
            # convert total unit indices to indices that describe their
            # relative position in the MILP formulation
            i <- match(d$idx, y$planning_unit_indices())
            # note that any total units which are not planning units --
            # because they have NA cost values -- will have NA values in i.
            # thus we need to exclude these values when applying the constraints
            keep <- !is.na(i)
            # apply constraints
            invisible(
              rcpp_apply_bounded_constraints(
                x$ptr, i[keep], d$zone[keep], d$lower[keep], d$upper[keep]
              )
            )
          }
        )
      )$new()
    )
  }
)
