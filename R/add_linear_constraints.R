#' @include internal.R
NULL

#' Add linear constraints
#'
#' Add constraints to a conservation planning problem to ensure
#' that all selected planning units meet certain criteria.
#'
#' @inheritParams add_contiguity_constraints
#'
#' @param sense `character` sense for the constraint. Available
#'  options include `">="`, `"<="`, or `"="` values.
#'
#' @param threshold `numeric` value.
#'   This threshold value is also known as a "right-hand-side" value
#'   per integer programming terminology.
#'
#' @param data `character`, `numeric`,
#'   [terra::rast()], `matrix`, or `Matrix` object
#'   containing the constraint values.
#'   These constraint values are also known as constraint coefficients
#'   per integer programming terminology.
#'   See the Data format section for more information.
#'
#' @inherit add_contiguity_constraints return
#'
#' @inheritSection add_linear_penalties Data format
#'
#' @details
#' This function adds general purpose constraints that can be used to
#' ensure that solutions meet certain criteria
#' (see Examples section below for details).
#' For example, these constraints can be used to add multiple budgets.
#' They can also be used to ensure that the total number of planning units
#' allocated to a certain administrative area (e.g., country) does not exceed
#' a certain threshold (e.g., 30% of its total area). Furthermore,
#' they can also be used to ensure that features have a minimal level
#' of representation (e.g., 30%) when using an objective
#' function that aims to enhance feature representation given a budget
#' (e.g., [add_min_shortfall_objective()]).
#'
#' @section Mathematical formulation:
#' The linear constraints are implemented using the following
#' equation.
#' Let \eqn{I} denote the set of planning units
#' (indexed by \eqn{i}), \eqn{Z} the set of management zones (indexed by
#' \eqn{z}), and \eqn{X_{iz}}{Xiz} the decision variable for allocating
#' planning unit \eqn{i} to zone \eqn{z} (e.g., with binary
#' values indicating if each planning unit is allocated or not). Also, let
#' \eqn{D_{iz}}{Diz} denote the constraint data associated with
#' planning units \eqn{i \in I}{i in I} for zones \eqn{z \in Z}{z in Z}
#' (argument to `data`, if supplied as a `matrix` object),
#' \eqn{\theta} denote the constraint sense
#' (argument to `sense`, e.g., \eqn{<=}), and \eqn{t} denote the constraint
#' threshold (argument to `threshold`).
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{z}^{Z} (D_{iz} \times X_{iz}) \space \theta \space t
#' }{
#' sum_i^I sum (Diz * Xiz) \theta t
#' }
#'
#' @seealso
#' See [constraints] for an overview of all functions for adding constraints.
#'
#' @family constraints
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create a baseline problem with minimum shortfall objective
#' p0 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_shortfall_objective(1800) %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s0 <- solve(p0)
#'
#' # plot solution
#' plot(s0, main = "solution", axes = FALSE)
#'
#' # now let's create some modified versions of this baseline problem by
#' # adding additional criteria using linear constraints
#'
#' # first, let's create a modified version of p0 that contains
#' # an additional budget of 1600 based on a secondary cost dataset
#'
#' # create a secondary cost dataset by simulating values
#' sim_pu_raster2 <- simulate_cost(sim_pu_raster)
#'
#' # plot the primary cost dataset (sim_pu_raster) and
#' # the secondary cost dataset (sim_pu_raster2)
#' plot(
#'   c(sim_pu_raster, sim_pu_raster2),
#'   main = c("sim_pu_raster", "sim_pu_raster2"),
#'   axes = FALSE
#' )
#'
#' # create a modified version of p0 with linear constraints that
#' # specify that the planning units in the solution must not have
#' # values in sim_pu_raster2 that sum to a total greater than 500
#' p1 <-
#'   p0 %>%
#'   add_linear_constraints(
#'     threshold = 500, sense = "<=", data = sim_pu_raster2
#'   )
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solutions s1 and s2 to compare them
#' plot(c(s0, s1), main = c("s0", "s1"), axes = FALSE)
#'
#' # second, let's create a modified version of p0 that contains
#' # additional constraints to ensure that each feature has
#' # at least 8% of its overall distribution represented by the solution
#'
#' # to achieve this, we need to calculate the total amount of each feature
#' # within the planning units so we can, in turn, set the constraint thresholds
#' feat_abund <- feature_abundances(p0)$absolute_abundance
#'
#' # create a modified version of p0 with additional constraints for each
#' # feature to specify that the planning units in the solution must
#' # secure at least 8% of the total abundance for each feature
#' p2 <- p0
#' for (i in seq_len(terra::nlyr(sim_features))) {
#'   p2 <-
#'     p2 %>%
#'     add_linear_constraints(
#'       threshold = feat_abund[i] * 0.08,
#'       sense = ">=",
#'       data = sim_features[[i]]
#'     )
#' }
#'
#' # overall, p2 could be described as an optimization problem
#' # that maximizes feature representation as much as possible
#' # towards securing 20% of the total amount of each feature,
#' # whilst ensuring that (i) the total cost of the solution does
#' # not exceed 1800 (per cost values in sim_pu_raster) and (ii)
#' # the solution secures at least 8% of the total amount of each feature
#' # (if 20% is not possible due to the budget)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solutions s0 and s2 to compare them
#' plot(c(s0, s2), main = c("s1", "s2"), axes = FALSE)
#'
#' # third, let's create a modified version of p0 that contains
#' # additional constraints to ensure that the solution equitably
#' # distributes conservation effort across different administrative areas
#' # (e.g., countries) within the study region
#'
#' # to begin with, we will simulate a dataset describing the spatial extent of
#' # four different administrative areas across the study region
#' sim_admin <- sim_pu_raster
#' sim_admin <- terra::aggregate(sim_admin, fact = 5)
#' sim_admin <- terra::setValues(sim_admin, seq_len(terra::ncell(sim_admin)))
#' sim_admin <- terra::resample(sim_admin, sim_pu_raster, method = "near")
#' sim_admin <- terra::mask(sim_admin, sim_pu_raster)
#'
#' # plot administrative areas layer,
#' # we can see that the administrative areas subdivide
#' # the study region into four quadrants, and the sim_admin object is a
#' # SpatRaster with integer values denoting ids for the administrative areas
#' plot(sim_admin, axes = FALSE)
#'
#' # next we will convert the sim_admin SpatRaster object into a SpatRaster
#' # object (with a layer for each administrative area) indicating which
#' # planning units belong to each administrative area using binary
#' # (presence/absence) values
#' sim_admin2 <- binary_stack(sim_admin)
#'
#' # plot binary stack of administrative areas
#' plot(sim_admin2, axes = FALSE)
#'
#' # we will now calculate the total amount of planning units associated
#' # with each administrative area, so that we can set the constraint threshold
#'
#' # since we are using raster data, we won't bother explicitly
#' # accounting for the total area of each planning unit (because all
#' # planning units have the same area in raster formats) -- but if we were
#' # using vector data then we would need to account for the area of each unit
#' admin_total <- Matrix::rowSums(rij_matrix(sim_pu_raster, sim_admin2))
#'
#' # create a modified version of p0 with additional constraints for each
#' # administrative area to specify that the planning units in the solution must
#' # not encompass more than 10% of the total extent of the administrative
#' # area
#' p3 <- p0
#' for (i in seq_len(terra::nlyr(sim_admin2))) {
#'   p3 <-
#'     p3 %>%
#'     add_linear_constraints(
#'       threshold = admin_total[i] * 0.1,
#'       sense = "<=",
#'       data = sim_admin2[[i]]
#'     )
#' }
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solutions s0 and s3 to compare them
#' plot(c(s0, s3), main = c("s0", "s3"), axes = FALSE)
#' }
#'
#' @name add_linear_constraints
#'
#' @exportMethod add_linear_constraints
#'
#' @aliases add_linear_constraints,ConservationProblem,ANY,ANY,Matrix-method add_linear_constraints,ConservationProblem,ANY,ANY,matrix-method add_linear_constraints,ConservationProblem,ANY,ANY,dgCMatrix-method add_linear_constraints,ConservationProblem,ANY,ANY,character-method add_linear_constraints,ConservationProblem,ANY,ANY,numeric-method add_linear_constraints,ConservationProblem,ANY,ANY,Raster-method add_linear_constraints,ConservationProblem,ANY,ANY,SpatRaster-method
NULL

#' @export
methods::setGeneric("add_linear_constraints",
  signature = methods::signature("x", "threshold", "sense", "data"),
  function(x, threshold, sense, data) {
    assert_required(x)
    assert_required(threshold)
    assert_required(sense)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      is_inherits(
        data,
        c(
          "character", "numeric",  "dgCMatrix",
          "matrix", "Matrix", "SpatRaster", "Raster"
        )
      )
    )
    standardGeneric("add_linear_constraints")
  }
)

#' @name add_linear_constraints
#' @usage \S4method{add_linear_constraints}{ConservationProblem,ANY,ANY,character}(x, threshold, sense, data)
#' @rdname add_linear_constraints
methods::setMethod("add_linear_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "character"),
  function(x, threshold, sense, data) {
    # validate arguments
    assert(
      assertthat::is.number(threshold),
      assertthat::noNA(threshold),
      assertthat::noNA(data),
      number_of_zones(x) == length(data)
    )
    assert(
      assertthat::is.string(sense),
      assertthat::noNA(sense),
      is_match_of(sense, c("<=", "=", ">="))
    )
    assert(
      is_inherits(x$data$cost, c("data.frame", "tbl_df", "sf", "Spatial")),
      msg = c(
        "{.arg data} cannot be a character vector.",
        "i" = paste(
          "This is because planning unit data for {.arg x} are not a",
          "data frame or {.cls sf}."
        )
      )
    )
    assert(
      all(data %in% names(x$data$cost)),
      msg = paste(
        "{.arg data} must contain character values that refer to column",
        "names of the planning unit data for {.arg x}."
      )
    )
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
    assert(
      is.numeric(d),
      msg = paste(
        "{.arg data} must contain character values that refer to numeric",
        "columns of the planning unit data for {.arg x}."
      )
    )
    assert(
      assertthat::noNA(d),
      msg = paste(
        "{.arg data} must not refer to columns",
        "of the planning unit data for {.arg} with missing",
        "({.val {NA}) values."
      )
    )
    # add penalties
    add_linear_constraints(x, threshold, sense, d)
  }
)

#' @name add_linear_constraints
#' @usage \S4method{add_linear_constraints}{ConservationProblem,ANY,ANY,numeric}(x, threshold, sense, data)
#' @rdname add_linear_constraints
methods::setMethod("add_linear_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "numeric"),
  function(x, threshold, sense, data) {
    add_linear_constraints(x, threshold, sense, matrix(data, ncol = 1))
  }
)

#' @name add_linear_constraints
#' @usage \S4method{add_linear_constraints}{ConservationProblem,ANY,ANY,matrix}(x, threshold, sense, data)
#' @rdname add_linear_constraints
methods::setMethod("add_linear_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "matrix"),
  function(x, threshold, sense, data) {
    add_linear_constraints(x, threshold, sense, as_Matrix(data, "dgCMatrix"))
  }
)

#' @name add_linear_constraints
#' @usage \S4method{add_linear_constraints}{ConservationProblem,ANY,ANY,Matrix}(x, threshold, sense, data)
#' @rdname add_linear_constraints
methods::setMethod("add_linear_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "Matrix"),
  function(x, threshold, sense, data) {
    add_linear_constraints(x, threshold, sense, as_Matrix(data, "dgCMatrix"))
  }
)

#' @name add_linear_constraints
#' @usage \S4method{add_linear_constraints}{ConservationProblem,ANY,ANY,Raster}(x, threshold, sense, data)
#' @rdname add_linear_constraints
methods::setMethod("add_linear_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "Raster"),
  function(x, threshold, sense, data) {
    cli_warning(raster_pkg_deprecation_notice)
    add_linear_constraints(x, threshold, sense, terra::rast(data, "SpatRaster"))
  }
)

#' @name add_linear_constraints
#' @usage \S4method{add_linear_constraints}{ConservationProblem,ANY,ANY,SpatRaster}(x, threshold, sense, data)
#' @rdname add_linear_constraints
methods::setMethod("add_linear_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "SpatRaster"),
  function(x, threshold, sense, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is_pu_spatially_explicit(x),
      inherits(data, "SpatRaster"),
      is_numeric_values(data),
      assertthat::is.number(threshold),
      assertthat::noNA(threshold),
      assertthat::is.string(sense),
      assertthat::noNA(sense),
      is_match_of(sense, c("<=", "=", ">=")),
      number_of_zones(x) == terra::nlyr(data)
    )
    # extract constraint data
    if (inherits(x$data$cost, c("sf", "Spatial"))) {
      d <- fast_extract(data, x$data$cost, fun = "sum")
    } else {
      assert(is_pu_comparable_raster(x, data[[1]]))
      d <- as.matrix(terra::as.data.frame(data, na.rm = FALSE))
    }
    d[is.na(d)] <- 0
    # add constraints
    add_linear_constraints(x, threshold, sense, as_Matrix(d, "dgCMatrix"))
  }
)

#' @name add_linear_constraints
#' @usage \S4method{add_linear_constraints}{ConservationProblem,ANY,ANY,dgCMatrix}(x, threshold, sense, data)
#' @rdname add_linear_constraints
methods::setMethod("add_linear_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "dgCMatrix"),
  function(x, threshold, sense, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      assertthat::is.number(threshold),
      assertthat::noNA(threshold),
      assertthat::is.string(sense),
      assertthat::noNA(sense),
      is_match_of(sense, c("<=", "=", ">=")),
      is_numeric_values(data),
      all_finite(data),
      number_of_total_units(x) == nrow(data),
      number_of_zones(x) == ncol(data)
    )
    # assert that constraint is even remotely possible
    if (identical(sense, ">=")) {
      assert(
        any(Matrix::colSums(data) >= threshold),
        msg = paste(
          "The linear constraint cannot be meet {.arg threshold} even if all",
          "planning units selected in the solution."
        )
      )
    }
    # add penalties
    x$add_constraint(
      R6::R6Class(
        "LinearConstraint",
        inherit = Constraint,
        public = list(
          name = "linear constraints",
          data = list(threshold = threshold, sense = sense, data = data),
          apply = function(x, y) {
            assert(
              inherits(x, "OptimizationProblem"),
              inherits(y, "ConservationProblem"),
              .internal = TRUE
            )
            # extract data
            indices <- y$planning_unit_indices()
            d <- self$get_data("data")[indices, , drop = FALSE]
            # apply constraints
            rcpp_apply_linear_constraints(
              x$ptr,
              self$get_data("threshold"),
              self$get_data("sense"),
              d
            )
            # return success
            invisible(TRUE)
          }
        )
      )$new()
    )
  }
)
