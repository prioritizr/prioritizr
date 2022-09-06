#' @include internal.R Penalty-proto.R marxan_boundary_data_to_matrix.R
NULL

#' Add linear penalties
#'
#' Add penalties to a conservation planning [problem()] to penalize
#' solutions that select planning units with higher values from a specific
#' data source (e.g., anthropogenic impact). These penalties assume
#' a linear trade-off between the penalty values and the primary
#' objective of the conservation planning [problem()] (e.g.,
#' solution cost for minimum set problems; [add_min_set_objective()].
#'
#' @param x [problem()] (i.e., [`ConservationProblem-class`]) object.
#'
#' @param penalty `numeric` penalty value that is used to scale the
#'   importance not selecting planning units with high `data` values.
#'   Higher `penalty` values can be used to obtain solutions that
#'   are strongly averse to selecting places with high `data`
#'   values, and smaller `penalty` values can be used to obtain solutions
#'   that only avoid places with especially high `data` values.
#'   Note that negative
#'   `penalty` values can be used to obtain solutions that prefer places
#'   with high `data` values. Additionally, when adding these
#'   penalties to problems with multiple zones, the argument to `penalty`
#'   must have a value for each zone.
#'
#' @param data `character`, `numeric`,
#'   [`Raster-class`], `matrix`, or `Matrix` object
#'   containing the values used to penalize solutions. Planning units that are
#'   associated with higher data values are penalized more strongly
#'   in the solution. See the Data format section for more information.
#'
#' @details
#' This function penalizes solutions that have higher values according
#' to the sum of the penalty values associated with each planning unit,
#' weighted by status of each planning unit in the solution.
#'
#' @section Data format:
#'
#' The argument to `data` can be specified using the following formats.
#'
#' \describe{
#'
#' \item{`data` as `character` vector}{containing field (column) name(s) that
#'   contain penalty values for planning units. This format is only
#'   compatible if the planning units in the argument to `x` are a
#'   [`Spatial-class`], [sf::sf()], or
#'   `data.frame` object. The fields (columns) must have `numeric`
#'   values, and must not contain any missing (`NA`) values.
#'   For problems that contain a single zone, the argument to `data` must
#'   contain a single field name. Otherwise, for problems that
#'   contain multiple zones, the argument to `data` must
#'   contain a field name for each zone.}
#'
#' \item{`data` as a `numeric` vector}{containing values for
#'   planning units. These values must not contain any missing
#'   (`NA`) values. Note that this format is only available
#'   for planning units that contain a single zone.}
#'
#' \item{`data` as a `matrix`/`Matrix` object}{containing `numeric` values
#'   that specify data for each planning unit.
#'   Each row corresponds to a planning unit, each column corresponds to a
#'   zone, and each cell indicates the data for penalizing a planning unit
#'   when it is allocated to a given zone.}
#'
#' \item{`data` as a [`Raster-class`] object}{containing values for planning
#'   units. This format is only
#'   compatible if the planning units in the argument to `x` are
#'   [`Spatial-class`], [sf::sf()], or
#'   [`Raster-class`] objects.
#'   If the planning unit data are a [`Spatial-class`] or
#'   [sf::sf()] object, then the values are calculated by overlaying the
#'   planning units with the argument to `data` and calculating the sum of the
#'   values associated with each planning unit.
#'   If the planning unit data are a
#'   [`Raster-class`] object then the values are calculated by extracting the
#'   cell
#'   values (note that the planning unit data and the argument to `data` must
#'   have exactly the same dimensionality, extent, and missingness).
#'   For problems involving multiple zones, the argument to `data` must
#'   contain a layer for each zone.}
#'
#' }
#'
#' @section Mathematical formulation:
#' The linear penalties are implemented using the following
#' equations.
#' Let \eqn{I} denote the set of planning units
#' (indexed by \eqn{i}), \eqn{Z} the set of management zones (indexed by
#' \eqn{z}), and \eqn{X_{iz}}{Xiz} the decision variable for allocating
#' planning unit \eqn{i} to zone \eqn{z} (e.g., with binary
#' values indicating if each planning unit is allocated or not). Also, let
#' \eqn{P_z} represent the penalty scaling value for zones
#' \eqn{z \in Z}{z in Z} (argument to `penalty`), and
#' \eqn{D_{iz}}{Diz} the penalty data for allocating planning unit
#' \eqn{i \in I}{i in I} to zones \eqn{z \in Z}{z in Z} (argument to
#' `data`, if supplied as a `matrix` object).
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{z}^{Z} P_z \times D_{iz} \times X_{iz}
#' }{
#' sum_i^I sum_z^Z (Pz * Diz * Xiz)
#' }
#'
#' Note that when the problem objective is to maximize some measure of
#' benefit and not minimize some measure of cost, the term \eqn{P_z} is
#' replaced with \eqn{-P_z}.
#'
#' @inherit add_boundary_penalties return
#'
#' @seealso
#' See [penalties] for an overview of all functions for adding penalties.
#'
#' @family penalties
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' data(sim_pu_polygons, sim_pu_zones_stack, sim_features, sim_features_zones)
#'
#' # add a column to contain the penalty data for each planning unit
#' # e.g., these values could indicate the level of habitat
#' sim_pu_polygons$penalty_data <- runif(nrow(sim_pu_polygons))
#'
#' # plot the penalty data to visualise its spatial distribution
#' spplot(sim_pu_polygons, zcol = "penalty_data", main = "penalty data",
#'        axes = FALSE, box = FALSE)
#'
#' # create minimal problem with minimum set objective,
#' # this does not use the penalty data
#' p1 <- problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # print problem
#' print(p1)
#'
#' # create an updated version of the previous problem,
#' # with the penalties added to it
#' p2 <- p1 %>% add_linear_penalties(100, data = "penalty_data")
#'
#' # print problem
#' print(p2)
#'
#' \dontrun{
#' # solve the two problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#'
#' # plot the solutions and compare them,
#' # since we supplied a very high penalty value (i.e., 100), relative
#' # to the range of values in the penalty data and the objective function,
#' # the solution in s2 is very sensitive to values in the penalty data
#' spplot(s1, zcol = "solution_1", main = "solution without penalties",
#'        axes = FALSE, box = FALSE)
#' spplot(s2, zcol = "solution_1", main = "solution with penalties",
#'        axes = FALSE, box = FALSE)
#'
#' # for real conservation planning exercises,
#' # it would be worth exploring a range of penalty values (e.g., ranging
#' # from 1 to 100 increments of 5) to explore the trade-offs
#' }
#'
#' # now, let's examine a conservation planning exercise involving multiple
#' # management zones
#'
#' \dontrun{
#' # create targets for each feature within each zone,
#' # these targets indicate that each zone needs to represent 10% of the
#' # spatial distribution of each feature
#' targ <- matrix(0.1, ncol = number_of_zones(sim_features_zones),
#'                nrow = number_of_features(sim_features_zones))
#'
#' # create penalty data for allocating each planning unit to each zone,
#' # these data will be generated by simulating values
#' penalty_stack <- simulate_cost(sim_pu_zones_stack[[1]],
#'                                n = number_of_zones(sim_features_zones))
#'
#' # plot the penalty data, each layer corresponds to a different zone
#' plot(penalty_stack, main = "penalty data", axes = FALSE, box = FALSE)
#'
#' # create a multi-zone problem with the minimum set objective
#' # and penalties for allocating planning units to each zone,
#' # with a penalty scaling factor of 1 for each zone
#' p3 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(targ) %>%
#'       add_linear_penalties(c(1, 1, 1), penalty_stack) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # print problem
#' print(p3)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(category_layer(s3), main = "multi-zone solution",
#'      axes = FALSE, box = FALSE)
#' }
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
      msg = paste("argument to data is a character and planning units",
                  "specified in x are not a data.frame, Spatial, or sf object"))
    assertthat::assert_that(
      is.numeric(penalty), assertthat::noNA(penalty),
      number_of_zones(x) == length(penalty),
      assertthat::noNA(data), number_of_zones(x) == length(data))
    assertthat::assert_that(
      all(data %in% names(x$data$cost)),
      msg = paste("argument to data is not a field name in the planning units",
                  "specified in x"))
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
      is.numeric(penalty), assertthat::noNA(penalty),
      number_of_zones(x) == length(penalty),
      assertthat::noNA(data), number_of_total_units(x) == length(data))
      add_linear_penalties(x, penalty, matrix(data, ncol = 1))
})

#' @name add_linear_penalties
#' @usage \S4method{add_linear_penalties}{ConservationProblem,ANY,matrix}(x, penalty, data)
#' @rdname add_linear_penalties
methods::setMethod("add_linear_penalties",
  methods::signature("ConservationProblem", "ANY", "matrix"),
  function(x, penalty, data) {
    assertthat::assert_that(
      is.numeric(penalty), assertthat::noNA(penalty),
      number_of_zones(x) == length(penalty),
      assertthat::noNA(c(data)),
      number_of_total_units(x) == nrow(data),
      number_of_zones(x) == ncol(data))
    add_linear_penalties(x, penalty, as_Matrix(data, "dgCMatrix"))
})

#' @name add_linear_penalties
#' @usage \S4method{add_linear_penalties}{ConservationProblem,ANY,Matrix}(x, penalty, data)
#' @rdname add_linear_penalties
methods::setMethod("add_linear_penalties",
  methods::signature("ConservationProblem", "ANY", "Matrix"),
  function(x, penalty, data) {
    assertthat::assert_that(
      is.numeric(penalty), assertthat::noNA(penalty),
      number_of_zones(x) == length(penalty),
      assertthat::noNA(c(data)),
      number_of_total_units(x) == nrow(data),
      number_of_zones(x) == ncol(data))
    add_linear_penalties(x, penalty, as_Matrix(data, "dgCMatrix"))
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
      is.numeric(penalty), assertthat::noNA(penalty),
      number_of_zones(x) == raster::nlayers(data),
      number_of_zones(x) == length(penalty),
      inherits(x$data$cost, c("sf", "Spatial", "Raster")))
    # extract penalty data
    if (inherits(x$data$cost, c("sf", "Spatial"))) {
      d <- fast_extract(data, x$data$cost, fun = "sum")
    } else {
      assertthat::assert_that(is_comparable_raster(x$data$cost, data[[1]]))
      d <- as.matrix(raster::as.data.frame(data))
    }
    d[is.na(d)] <- 0
    # add penalties
    add_linear_penalties(x, penalty, as_Matrix(d, "dgCMatrix"))
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
      number_of_zones(x) == length(penalty),
      is.numeric(data@x), all(is.finite(data@x),
      number_of_total_units(x) == nrow(data),
      number_of_zones(x) == ncol(data)))
    # create parameters
    if (number_of_zones(x) == 1) {
      p <- numeric_parameter("penalty", penalty)
    } else {
      p <- numeric_parameter_array("penalty", penalty, x$zone_names())
    }
    # add penalties
    x$add_penalty(pproto(
      "LinearPenalty",
      Penalty,
      name = "Linear penalties",
      data = list(data = data),
      parameters = parameters(p),
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
                                inherits(y, "ConservationProblem"))
        # extract parameters
        p <- self$parameters$get("penalty")
        if (inherits(p, "data.frame")) p <- p[[1]]
        if (min(abs(p)) > 1e-50) {
          # extract data
          indices <- y$planning_unit_indices()
          d <- self$get_data("data")[indices, , drop = FALSE]
          # apply penalties
          rcpp_apply_linear_penalties(x$ptr, p, d)
        }
        invisible(TRUE)
    }))
})
