#' @include internal.R Penalty-proto.R marxan_boundary_data_to_matrix.R
NULL

#' Add asymmetric connectivity penalties
#'
#' Add penalties to a conservation planning [problem()] to account for
#' asymmetric connectivity between planning units.
#' Asymmetric connectivity data describe connectivity information that is
#' directional.
#' For example, asymmetric connectivity data could describe
#' the strength of rivers flowing between different planning unit. Since
#' river flow is directional, the level of connectivity
#' from an upstream planning unit to a downstream planning unit would
#' be higher than that for a downstream planning unit to an upstream planning
#' unit.
#'
#' @inheritParams add_connectivity_penalties
#'
#' @inheritSection add_connectivity_penalties Data format
#'
#' @details
#' This function adds penalties to conservation planning problem to penalize
#' solutions that have low connectivity.
#' Specifically, it penalizes solutions that select planning units that
#' share high connectivity values with other planning units that are
#' not selected by the solution (based on Beger *et al.* 2010).
#'
#' @section Mathematical formulation:
#' The connectivity penalties are implemented using the following equations.
#' Let \eqn{I} represent the set of planning units
#' (indexed by \eqn{i} or \eqn{j}), \eqn{Z} represent the set
#' of management zones (indexed by \eqn{z} or \eqn{y}), and \eqn{X_{iz}}{Xiz}
#' represent the decision variable for planning unit \eqn{i} for in zone
#' \eqn{z} (e.g., with binary
#' values one indicating if planning unit is allocated or not). Also, let
#' \eqn{p} represent the argument to `penalty`, \eqn{D} represent the
#' argument to `data`, and \eqn{W} represent the argument
#' to `zones`.
#'
#' If the argument to `data` is supplied as a `matrix` or
#' `Matrix` object, then the penalties are calculated as:
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z} \sum_{y}^{Z}
#' (p \times X_{iz} \times D_{ij} \times W_{zy}) -
#' \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z} \sum_{y}^{Z}
#' (p \times X_{iz} \times X_{jy} \times D_{ij} \times W_{zy})}{
#' sum_i^I sum_j^I sum_z^Z sum_y^Z
#' (p * Xiz * Dij * Wzy) -
#' sum_i^I sum_j^I sum_z^Z sum_y^Z
#' (p * Xiz * Xjy * Dij * Wzy)
#' }
#'
#' Otherwise, if the argument to `data` is supplied as an
#' `array` object, then the penalties are
#' calculated as:
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z} \sum_{y}^{Z}
#' (p \times X_{iz} \times D_{ijzy}) -
#' \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z} \sum_{y}^{Z}
#' (p \times X_{iz} \times X_{jy} \times D_{ijzy})}{
#' sum_i^I sum_j^I sum_z^Z sum_y^Z
#' (p * Xiz * Dijzy) -
#' sum_i^I sum_j^I sum_z^Z sum_y^Z
#' (p * Xiz * Xjy * Dijzy)
#' }
#'
#' Note that when the problem objective is to maximize some measure of
#' benefit and not minimize some measure of cost, the term \eqn{p} is
#' replaced with \eqn{-p}.
#'
#' @inherit add_boundary_penalties return
#'
#' @seealso
#' See [penalties] for an overview of all functions for adding penalties.
#'
#' @family penalties
#'
#' @references
#' Beger M, Linke S, Watts M, Game E, Treml E, Ball I, and Possingham, HP (2010)
#' Incorporating asymmetric connectivity into spatial decision making for
#' conservation, *Conservation Letters*, 3: 359--368.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(600)
#'
#' # load Matrix package for visualizing matrices
#' require(Matrix)
#'
#' # load data
#' data(sim_pu_polygons, sim_pu_zones_stack, sim_features, sim_features_zones)
#'
#' # define function to rescale values between zero and one so that we
#' # can compare solutions from different connectivity matrices
#' rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
#'   (x - from[1]) / diff(from) * diff(to) + to[1]
#' }
#'
#' # create basic problem
#' p1 <- problem(sim_pu_polygons, sim_features, "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_default_solver(verbose = FALSE)
#'
#' # create an asymmetric connectivity matrix. Here, connectivity occurs between
#' # adjacent planning units and, due to rivers flowing southwards
#' # through the study area, connectivity from northern planning units to
#' # southern planning units is ten times stronger than the reverse.
#' acm1 <- matrix(0, length(sim_pu_polygons), length(sim_pu_polygons))
#' acm1 <- as(acm1, "Matrix")
#' centroids <- rgeos::gCentroid(sim_pu_polygons, byid = TRUE)
#' adjacent_units <- rgeos::gIntersects(sim_pu_polygons, byid = TRUE)
#' for (i in seq_len(length(sim_pu_polygons))) {
#'   for (j in seq_len(length(sim_pu_polygons))) {
#'     # find if planning units are adjacent
#'     if (adjacent_units[i, j]) {
#'       # find if planning units lay north and south of each other
#'       # i.e., they have the same x-coordinate
#'       if (centroids@coords[i, 1] == centroids@coords[j, 1]) {
#'         if (centroids@coords[i, 2] > centroids@coords[j, 2]) {
#'           # if i is north of j add 10 units of connectivity
#'           acm1[i, j] <- acm1[i, j] + 10
#'         } else if (centroids@coords[i, 2] < centroids@coords[j, 2]) {
#'           # if i is south of j add 1 unit of connectivity
#'           acm1[i, j] <- acm1[i, j] + 1
#'         }
#'       }
#'     }
#'   }
#' }
#'
#' # standardize matrix values to lay between zero and one
#' acm1[] <- rescale(acm1[])
#'
#' # visualize asymmetric connectivity matrix
#' \dontrun{
#' image(acm1)
#' }
#'
#' # create penalties
#' penalties <- c(1, 50)
#'
#' # create problems using the different penalties
#' p2 <- list(
#'   p1,
#'   p1 %>% add_asym_connectivity_penalties(penalties[1], data = acm1),
#'   p1 %>% add_asym_connectivity_penalties(penalties[2], data = acm1))
#'
#' # assign names to the problems
#' names(p2) <- c("basic problem", paste0("acm1 (", penalties,")"))
#'
#' \dontrun{
#' # solve problems
#' s2 <- lapply(p2, solve)
#'
#' # plot solutions
#' par(mfrow = c(1, 3))
#' for (i in seq_along(s2)) {
#'   plot(s2[[i]], main = names(p2)[i], cex = 1.5, col = "white")
#'   plot(s2[[i]][s2[[i]]$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' }
#' }
#'
#' # create minimal multi-zone problem and limit solver to one minute
#' # to obtain solutions in a short period of time
#' p3 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(0.15, nrow = 5, ncol = 3)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(time_limit = 60, verbose = FALSE)
#'
#' # crate asymmetric connectivity data by randomly simulating values
#' acm2 <- matrix(runif(ncell(sim_pu_zones_stack) ^ 2),
#'                nrow = ncell(sim_pu_zones_stack))
#'
#' # create multi-zone problems using the penalties
#' p4 <- list(
#'   p3,
#'   p3 %>% add_asym_connectivity_penalties(penalties[1], data = acm2),
#'   p3 %>% add_asym_connectivity_penalties(penalties[2], data = acm2))
#'
#' # assign names to the problems
#' names(p4) <- c("basic problem", paste0("acm2 (", penalties,")"))
#'
#' \dontrun{
#' # solve problems
#' s4 <- lapply(p4, solve)
#' s4 <- lapply(s4, category_layer)
#' s4 <- stack(s4)
#'
#' # plot solutions
#' plot(s4, main = names(p4), axes = FALSE, box = FALSE)
#' }
#'
#' @name add_asym_connectivity_penalties
#'
#' @exportMethod add_asym_connectivity_penalties
#'
#' @aliases add_asym_connectivity_penalties,ConservationProblem,ANY,ANY,Matrix-method add_asym_connectivity_penalties,ConservationProblem,ANY,ANY,matrix-method add_asym_connectivity_penalties,ConservationProblem,ANY,ANY,dgCMatrix-method add_asym_connectivity_penalties,ConservationProblem,ANY,ANY,data.frame-method add_asym_connectivity_penalties,ConservationProblem,ANY,ANY,array-method
NULL

#' @export
methods::setGeneric("add_asym_connectivity_penalties",
  signature = methods::signature("x", "penalty", "zones", "data"),
  function(x, penalty, zones = diag(number_of_zones(x)), data)
    standardGeneric("add_asym_connectivity_penalties"))

#' @name add_asym_connectivity_penalties
#' @usage \S4method{add_asym_connectivity_penalties}{ConservationProblem,ANY,ANY,matrix}(x, penalty, zones, data)
#' @rdname add_asym_connectivity_penalties
methods::setMethod("add_asym_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "matrix"),
  function(x, penalty, zones, data) {
     add_asym_connectivity_penalties(x, penalty, zones,
       methods::as(data, "dgCMatrix"))
})

#' @name add_asym_connectivity_penalties
#' @usage \S4method{add_asym_connectivity_penalties}{ConservationProblem,ANY,ANY,Matrix}(x, penalty, zones, data)
#' @rdname add_asym_connectivity_penalties
methods::setMethod("add_asym_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "Matrix"),
  function(x, penalty, zones, data) {
     add_asym_connectivity_penalties(x, penalty, zones,
       methods::as(data, "dgCMatrix"))
})

#' @name add_asym_connectivity_penalties
#' @usage \S4method{add_asym_connectivity_penalties}{ConservationProblem,ANY,ANY,data.frame}(x, penalty, zones, data)
#' @rdname add_asym_connectivity_penalties
methods::setMethod("add_asym_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "data.frame"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ConservationProblem"), assertthat::is.scalar(penalty),
      is.finite(penalty), is.data.frame(data))
  # add penalties to problem
  add_asym_connectivity_penalties(
    x, penalty, zones,
    marxan_connectivity_data_to_matrix(x, data, symmetric = FALSE))
})

#' @name add_asym_connectivity_penalties
#' @usage \S4method{add_asym_connectivity_penalties}{ConservationProblem,ANY,ANY,dgCMatrix}(x, penalty, zones, data)
#' @rdname add_asym_connectivity_penalties
methods::setMethod("add_asym_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "dgCMatrix"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ConservationProblem"), assertthat::is.scalar(penalty),
      isTRUE(all(is.finite(penalty))), inherits(zones, c("matrix", "Matrix")),
      nrow(zones) == ncol(zones), is.numeric(as.vector(zones)),
      all(is.finite(as.vector(zones))),
      is.numeric(data@x), ncol(data) == nrow(data),
      max(zones) <= 1, min(zones) >= -1,
      number_of_total_units(x) == ncol(data),
      number_of_zones(x) == ncol(zones),
      all(is.finite(data@x)))
    # check for symmetry
    if (Matrix::isSymmetric(data)) {
      warning(
        paste0(
          "argument to data contains symmetric connectivity values, ",
          "it it recommended to use add_connectivity_penalties()"
        ),
        .call = FALSE, .immediate = TRUE
      )
    }
    # coerce zones to matrix
    zones <- as.matrix(zones)
    indices <- x$planning_unit_indices()
    data <- data[indices, indices, drop = FALSE]
    # convert zones & dgCMatrix data to list of sparse matrices
    m <- list()
    for (z1 in seq_len(ncol(zones))) {
      m[[z1]] <- list()
      for (z2 in seq_len(nrow(zones))) {
        m[[z1]][[z2]] <- data * zones[z1, z2]
      }
    }
    # add penalties
    internal_add_asym_connectivity_penalties(x, penalty, m)
})

#' @name add_asym_connectivity_penalties
#' @usage \S4method{add_asym_connectivity_penalties}{ConservationProblem,ANY,ANY,array}(x, penalty, zones, data)
#' @rdname add_asym_connectivity_penalties
methods::setMethod("add_asym_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "array"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      assertthat::is.scalar(penalty), is.finite(penalty), is.null(zones),
      is.array(data), length(dim(data)) == 4,
      dim(data)[1] == number_of_total_units(x),
      dim(data)[2] == number_of_total_units(x),
      dim(data)[3] == number_of_zones(x),
      dim(data)[4] == number_of_zones(x),
      all(is.finite(data)))
    # generate indices for units that are planning units
    indices <- x$planning_unit_indices()
    # convert array to list of list of sparseMatrix objects
    m <- list()
    for (z1 in seq_len(dim(data)[3])) {
      m[[z1]] <- list()
      for (z2 in seq_len(dim(data)[4])) {
        m[[z1]][[z2]] <-
          methods::as(data[indices, indices, z1, z2], "dgCMatrix")
      }
    }
    # add penalties
    internal_add_asym_connectivity_penalties(x, penalty, m)
})

internal_add_asym_connectivity_penalties <- function(x, penalty, data) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    assertthat::is.scalar(penalty), is.finite(penalty),
    is.list(data))
    # create new penalty object
    x$add_penalty(pproto(
      "AsymConnectivityPenalty",
      Penalty,
      name = "Asymmetric connectivity penalties",
      data = list(data = data),
      parameters = parameters(numeric_parameter("penalty", penalty)),
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
                                inherits(y, "ConservationProblem"))
        rcpp_apply_asym_connectivity_penalties(
          x$ptr, self$parameters$get("penalty"), self$get_data("data"))
        invisible(TRUE)
    }))
}
