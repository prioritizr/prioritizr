#' @include internal.R Penalty-class.R marxan_connectivity_data_to_matrix.R
NULL

#' Add asymmetric connectivity penalties
#'
#' Add penalties to a conservation planning problem to account for
#' asymmetric connectivity between planning units.
#' Asymmetric connectivity data describe connectivity information that is
#' directional.
#' For example, asymmetric connectivity data could describe
#' the strength of rivers flowing between different planning units. Since
#' river flow is directional, the level of connectivity
#' from an upstream planning unit to a downstream planning unit would
#' be higher than that from a downstream planning unit to an upstream planning
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
#' replaced with \eqn{-p}. Additionally, to linearize the problem,
#' the expression \eqn{X_{iz} \times X_{jy}}{Xiz * Xjy} is modeled using
#' a set of continuous variables (bounded between 0 and 1) based on
#' Beyer *et al.* (2016).
#'
#' @inherit add_boundary_penalties return
#'
#' @seealso
#' See [penalties] for an overview of all functions for adding penalties.
#' Also see [calibrate_cohon_penalty()] for assistance with selecting
#' an appropriate `penalty` value.
#'
#' @family penalties
#'
#' @references
#' Beger M, Linke S, Watts M, Game E, Treml E, Ball I, and Possingham, HP (2010)
#' Incorporating asymmetric connectivity into spatial decision making for
#' conservation, *Conservation Letters*, 3: 359--368.
#'
#' Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
#' conservation planning problems with integer linear programming.
#' *Ecological Modelling*, 228: 14--22.
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(600)
#'
#' # load data
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create basic problem
#' p1 <-
#'   problem(sim_pu_polygons, sim_features, "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create an asymmetric connectivity matrix. Here, connectivity occurs between
#' # adjacent planning units and, due to rivers flowing southwards
#' # through the study area, connectivity from northern planning units to
#' # southern planning units is ten times stronger than the reverse.
#' acm1 <- matrix(0, nrow(sim_pu_polygons), nrow(sim_pu_polygons))
#' acm1 <- as(acm1, "Matrix")
#' centroids <- sf::st_coordinates(
#'   suppressWarnings(sf::st_centroid(sim_pu_polygons))
#' )
#' adjacent_units <- sf::st_intersects(sim_pu_polygons, sparse = FALSE)
#' for (i in seq_len(nrow(sim_pu_polygons))) {
#'   for (j in seq_len(nrow(sim_pu_polygons))) {
#'     # find if planning units are adjacent
#'     if (adjacent_units[i, j]) {
#'       # find if planning units lay north and south of each other
#'       # i.e., they have the same x-coordinate
#'       if (centroids[i, 1] == centroids[j, 1]) {
#'         if (centroids[i, 2] > centroids[j, 2]) {
#'           # if i is north of j add 10 units of connectivity
#'           acm1[i, j] <- acm1[i, j] + 10
#'         } else if (centroids[i, 2] < centroids[j, 2]) {
#'           # if i is south of j add 1 unit of connectivity
#'           acm1[i, j] <- acm1[i, j] + 1
#'         }
#'       }
#'     }
#'   }
#' }
#'
#' # rescale matrix values to have a maximum value of 1
#' acm1 <- rescale_matrix(acm1, max = 1)
#'
#' # visualize asymmetric connectivity matrix
#' Matrix::image(acm1)
#'
#' # create penalties
#' penalties <- c(1, 50)
#'
#' # create problems using the different penalties
#' p2 <- list(
#'   p1,
#'   p1 %>% add_asym_connectivity_penalties(penalties[1], data = acm1),
#'   p1 %>% add_asym_connectivity_penalties(penalties[2], data = acm1)
#' )
#'
#' # solve problems
#' s2 <- lapply(p2, solve)
#'
#' # create object with all solutions
#' s2 <- sf::st_sf(
#'   tibble::tibble(
#'     p2_1 = s2[[1]]$solution_1,
#'     p2_2 = s2[[2]]$solution_1,
#'     p2_3 = s2[[3]]$solution_1
#'  ),
#'  geometry = sf::st_geometry(s2[[1]])
#' )
#'
#' names(s2)[1:3] <- c("basic problem", paste0("acm1 (", penalties,")"))
#'
#' # plot solutions based on different penalty values
#' plot(s2, cex = 1.5)
#'
#' # create minimal multi-zone problem and limit solver to one minute
#' # to obtain solutions in a short period of time
#' p3 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.15, nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(time_limit = 60, verbose = FALSE)
#'
#' # crate asymmetric connectivity data by randomly simulating values
#' acm2 <- matrix(
#'   runif(ncell(sim_zones_pu_raster) ^ 2),
#'   nrow = ncell(sim_zones_pu_raster)
#' )
#'
#' # create multi-zone problems using the penalties
#' p4 <- list(
#'   p3,
#'   p3 %>% add_asym_connectivity_penalties(penalties[1], data = acm2),
#'   p3 %>% add_asym_connectivity_penalties(penalties[2], data = acm2)
#' )
#'
#' # solve problems
#' s4 <- lapply(p4, solve)
#' s4 <- lapply(s4, category_layer)
#' s4 <- terra::rast(s4)
#' names(s4) <- c("basic problem", paste0("acm2 (", penalties,")"))
#'
#' # plot solutions
#' plot(s4, axes = FALSE)
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
  function(x, penalty, zones = diag(number_of_zones(x)), data) {
    assert_required(x)
    assert_required(penalty)
    assert_required(zones)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      is_inherits(
        data,
        c("dgCMatrix", "data.frame", "matrix", "Matrix", "array")
      )
    )
    standardGeneric("add_asym_connectivity_penalties")
  }
)

#' @name add_asym_connectivity_penalties
#' @usage \S4method{add_asym_connectivity_penalties}{ConservationProblem,ANY,ANY,matrix}(x, penalty, zones, data)
#' @rdname add_asym_connectivity_penalties
methods::setMethod("add_asym_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "matrix"),
  function(x, penalty, zones, data) {
    add_asym_connectivity_penalties(
      x, penalty, zones, as_Matrix(data, "dgCMatrix")
    )
  }
)

#' @name add_asym_connectivity_penalties
#' @usage \S4method{add_asym_connectivity_penalties}{ConservationProblem,ANY,ANY,Matrix}(x, penalty, zones, data)
#' @rdname add_asym_connectivity_penalties
methods::setMethod("add_asym_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "Matrix"),
  function(x, penalty, zones, data) {
    add_asym_connectivity_penalties(
      x, penalty, zones, as_Matrix(data, "dgCMatrix")
    )
  }
)

#' @name add_asym_connectivity_penalties
#' @usage \S4method{add_asym_connectivity_penalties}{ConservationProblem,ANY,ANY,data.frame}(x, penalty, zones, data)
#' @rdname add_asym_connectivity_penalties
methods::setMethod("add_asym_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "data.frame"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      assertthat::is.scalar(penalty),
      all_finite(penalty),
      is.data.frame(data)
    )
    # add penalties to problem
    add_asym_connectivity_penalties(
      x, penalty, zones,
      marxan_connectivity_data_to_matrix(x, data, symmetric = FALSE)
    )
  }
)

#' @name add_asym_connectivity_penalties
#' @usage \S4method{add_asym_connectivity_penalties}{ConservationProblem,ANY,ANY,dgCMatrix}(x, penalty, zones, data)
#' @rdname add_asym_connectivity_penalties
methods::setMethod("add_asym_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "dgCMatrix"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      assertthat::is.number(penalty),
      all_finite(penalty),
      is_inherits(zones, c("matrix", "Matrix")),
      nrow(zones) == ncol(zones),
      is_numeric_values(zones),
      all_finite(zones),
      is_numeric_values(data),
      all_finite(data),
      ncol(data) == nrow(data),
      max(zones) <= 1,
      min(zones) >= -1,
      number_of_total_units(x) == ncol(data),
      number_of_zones(x) == ncol(zones)
    )
    # check for symmetry
    verify(
      !Matrix::isSymmetric(data),
      msg =  paste0(
        "{.arg data} does not contain asymmetric connectivity values, ",
        "use {.fn add_connectivity_penalties} instead."
      )
    )
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
  }
)

#' @name add_asym_connectivity_penalties
#' @usage \S4method{add_asym_connectivity_penalties}{ConservationProblem,ANY,ANY,array}(x, penalty, zones, data)
#' @rdname add_asym_connectivity_penalties
methods::setMethod("add_asym_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "array"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      assertthat::is.number(penalty),
      all_finite(penalty),
      is.null(zones),
      is.array(data),
      length(dim(data)) == 4,
      dim(data)[1] == number_of_total_units(x),
      dim(data)[2] == number_of_total_units(x),
      dim(data)[3] == number_of_zones(x),
      dim(data)[4] == number_of_zones(x),
      all_finite(data)
    )
    # generate indices for units that are planning units
    indices <- x$planning_unit_indices()
    # convert array to list of list of sparseMatrix objects
    m <- list()
    for (z1 in seq_len(dim(data)[3])) {
      m[[z1]] <- list()
      for (z2 in seq_len(dim(data)[4])) {
        m[[z1]][[z2]] <- as_Matrix(data[indices, indices, z1, z2], "dgCMatrix")
      }
    }
    # add penalties
    internal_add_asym_connectivity_penalties(x, penalty, m)
  }
)

internal_add_asym_connectivity_penalties <- function(x, penalty, data) {
  # assert valid arguments
  assert(
    is_conservation_problem(x),
    assertthat::is.number(penalty),
    all_finite(penalty),
    is.list(data),
    .internal = TRUE
  )
  # create new penalty object
  x$add_penalty(
    R6::R6Class(
      "AsymConnectivityPenalty",
      inherit = Penalty,
      public = list(
        name = "asymmetric connectivity penalties",
        data = list(penalty = penalty, data = data),
        apply = function(x, y) {
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          rcpp_apply_asym_connectivity_penalties(
            x$ptr, self$get_data("penalty"), self$get_data("data")
          )
          invisible(TRUE)
        }
      )
    )$new()
  )
}
