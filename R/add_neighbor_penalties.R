#' @include internal.R Penalty-class.R marxan_connectivity_data_to_matrix.R
NULL

#' Add neighbor penalties
#'
#' Add penalties to a conservation planning problem to penalize solutions
#' that have few neighboring planning units. These penalties can be used
#' to promote spatial clustering in solutions. In particular, they are
#' recommended for reducing spatial fragmentation in large-scale problems
#' or when using open source solvers.
#'
#' @inheritParams add_neighbor_constraints
#'
#' @inheritParams add_connectivity_penalties
#'
#' @details
#' This function adds penalties to conservation planning problem to penalize
#' solutions that have low spatial clustering.
#' Specifically, it favors pair-wise connections between planning units
#' that have high connectivity values (based on Önal and Briers 2002).
#'
#' @inheritSection add_neighbor_constraints Data format
#'
#' @section Mathematical formulation:
#' The neighbor penalties are implemented using the following equations.
#' Let \eqn{I} represent the set of planning units
#' (indexed by \eqn{i} or \eqn{j}), \eqn{Z} represent the set
#' of management zones (indexed by \eqn{z} or \eqn{y}), and \eqn{X_{iz}}{Xiz}
#' represent the decision variable for planning unit \eqn{i} for in zone
#' \eqn{z} (e.g., with binary
#' values one indicating if planning unit is allocated or not). Also, let
#' \eqn{p} represent the argument to `penalty`, \eqn{D} represent the
#' argument to `data` , and \eqn{W} represent the argument
#' to `zones`.
#'
#' If the argument to `data` is supplied as a `matrix` or
#' `Matrix` object, then the penalties are calculated as:
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z} \sum_{y}^{Z} (-p \times X_{iz}
#' \times X_{jy} \times D_{ij} \times W_{zy})}{
#' sum_i^I sum_j^I sum_z^Z sum_y^Z (-p * Xiz * Xjy * Dij * Wzy)
#' }
#'
#' Otherwise, if the argument to `data` is supplied as a
#' `data.frame` or `array` object, then the penalties are
#' calculated as:
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z} \sum_{y}^{Z} (-p \times X_{iz}
#' \times X_{jy} \times D_{ijzy})}{
#' sum_i^I sum_j^I sum_z^Z sum_y^Z (-p * Xiz * Xjy * Dijzy)
#' }
#'
#' Note that when the problem objective is to maximize some measure of
#' benefit and not minimize some measure of cost, the term \eqn{-p} is
#' replaced with \eqn{p}.
#'
#' @inherit add_linear_penalties return seealso
#'
#' @family penalties
#'
#' @encoding UTF-8
#'
#' @references
#' Williams JC, ReVelle CS, and Levin SA (2005) Spatial attributes and reserve
#' design models: A review. *Environmental Modeling and Assessment*, 10:
#' 163--181.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create minimal problem
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with low neighbor penalties and
#' # using a rook-style neighborhood (the default neighborhood style)
#' p2 <- p1 %>% add_neighbor_constraints(0.001)
#'
#' # create problem with high penalties
#' # using a rook-style neighborhood (the default neighborhood style)
#' p3 <- p1 %>% add_neighbor_constraints(0.01)
#'
#' # create problem with high penalties and using a queen-style neighborhood
#' p4 <-
#'   p1 %>%
#'   add_neighbor_constraints(
#'     0.01, data = adjacency_matrix(sim_pu_raster, directions = 8)
#'   )
#'
#' # solve problems
#' s1 <- terra::rast(list(solve(p1), solve(p2), solve(p3), solve(p4)))
#' names(s1) <- c("basic solution", "low (rook)", "high (rook)", "high (queen")
#'
#' # plot solutions
#' plot(s1, axes = FALSE)
#'
#' # create minimal problem with multiple zones
#' p5 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with low neighbor penalties, a rook style neighborhood,
#' # and planning units are only considered neighbors if they are allocated to
#' # the same zone
#' z6 <- diag(3)
#' print(z6)
#' p6 <- p5 %>% add_neighbor_penalties(0.001, zones = z6)
#'
#' # create problem with high penalties and the same neighborhood as above
#' p7 <- p5 %>% add_neighbor_constraints(0.01, zones = z6)
#'
#' # create problem with high neighborhood penalties, a queen-style
#' # neighborhood, neighboring planning units that are allocated to zones 1
#' # or 2 are treated as neighbors
#' z8 <- diag(3)
#' z8[1, 2] <- 1
#' z8[2, 1] <- 1
#' print(z8)
#' p8 <- p5 %>% add_neighbor_constraints(0.01, zones = z8)
#'
#' # create problem with high neighborhood penalties, a queen-style
#' # neighborhood, and here we want to promote spatial fragmentation
#' # within each zone, so we use negative zone values.
#' z9 <- diag(3) * -1
#' print(z9)
#' p9 <- p5 %>% add_neighbor_constraints(0.01, zones = z9)
#'
#' # solve problems
#' s2 <- list(p5, p6, p7, p8, p9)
#' s2 <- lapply(s2, solve)
#' s2 <- lapply(s2, category_layer)
#' s2 <- terra::rast(s2)
#' names(s2) <- c("basic problem", "p6", "p7", "p8", "p9")
#'
#' # plot solutions
#' plot(s2, main = names(s2), axes = FALSE)
#' }
#' @name add_neighbor_penalties
#'
#' @exportMethod add_neighbor_penalties
#'
#' @aliases add_neighbor_penalties,ConservationProblem,ANY,ANY,Matrix-method add_neighbor_penalties,ConservationProblem,ANY,ANY,matrix-method add_neighbor_penalties,ConservationProblem,ANY,ANY,ANY-method add_neighbor_penalties,ConservationProblem,ANY,ANY,data.frame-method add_neighbor_penalties,ConservationProblem,ANY,ANY,array-method
NULL

#' @export
methods::setGeneric("add_neighbor_penalties",
  signature = methods::signature("x", "penalty", "zones", "data"),
  function(x, penalty, zones = diag(number_of_zones(x)), data = NULL) {
    assert_required(x)
    assert_required(penalty)
    assert_required(zones)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      is_inherits(
        data,
        c("NULL", "dgCMatrix", "data.frame", "matrix", "Matrix", "array")
      )
    )
    standardGeneric("add_neighbor_penalties")
  }
)

#' @name add_neighbor_penalties
#' @usage \S4method{add_neighbor_penalties}{ConservationProblem,ANY,ANY,matrix}(x, penalty, zones, data)
#' @rdname add_neighbor_penalties
methods::setMethod("add_neighbor_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "matrix"),
  function(x, penalty, zones, data) {
    add_neighbor_penalties(
      x, penalty, zones, as_Matrix(data, "dgCMatrix")
    )
  }
)

#' @name add_neighbor_penalties
#' @usage \S4method{add_neighbor_penalties}{ConservationProblem,ANY,ANY,data.frame}(x, penalty, zones, data)
#' @rdname add_neighbor_penalties
methods::setMethod("add_neighbor_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "data.frame"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      assertthat::is.number(penalty),
      all_finite(penalty),
      is.data.frame(data)
    )
    # add penalties to problem
    add_neighbor_penalties(
      x, penalty, zones,
      marxan_connectivity_data_to_matrix(x, data, symmetric = TRUE)
    )
  }
)

#' @name add_neighbor_penalties
#' @usage \S4method{add_neighbor_penalties}{ConservationProblem,ANY,ANY,ANY}(x, penalty, zones, data)
#' @rdname add_neighbor_penalties
methods::setMethod("add_neighbor_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      assertthat::is.number(penalty),
      all_finite(penalty),
      is_matrix_ish(zones),
      number_of_zones(x) == ncol(zones),
      is_inherits(data, c("NULL", "Matrix"))
    )
    if (!is.null(data)) {
      # check argument to data if not NULL
      assert(
        ncol(data) == nrow(data),
        number_of_total_units(x) == ncol(data),
        all_binary(data)
      )
      # check for symmetry
      assert(
        Matrix::isSymmetric(data),
        msg = paste0(
          "{.arg data} must have symmetric values."
        )
      )
    } else {
      # check that planning unit data is spatially referenced
      assert(
        is_pu_spatially_explicit(x),
        msg = c(
          "!" = paste(
            "{.arg data} must be manually specified (e.g., as a {.cls Matrix})."
          ),
          "i" = paste(
            "This is because {.arg x} has planning unit data that are not",
            "spatially explicit",
            "(e.g., {.cls sf}, or {.cls SpatRaster} objects)."
          )
        )
      )
    }
    # add penalties
    internal_add_neighbor_penalties(x, penalty, as.matrix(zones), data)
  }
)

#' @name add_neighbor_penalties
#' @usage \S4method{add_neighbor_penalties}{ConservationProblem,ANY,ANY,array}(x, penalty, zones, data)
#' @rdname add_neighbor_penalties
methods::setMethod("add_neighbor_penalties",
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
    # add penalties
    internal_add_neighbor_penalties(x, penalty, zones, data)
  }
)

internal_add_neighbor_penalties <- function(x, penalty, zones, data) {
  # assert valid arguments
  assert(
    is_conservation_problem(x),
    assertthat::is.number(penalty),
    all_finite(penalty),
    .internal = TRUE
  )
  # convert zones to matrix
  if (!is.null(zones)) {
    zones <- as.matrix(zones)
    assert(
      is_numeric_values(zones),
      all_finite(zones),
      isSymmetric(zones),
      nrow(zones) == ncol(zones),
      ncol(zones) == number_of_zones(x),
      min(zones) >= -1,
      max(zones) <= 1,
      call = fn_caller_env()
    )
    colnames(zones) <- x$zone_names()
    rownames(zones) <- colnames(zones)
  }
  # create new penalty object
  x$add_penalty(
    R6::R6Class(
      "NeighborPenalty",
      inherit = Penalty,
      public = list(
        name = "neighbor penalties",
        data = list(penalty = penalty, zones = zones, data = data),
        calculate = function(x) {
          assert(is_conservation_problem(x))
          # if needed, generate adjacency matrix if null
          if (
            is.null(self$get_data("data")) &&
            is.Waiver(x$get_data("adjacency"))
          ) {
            x$set_data("adjacency", adjacency_matrix(x$data$cost))
          }
          # return success
          invisible(TRUE)
        },
        apply = function(x, y) {
          # assert valid arguments
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # extract data
          d <- self$get_data("data")
          if (is.null(d)) {
            d <- y$get_data("adjacency")
          }
          indices <- y$planning_unit_indices()
          # process data
          m <- list()
          if (inherits(d, "Matrix")) {
            ## if data is a Matrix...
            d <- d[indices, indices, drop = FALSE]
            z <- self$get_data("zones")
            for (z1 in seq_len(ncol(z))) {
              m[[z1]] <- list()
              for (z2 in seq_len(nrow(z))) {
                m[[z1]][[z2]] <- d * z[z1, z2]
              }
            }
          } else if (inherits(d, "array")) {
            ## if data is an array...
            for (z1 in seq_len(dim(d)[3])) {
              m[[z1]] <- list()
              for (z2 in seq_len(dim(d)[4])) {
                m[[z1]][[z2]] <- as_Matrix(
                  d[indices, indices, z1, z2],
                  "dgCMatrix"
                )
              }
            }
          } else {
            ## throw error if not recognized
            # nocov start
            cli::cli_abort(
              "Failed calculations for {.fn add_neighbor_penalties}.",
              .internal = TRUE
            )
            # nocov end
          }
          # coerce to symmetric connectivity data
          m <- lapply(m, function(x) {
            lapply(x, function(y) as_Matrix(Matrix::tril(y), "dgCMatrix"))
          })
          # apply penalties
          rcpp_apply_connectivity_penalties(x$ptr, self$get_data("penalty"), m)
          invisible(TRUE)
        }
      )
    )$new()
  )
}
