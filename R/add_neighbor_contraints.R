#' @include internal.R Constraint-proto.R marxan_boundary_data_to_matrix.R
NULL

#' Add neighbor constraints
#'
#' Add constraints to a conservation planning \code{\link{problem}} to ensure
#' that all selected planning units in the solution have at least a certain
#' number of neighbors that are also selected in the solution.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param k \code{integer} minimum number of neighbors for selected
#'   planning units in the solution. For problems with multiple zones,
#'   the argument to \code{k} must have an element for each zone.
#'
#' @param zones \code{matrix} or \code{Matrix} object describing the
#'   neighborhood scheme for different zones. Each row and column corresponds
#'   to a different zone in the argument to \code{x}, and cell values must
#'   contain binary \code{numeric} values (i.e. one or zero) that indicate
#'   if neighboring planning units (as specified in the argument to
#'   \code{data}) should be considered neighbors if they are allocated to
#'   different zones. The cell values along the diagonal
#'   of the matrix indicate if planning units that are allocated to the same
#'   zone should be considered neighbors or not. The default argument to
#'   \code{zones} is an identity matrix (i.e. a matrix with ones along the
#'   matrix diagonal and zeros elsewhere), so that planning units are
#'   only considered neighbors if they are both allocated to the same zone.
#'
#' @param data \code{NULL}, \code{matrix}, \code{Matrix}, \code{data.frame}, or
#'   \code{array} object showing which planning units are neighbors with each
#'   other. The argument defaults to \code{NULL} which means that the
#'   neighborhood data is calculated automatically using the
#'   \code{\link{connected_matrix}} function. See the Details section for more
#'   information.
#'
#' @details This function uses neighborhood data identify solutions that
#'   surround planning units with a minimum number of neighbors. It
#'   was inspired by the mathematical formulations detailed in
#'   Billionnet (2013) and Beyer \emph{et al.} (2016).
#'
#'   The argument to \code{data} can be specified in several ways:
#'
#'   \describe{
#'
#'   \item{\code{NULL}}{neighborhood data should be calculated automatically
#'     using the \code{\link{connected_matrix}} function. This is the default
#'     argument. Note that the neighborhood data must be manually defined
#'     using one of the other formats below when the planning unit data
#'     in the argument to \code{x} is not spatially referenced (e.g.
#'     in \code{data.frame} or \code{numeric} format).}
#'
#'   \item{\code{matrix}, \code{Matrix}}{where rows and columns represent
#'     different planning units and the value of each cell indicates if the
#'     two planning units are neighbors or not. Cell values should be binary
#'     \code{numeric} values (i.e. one or zero). Cells that occur along the
#'     matrix diagonal have no effect on the solution at all because each
#'     planning unit cannot be a neighbor with itself.}
#'
#'   \item{\code{data.frame}}{containing the fields (columns)
#'     \code{"id1"}, \code{"id2"}, and \code{"boundary"}. Here, each row
#'     denotes the connectivity between two planning units following the
#'     \emph{Marxan} format. The field \code{boundary} should contain
#'     binary \code{numeric} values that indicate if the two planning units
#'     specified in the fields \code{"id1"} and \code{"id2"} are neighbors
#'     or not. This data can be used to describe symmetric or
#'     asymmetric relationships between planning units. By default,
#'     input data is assumed to be symmetric unless asymmetric data is
#'     also included (e.g. if data is present for planning units 2 and 3, then
#'     the same amount of connectivity is expected for planning units 3 and 2,
#'     unless connectivity data is also provided for planning units 3 and 2).
#'     If the argument to \code{x} contains multiple zones, then the columns
#'     \code{"zone1"} and \code{"zone2"} can optionally be provided to manually
#'     specify if the neighborhood data pertain to specific zones. The fields
#'     \code{"zone1"} and \code{"zone2"} should contain the \code{character}
#'     names of the zones. If the columns \code{"zone1"} and \code{"zone2"}
#'     are present, then the argument to \code{zones} must be \code{NULL}.}
#'
#'   \item{\code{array}}{containing four-dimensions where binary
#'     \code{numeric} values indicate if planning unit should be treated
#'     as being neighbors with every other planning unit when they
#'     are allocated to every combination of management zone. The first two
#'     dimensions (i.e. rows and columns) correspond to the planning units,
#'     and second two dimensions correspond to the management zones. For
#'     example, if the argument to \code{data} had a value of 1 at the index
#'     \code{data[1, 2, 3, 4]} this would indicate that planning unit 1 and
#'     planning unit 2 should be treated as neighbors when they are
#'     allocated to zones 3 and 4 respectively.}
#'
#'   }
#'
#' @return \code{\link{ConservationProblem-class}} object with the constraint
#'   added to it.
#'
#' @seealso \code{\link{constraints}}, \code{\link{penalties}}.
#'
#' @references
#' Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
#' conservation planning problems with integer linear programming.
#' \emph{Ecological Modelling}, 228: 14--22.
#'
#' Billionnet A (2013) Mathematical optimization ideas for biodiversity
#' conservation. \emph{European Journal of Operational Research}, 231:
#' 514--534.
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#'
#' # create minimal problem
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1)
#'
#' # create problem with constraints that require 1 neighbor
#' # and neighbors are defined using a rook-style neighborhood
#' p2 <- p1 %>% add_neighbor_constraints(1)
#'
#' # create problem with constraints that require 2 neighbor
#' # and neighbors are defined using a rook-style neighborhood
#' p3 <- p1 %>% add_neighbor_constraints(2)
#'
#' # create problem with constraints that require 3 neighbor
#' # and neighbors are defined using a queen-style neighborhood
#' p4 <- p1 %>% add_neighbor_constraints(3,
#'                data = connected_matrix(sim_pu_raster, directions = 8))
#'
#' \donttest{
#' # solve problems
#' s1 <- stack(list(solve(p1), solve(p2), solve(p3), solve(p4)))
#'
#' # plot solutions
#' plot(s1, box = FALSE, axes = FALSE,
#'      main = c("basic solution", "1 neighbor", "2 neighbors", "3 neighbors"))
#' }
#' # create minimal problem with multiple zones
#' p5 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(0.1, ncol = 3, nrow = 5))
#'
#' # create problem where selected planning units require at least 2 neighbors
#' # for each zone and planning units are only considered neighbors if they
#' # are allocated to the same zone
#' z6 <- diag(3)
#' print(z6)
#' p6 <- p5 %>% add_neighbor_constraints(rep(2, 3), z6)
#'
#' # create problem where the planning units in zone 1 don't explicitly require
#' # any neighbors, planning units in zone 2 require at least 1 neighbors, and
#' # planning units in zone 3 require at least 2 neighbors. As before, planning
#' # units are still only considered neighbors if they are allocated to the
#' # same zone
#' p7 <- p5 %>% add_neighbor_constraints(c(0, 1, 2), z6)
#'
#' # create problem given the same constraints as outlined above, except
#' # that when determining which selected planning units are neighbors,
#' # planning units that are allocated to zone 1 and zone 2 can also treated
#' # as being neighbors with each other
#' z8 <- diag(3)
#' z8[1, 2] <- 1
#' z8[2, 1] <- 1
#' print(z8)
#' p8 <- p5 %>% add_neighbor_constraints(c(0, 1, 2), z8)
#' \donttest{
#' # solve problems
#' s2 <- list(p5, p6, p7, p8)
#' s2 <- lapply(s2, solve)
#' s2 <- lapply(s2, category_layer)
#' s2 <- stack(s2)
#' names(s2) <- c("basic problem", "p6", "p7", "p8")
#'
#' # plot solutions
#' plot(s2, main = names(s2), box = FALSE, axes = FALSE)
#' }
#' @name add_neighbor_constraints
#'
#' @exportMethod add_neighbor_constraints
#'
#' @aliases add_neighbor_constraints,ConservationProblem,ANY,ANY,array-method add_neighbor_constraints,ConservationProblem,ANY,ANY,matrix-method add_neighbor_constraints,ConservationProblem,ANY,ANY,data.frame-method add_neighbor_constraints,ConservationProblem,ANY,ANY,ANY-method
NULL

#' @export
methods::setGeneric("add_neighbor_constraints",
  signature = methods::signature("x", "k", "zones", "data"),
  function(x, k, zones = diag(number_of_zones(x)), data = NULL)
  standardGeneric("add_neighbor_constraints"))

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,ANY,ANY,ANY}(x, k, zones, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY"),
  function(x, k, zones, data) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
     all(is.finite(k)), is.numeric(k), all(k == round(k), na.rm = TRUE),
     all(k >= 0), length(k) == number_of_zones(x),
     inherits(zones, c("matrix", "Matrix")),
     inherits(data, c("NULL", "Matrix")))
    if (!is.null(data)) {
      # check argument to data if not NULL
      assertthat::assert_that(all(as.vector(data) %in% c(0, 1, NA)),
        ncol(data) == nrow(data), number_of_total_units(x) == ncol(data),
        sum(!is.finite(as.vector(data))) == 0)
      d <- list(connected_matrix = data)
    } else {
      # check that planning unit data is spatially referenced
      assertthat::assert_that(inherits(x$data$cost, c("Spatial", "Raster")),
        msg = paste("argument to data must be supplied because planning unit",
                    "data are not in a spatially referenced format"))
      d <- list()
    }
    # convert zones to matrix
    zones <- as.matrix(zones)
    assertthat::assert_that(
      isSymmetric(zones), ncol(zones) == number_of_zones(x),
      is.numeric(zones), all(zones %in% c(0, 1)))
    colnames(zones) <- x$zone_names()
    rownames(zones) <- colnames(zones)
    # create parameter
    if (length(k) > 1) {
      p <- integer_parameter_array("number of neighbors", as.integer(k),
                                   x$zone_names(),
                                   lower_limit = rep(0L, length(k)))
    } else {
      p <- integer_parameter("number of neighbors", as.integer(k),
                             lower_limit = 0L)
    }
    # add constraints
    x$add_constraint(pproto(
      "NeighborConstraint",
      Constraint,
      data = d,
      name = "Neighbor constraint",
      parameters = parameters(p,
        binary_matrix_parameter("zones", zones, symmetric = FALSE)),
      calculate = function(self, x) {
        assertthat::assert_that(inherits(x, "ConservationProblem"))
        # generate connected matrix if null
        if (is.Waiver(self$get_data("connected_matrix"))) {
          # create matrix
          data <- connected_matrix(x$data$cost)
          # coerce matrix to full matrix
          data <- methods::as(data, "dgCMatrix")
          # store data
          self$set_data("connected_matrix", data)
        }
        # return success
        invisible(TRUE)
      },
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
          inherits(y, "ConservationProblem"))
        k <- self$parameters$get("number of neighbors")[[1]]
        if (any(k > 0)) {
          # extract data and parameters
          ind <- y$planning_unit_indices()
          d <- self$get_data("connected_matrix")[ind, ind]
          z <- self$parameters$get("zones")
          # generate list of sparse matrix objects
          m <- list()
          for (z1 in seq_len(ncol(z))) {
            m[[z1]] <- list()
            for (z2 in seq_len(nrow(z))) {
              m[[z1]][[z2]] <- methods::as(d * z[z1, z2], "dgCMatrix")
            }
          }
          # apply constraints
          rcpp_apply_neighbor_constraints(x$ptr, m, k)
        }
        invisible(TRUE)
      }))
})

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,ANY,ANY,data.frame}(x, k, zones, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "data.frame"),
  function(x, k, zones, data) {
    # add constraints
    add_neighbor_constraints(x, k, zones,
                             marxan_boundary_data_to_matrix(x, data))
})

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,ANY,ANY,matrix}(x, k, zones, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "matrix"),
  function(x, k, zones, data) {
    # add constraints
    add_neighbor_constraints(x, k, zones, methods::as(data, "dgCMatrix"))
})

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,ANY,ANY,array}(x, k, zones, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "array"),
  function(x, k, zones, data) {
    # assert arguments are valid
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(data, "array"),
      is.null(zones),
      all(is.finite(k)), is.numeric(k), all(k >= 0),
      all(k == round(k), na.rm = TRUE), length(k) == number_of_zones(x),
      dim(data)[1] == x$number_of_total_units(),
      dim(data)[2] == x$number_of_total_units(),
      dim(data)[3] == x$number_of_zones(),
      dim(data)[4] == x$number_of_zones(),
      sum(is.finite(as.vector(data))) > 0,
      all(as.vector(data) %in% c(0, 1)))
    # create parameter
    if (length(k) > 1) {
      p <- integer_parameter_array("number of neighbors", as.integer(k),
                                   x$zone_names(),
                                   lower_limit = rep(0, length(k)))
    } else {
      p <- integer_parameter("number of neighbors", as.integer(k),
                             lower_limit = 0)
    }
    # convert connected matrix to list of sparse matrices
    indices <- x$planning_unit_indices()
    m <- list()
    for (z1 in seq_len(dim(data)[3])) {
      m[[z1]] <- list()
      for (z2 in seq_len(dim(data)[4])) {
        m[[z1]][[z2]] <- methods::as(data[indices, indices, z1, z2],
                                     "dgCMatrix")
      }
    }
    # add the constraint
    x$add_constraint(pproto(
      "NeighborConstraint",
      Constraint,
      data = list(connected_matrix_list = m),
      name = "Neighbor constraints",
      parameters = parameters(p),
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
          inherits(y, "ConservationProblem"))
        k <- self$parameters$get("number of neighbors")[[1]]
        if (any(k > 0))
          rcpp_apply_neighbor_constraints(x$ptr,
            self$get_data("connected_matrix_list"), k)
        invisible(TRUE)
      }))
})
