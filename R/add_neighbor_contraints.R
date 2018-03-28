#' @include internal.R Constraint-proto.R
NULL

#' Add neighbor constraints
#'
#' Add constraints to a conservation problem to ensure that all selected
#' planning units have at least a certain number of neighbors in the solution.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param k \code{integer} minimum number of neighbors required for
#'   planning units selected in the solution. For problems with multiple zones,
#'   the argument to \code{k} must have an element for each zone.
#'
#' @param data A \code{NULL}, \code{matrix}, \code{array}, or \code{data.frame}
#'   object showing which planning units are neighbors with each
#'   other. The argument defaults to \code{NULL} which means that the
#'   neighborhood data is calculated automatically using the
#'   \code{\link{connected_matrix}} function. See the Details section for more
#'   information.
#'
#' @details This function uses neighborhood data identify solutions that select
#'   planning units with a minimum number of neighbors in the solution. It
#'   was inspired by one of the mathematical formulations detailed in
#'   Billionnet (2013). The argument to \code{data} can be specified in several
#'   ways.
#'
#'   \describe{
#'
#'   \item{\code{NULL}}{neighborhood data should be calculated automatically.
#'     This is the default. Note that neighborhood data must be supplied
#'     when the argument to \code{x} contains multiple zones.}
#'
#'   \item{\code{matrix}, \code{Matrix}}{where rows and columns represent
#'     different planning units and the value of each cell indicates if the
#'     two planning units are neighbors or not. Cells that occur along the
#'     diagonal have no effect on the solution because each planning unit
#'     cannot be a neighbor with itself. Note that \code{matrix} objects cannot
#'     be supplied as an argument for \code{data} when the argument to \code{x}
#'     contains multiple zones, and instead an \code{array} object must be
#'     supplied.}
#'
#'   \item{\code{data.frame}}{containing the fields (columns)
#'     \code{"id1"}, \code{"id2"}, and \code{"boundary"} (following the
#'     \emph{Marxan} format). Here, each row denotes the neighborhood
#'     relationship between two planning units, the \code{"id1"}, and
#'     \code{"id2"} columns contain the integer planning unit identifiers,
#'     and the code{"boundary"} column contains \code{TRUE} or \code{FALSE}
#'     values indicating if the planning units are neighbors or not. Note that
#'     relationships between planning units that are \code{FALSE} do not
#'     need to be supplied, and any missing combinations of planning units
#'     are assumed to not be neighbors. If the argument to \code{x} contains
#'     multiple zones, then \code{"zone1"} and \code{"zone2"} columns are
#'     also required to indicate the names of zones that neighborhood
#'     relationships pertain too (e.g. this could be used to specify that
#'     planning units are not counted as neighbors when they are allocated
#'     to different zones).}
#'
#'   \item{\code{array}}{containing four-dimensions where cell values
#'     indicate the strength of connectivity between planning units
#'     when they are assigned to specific management zones. The first two
#'     dimensions (i.e. rows and columns) indicate the strength of
#'     connectivity between different planning units and the second two
#'     dimensions indicate the different management zones. Thus
#'     the \code{data[1, 2, 3, 4]} indicates that planning unit 1 and planning
#'     unit 2 when should be treated as neighbors when they are
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
#' Billionnet A (2013) Mathematical optimization ideas for biodiversity
#' conservation. \emph{European Journal of Operational Research}, 231:
#' 514--534.
#'
#' @examples
#' # # create basic problem
#' # p1 <- problem(sim_pu_polygons, sim_features, "cost") %>%
#' #       add_min_set_objective() %>%
#' #       add_relative_targets(0.2)
#' #
#' # # create problem with constraints that require 1 neighbor
#' # p2 <- p1 %>% add_neighbor_constraints(1)
#' #
#' # # create problem with constraints that require 2 neighbors
#' # p3 <- p1 %>% add_neighbor_constraints(2)
#' #
#' # # create problem with constraints that require 3 neighbors
#' # p4 <- p1 %>% add_neighbor_constraints(3)
#' #
#' # \donttest{
#' # # solve problems
#' # s <- list(solve(p1), solve(p2), solve(p3), solve(p4))
#' #
#' # # plot solutions
#' # par(mfrow = c(2,2), mar = c(0, 0, 4.1, 0))
#' #
#' # plot(s[[1]], main = "basic solution")
#' # plot(s[[1]][s[[1]]$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' #
#' # plot(s[[2]], main="1 neighbor")
#' # plot(s[[2]][s[[2]]$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' #
#' # plot(s[[3]], main="2 neighbors")
#' # plot(s[[3]][s[[3]]$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' #
#' # plot(s[[4]], main="3 neighbors")
#' # plot(s[[4]][s[[4]]$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#' # }
#' #
#' @name add_neighbor_constraints
#'
#' @exportMethod add_neighbor_constraints
#'
#' @aliases add_neighbor_constraints,ConservationProblem,numeric,Matrix-method add_neighbor_constraints,ConservationProblem,numeric,matrix-method add_neighbor_constraints,ConservationProblem,numeric,data.frame-method add_neighbor_constraints,ConservationProblem,numeric,ANY-method
NULL

#' @export
methods::setGeneric("add_neighbor_constraints",
                    signature = methods::signature("x", "k", "data"),
                    function(x, k, data = NULL)
                      standardGeneric("add_neighbor_constraints"))

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,numeric,Matrix}(x, k, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "numeric", "Matrix"),
  function(x, k, data) {
    # add constraints
    add_neighbor_constraints(x, k, as.matrix(data))
})

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,numeric,matrix}(x, k, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "numeric", "matrix"),
  function(x, k, data) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
                            all(data %in% c(0, 1, NA)),
                            ncol(data) == nrow(data),
                            number_of_total_units(x) == ncol(data),
                            sum(is.finite(data)) > 0)
    assertthat::assert_that(number_of_zones(x) == 1,
                           msg = paste("argument to data must",
                                       "be an array for problems with",
                                       "multiple zones"))
    # add constraints
    add_neighbor_constraints(x, k, array(c(data), dim = c(nrow(data),
                                                          ncol(data), 1, 1)))
})

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,numeric,data.frame}(x, k, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "numeric", "data.frame"),
  function(x, k, data) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
                            is.data.frame(data),
                            assertthat::has_name(data, "boundary"),
                            all(data$boundary %in% c(0, 1)))
    # assert valid arguments
    add_neighbor_constraints(x, k, dataframe_to_array(data, x))
})

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,numeric,ANY}(x, k, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "numeric", "ANY"),
  function(x, k, data) {
    # assert arguments are valid
    assertthat::assert_that(inherits(x, "ConservationProblem"),
                            is.numeric(k),
                            all(is.finite(k)),
                            all(k == round(k), na.rm = TRUE),
                            length(k) == x$number_of_zones(),
                            inherits(data, c("NULL", "array")))
   # verify data argument if array
   if (inherits(data, "array")) {
     assertthat::assert_that(dim(data)[1] == x$number_of_total_units(),
                             dim(data)[2] == x$number_of_total_units(),
                             dim(data)[3] == x$number_of_zones(),
                             dim(data)[4] == x$number_of_zones(),
                             sum(is.finite(data)) > 0,
                             all(data %in% c(0, 1, NA)))
     d <- list(connected_matrix = data)
  } else {
    assertthat::assert_that(inherits(x$data$cost, c("Spatial", "Raster")),
      msg = paste("argument to data be supplied to denote neighborhood",
                  "data when planning units are not spatially referenced"))
    assertthat::assert_that(x$number_of_zones() == 1,
                           msg = paste("argument to data must",
                                       "be an array for problems with",
                                       "multiple zones"))
    d <- list()
  }
  # create parameter
  p <- integer_parameter_array("number of neighbors", as.integer(k),
                              x$zone_names(), lower_limit = rep(0L, length(k)))
  # add the constraint
  x$add_constraint(pproto(
    "NeighborConstraint",
    Constraint,
    data = d,
    name = "Neighbor constraint",
    parameters = parameters(p),
    calculate = function(self, x) {
      assertthat::assert_that(inherits(x, "ConservationProblem"))
      # calculate connected matrix
      c_data <- self$get_data("connected_matrix")
      if (is.Waiver(c_data)) {
        c_data <- array(c(as.matrix(connected_matrix(x$data$cost))),
                        dim = c(rep(x$number_of_planning_units(), 2), 1, 1))
      }
      # convert connected matrix to list of sparse matrices
      indices <- x$planning_unit_indices()
      m <- list()
      for (z1 in seq_len(dim(c_data)[3])) {
        m[[z1]] <- list()
        for (z2 in seq_len(dim(c_data)[4])) {
          m[[z1]][[z2]] <- methods::as(c_data[indices, indices, z1, z2],
                                       "dsCMatrix")
          class(m[[z1]][[z2]]) <- "dgCMatrix"
        }
      }
      self$set_data("connected_matrix_list", m)
      invisible(TRUE)
    },
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
