#' @include internal.R Penalty-proto.R marxan_boundary_data_to_matrix.R
NULL

#' Add connectivity penalties
#'
#' Add penalties to a conservation planning \code{\link{problem}} to favor
#' solutions that select planning units with high connectivity between them.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param penalty \code{numeric} penalty that is used to scale the importance
#'   of selecting planning units with strong connectivity between them compared
#'   to the main problem objective (e.g. solution cost when the argument to
#'   \code{x} has a minimum set objective set using
#'   \code{\link{add_min_set_objective}}). Higher \code{penalty} values
#'   can be used to obtain solutions with a high degree of connectivity,
#'   and smaller \code{penalty} values can be used to obtain solutions with a #'   small degree of connectivity. Note that negative \code{penalty} values can
#'   be used to obtain solutions that have very little connectivity.
#'
#' @param zones \code{matrix} or \code{Matrix} object describing the
#'   level of connectivity between different zones. Each row and column
#'   corresponds to a different zone in the argument to \code{x}, and cell
#'   values indicate the level of connectivity between each combination
#'   of zones. Cell values along the diagonal of the matrix represent
#'   the level of connectivity between planning units allocated to the
#'   same zone. Cell values must lay between 1 and -1, where negative
#'   values favor solutions with weak connectivity. The default argument to
#'   \code{zones} is an identity matrix (i.e. a matrix with ones along the
#'   matrix diagonal and zeros elsewhere), so that planning units are
#'   only considered to be connected when they are allocated to the same zone.
#'   This argument is required when the argument to \code{data} is a
#'   \code{matrix} or \code{Matrix} object. If the argument to \code{data} is
#'   an \code{array} or \code{data.frame} with zone data, this argument
#'   must explicitly be set to \code{NULL} otherwise an error will be thrown.
#'
#' @param data \code{matrix}, \code{Matrix}, \code{data.frame}, or
#'   \code{array} object containing connectivity data. The connectivity values
#'   correspond to the strength of connectivity between
#'   different planning units. Thus connections between planning units
#'   that are associated with higher values are more favorable in the solution.
#'   See the Details section for more information.
#'
#' @param ... not used.
#'
#' @details This function uses connectivity data to penalize solutions
#'   that have low connectivity. It can accommodate symmetric and asymmetric
#'   relationships between planning units. Although \emph{Marxan}
#'   \strong{penalizes} connections between planning units with high
#'   connectivity values, it is important to note that this function
#'   \strong{favors} connections between planning units with high connectivity
#'   values. This function was inspired by Beger \emph{et al.} (2010).
#'
#'   The argument to \code{data} can be specified in several different ways:
#'
#'   \describe{
#'
#'   \item{\code{matrix}, \code{Matrix}}{where rows and columns represent
#'     different planning units and the value of each cell represents the
#'     strength of connectivity between two different planning units. Cells
#'     that occur along the matrix diagonal are treated as weights which
#'     indicate that planning units are more desirable in the solution.
#'     The argument to \code{zones} can be used to control
#'     the strength of connectivity between planning units in different zones.
#'     The default argument for \code{zones} is to treat planning units
#'     allocated to different zones as having zero connectivity.}
#'
#'   \item{\code{data.frame}}{containing the fields (columns)
#'     \code{"id1"}, \code{"id2"}, and \code{"boundary"}. Here, each row
#'     denotes the connectivity between two planning units following the
#'     \emph{Marxan} format. The data can be used to denote symmetric or
#'     asymmetric relationships between planning units. By default,
#'     input data is assumed to be symmetric unless asymmetric data is
#'     also included (e.g. if data is present for planning units 2 and 3, then
#'     the same amount of connectivity is expected for planning units 3 and 2,
#'     unless connectivity data is also provided for planning units 3 and 2).
#'     If the argument to \code{x} contains multiple zones, then the columns
#'     \code{"zone1"} and \code{"zone2"} can optionally be provided to manually
#'     specify the connectivity values between planning units when they are
#'     allocated to specific zones. If the columns \code{"zone1"} and
#'     \code{"zone2"} are present, then the argument to \code{zones} must be
#'     \code{NULL}.}
#'
#'   \item{\code{array}}{containing four-dimensions where cell values
#'     indicate the strength of connectivity between planning units
#'     when they are assigned to specific management zones. The first two
#'     dimensions (i.e. rows and columns) indicate the strength of
#'     connectivity between different planning units and the second two
#'     dimensions indicate the different management zones. Thus
#'     the \code{data[1, 2, 3, 4]} indicates the strength of
#'     connectivity between planning unit 1 and planning unit 2 when planning
#'     unit 1 is assigned to zone 3 and planning unit 2 is assigned to zone 4.}
#'
#'   }
#'
#'  The connectivity penalties are calculated using the following equations.
#'  Let \eqn{I} represent the set of planning units, \eqn{Z} represent the set
#'  of management zones, and \deqn{X_iz}{Xiz} represent the decision
#'  variable for planning unit \eqn{i} for in zone \eqn{z} (e.g. with binary
#'  values one indicating if planning unit is allocated or not). Also, let
#'  \eqn{p} represent the argument to \code{penalty}, \eqn{D} represent the
#'  argument to \code{data}, and \eqn{W} represent the argument to \code{zones}.
#'
#'  If the argument to \code{data} is supplied
#'  as a \code{matrix} or \code{Matrix}, then the penalties are calculated as:
#'
#'  \deqn{
#'  \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z} \sum_{y}^{Z} (-p \times X_{iz}
#'  \times X_{jy} \times D_{ij} \times W_{zy})}{
#'  sum_i^I sum_j^I sum_z^Z sum_y^Z (-p * Xiz * Xjy * Dij * Wzy)
#'  }
#'
#'  Otherwise, if the argument to \code{data} is supplied as a
#'  \code{data.frame} or \code{array}, then the penalties are calculated as:
#'
#'  \deqn{
#'  \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z} \sum_{y}^{Z} (-p \times X_{iz}
#'  \times X_{jy} \times D_{ijzy})}{
#'  sum_i^I sum_j^I sum_z^Z sum_y^Z (-p * Xiz * Xjy * Dijzy)
#'  }
#'
#'  Note that when the problem objective is to maximize some measure of
#'  benefit and not minimize some measure of cost, the term \eqn{-p} is
#'  replaced with \eqn{p}.
#'
#' @inherit add_boundary_penalties return seealso
#'
#' @references
#' Beger M, Linke S, Watts M, Game E, Treml E, Ball I, and Possingham, HP (2010)
#' Incorporating asymmetric connectivity into spatial decision making for
#' conservation, \emph{Conservation Letters}, 3: 359--368.
#'
#' @examples
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
#'       add_relative_targets(0.2)
#'
#' # create a symmetric connectivity matrix where the connectivity between
#' # two planning units corresponds to their shared boundary length
#' b_matrix <- boundary_matrix(sim_pu_polygons)
#'
#' # standardize matrix values to lay between zero and one
#' b_matrix[] <- rescale(b_matrix[])
#'
#' # visualize connectivity matrix
#' image(b_matrix)
#'
#' # create a symmetric connectivity matrix where the connectivity between
#' # two planning units corresponds to their spatial proximity
#' # i.e. planning units that are further apart share less connectivity
#' centroids <- rgeos::gCentroid(sim_pu_polygons, byid = TRUE)
#' d_matrix <- (1 / (as(dist(centroids@coords), "Matrix") + 1))
#'
#' # standardize matrix values to lay between zero and one
#' d_matrix[] <- rescale(d_matrix[])
#'
#' # remove connections between planning units without connectivity to
#' # reduce run-time
#' d_matrix[d_matrix < 0.7] <- 0
#'
#' # visualize connectivity matrix
#' image(d_matrix)
#'
#' # create a symmetric connectivity matrix where the connectivity
#' # between adjacent two planning units corresponds to their combined
#' # value in a field in the planning unit attribute data
#' # for example, this field could describe the extent of native vegetation in
#' # each planning unit and we could use connectivity penalties to identify
#' # solutions that cluster planning units together that both contain large
#' # amounts of native vegetation
#' c_matrix <- connectivity_matrix(sim_pu_polygons, "cost")
#'
#' # standardize matrix values to lay between zero and one
#' c_matrix[] <- rescale(c_matrix[])
#'
#' # visualize connectivity matrix
#' image(c_matrix)
#'
#' # create an asymmetric connectivity matrix. Here, connectivity occurs between
#' # adjacent planning units and, due to rivers flowing southwards
#' # through the study area, connectivity from northern planning units to
#' # southern planning units is ten times stronger than the reverse.
#' ac_matrix <- matrix(0, length(sim_pu_polygons), length(sim_pu_polygons))
#' ac_matrix <- as(ac_matrix, "Matrix")
#' adjacent_units <- rgeos::gIntersects(sim_pu_polygons, byid = TRUE)
#' for (i in seq_len(length(sim_pu_polygons))) {
#'   for (j in seq_len(length(sim_pu_polygons))) {
#'     # find if planning units are adjacent
#'     if (adjacent_units[i, j]) {
#'       # find if planning units lay north and south of each other
#'       # i.e. they have the same x-coordinate
#'       if (centroids@coords[i, 1] == centroids@coords[j, 1]) {
#'         if (centroids@coords[i, 2] > centroids@coords[j, 2]) {
#'           # if i is north of j add 10 units of connectivity
#'           ac_matrix[i, j] <- ac_matrix[i, j] + 10
#'         } else if (centroids@coords[i, 2] < centroids@coords[j, 2]) {
#'           # if i is south of j add 1 unit of connectivity
#'           ac_matrix[i, j] <- ac_matrix[i, j] + 1
#'         }
#'       }
#'     }
#'   }
#' }
#'
#' # standardize matrix values to lay between zero and one
#' ac_matrix[] <- rescale(ac_matrix[])
#'
#' # visualize asymmetric connectivity matrix
#' image(ac_matrix)
#'
#' # create penalties
#' penalties <- c(10, 25)
#'
#' # create problems using the different connectivity matrices and penalties
#' p2 <- list(p1,
#'            p1 %>% add_connectivity_penalties(penalties[1], data = b_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[2], data = b_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[1], data = d_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[2], data = d_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[1], data = c_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[2], data = c_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[1], data = ac_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[2], data = ac_matrix))
#'
#' # assign names to the problems
#' names(p2) <- c("basic problem",
#'                paste0("b_matrix (", penalties,")"),
#'                paste0("d_matrix (", penalties,")"),
#'                paste0("c_matrix (", penalties,")"),
#'                paste0("ac_matrix (", penalties,")"))
#' \donttest{
#' # solve problems
#' s2 <- lapply(p2, solve)
#'
#' # plot solutions
#' par(mfrow = c(3, 3))
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
#'       add_default_solver(time_limit = 60)
#'
#' # create matrix showing which planning units are adjacent to other units
#' a_matrix <- connected_matrix(sim_pu_zones_stack)
#'
#' # visualize matrix
#' image(a_matrix)
#'
#' # create a zone matrix where connectivities are only present between
#' # planning units that are allocated to the same zone
#' zm1 <- as(diag(3), "Matrix")
#'
#' # print zone matrix
#' print(zm1)
#'
#' # create a zone matrix where connectivities are strongest between
#' # planning units allocated to different zones
#' zm2 <- matrix(1, ncol = 3, nrow = 3)
#' diag(zm2) <- 0
#' zm2 <- as(zm2, "Matrix")
#'
#' # print zone matrix
#' print(zm2)
#'
#' # create a zone matrix that indicates that connectivities between planning
#' # units assigned to the same zone are much higher than connectivities
#' # assigned to different zones
#' zm3 <- matrix(0.1, ncol = 3, nrow = 3)
#' diag(zm3) <- 1
#' zm3 <- as(zm3, "Matrix")
#'
#' # print zone matrix
#' print(zm3)
#'
#' # create a zone matrix that indicates that connectivities between planning
#' # units allocated to zone 1 are very high, connectivities between planning
#' # units allocated to zones 1 and 2 are moderately high, and connectivities
#' # planning units allocated to other zones are low
#' zm4 <- matrix(0.1, ncol = 3, nrow = 3)
#' zm4[1, 1] <- 1
#' zm4[1, 2] <- 0.5
#' zm4[2, 1] <- 0.5
#' zm4 <- as(zm4, "Matrix")
#'
#' # print zone matrix
#' print(zm4)
#'
#' # create a zone matrix with strong connectivities between planning units
#' # allocated to the same zone, moderate connectivities between planning
#' # unit allocated to zone 1 and zone 2, and negative connectivities between
#' # planning units allocated to zone 3 and the other two zones
#' zm5 <- matrix(-1, ncol = 3, nrow = 3)
#' zm5[1, 2] <- 0.5
#' zm5[2, 1] <- 0.5
#' diag(zm5) <- 1
#' zm5 <- as(zm5, "Matrix")
#'
#' # print zone matrix
#' print(zm5)
#'
#' # create vector of penalties to use creating problems
#' penalties2 <- c(5, 30)
#'
#' # create multi-zone problems using the adjacent connectivity matrix and
#' # different zone matrices
#' p4 <- list(
#'   p3,
#'   p3 %>% add_connectivity_penalties(penalties2[1], zm1, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[2], zm1, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[1], zm2, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[2], zm2, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[1], zm3, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[2], zm3, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[1], zm4, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[2], zm4, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[1], zm5, a_matrix),
#'   p3 %>% add_connectivity_penalties(penalties2[2], zm5, a_matrix))
#'
#' # assign names to the problems
#' names(p4) <- c("basic problem",
#'                paste0("zm", rep(seq_len(5), each = 2), " (",
#'                       rep(penalties2, 2), ")"))
#' \donttest{
#' # solve problems
#' s4 <- lapply(p4, solve)
#' s4 <- lapply(s4, category_layer)
#' s4 <- stack(s4)
#'
#' # plot solutions
#' plot(s4, main = names(p4), axes = FALSE, box = FALSE)
#' }
#'
#' # create an array to manually specify the connectivities between
#' # each planning unit when they are allocated to each different zone
#' # for real-world problems, these connectivities would be generated using
#' # data - but here these connectivity values are assigned as random
#' # ones or zeros
#' c_array <- array(0, c(rep(ncell(sim_pu_zones_stack[[1]]), 2), 3, 3))
#' for (z1 in seq_len(3))
#'   for (z2 in seq_len(3))
#'     c_array[, , z1, z2] <- round(runif(ncell(sim_pu_zones_stack[[1]]) ^ 2,
#'                                        0, 0.505))
#'
#' # create a problem with the manually specified connectivity array
#' # note that the zones argument is set to NULL because the connectivity
#' # data is an array
#' p5 <- list(p3,
#'            p3 %>% add_connectivity_penalties(30, zones = NULL, c_array))
#'
#'
#' # assign names to the problems
#' names(p5) <- c("basic problem", "connectivity array")
#' \donttest{
#' # solve problems
#' s5 <- lapply(p5, solve)
#' s5 <- lapply(s5, category_layer)
#' s5 <- stack(s5)
#'
#' # plot solutions
#' plot(s5, main = names(p5), axes = FALSE, box = FALSE)
#' }
#' @name add_connectivity_penalties
#'
#' @exportMethod add_connectivity_penalties
#'
#' @aliases add_connectivity_penalties,ConservationProblem,ANY,ANY,Matrix-method add_connectivity_penalties,ConservationProblem,ANY,ANY,matrix-method add_connectivity_penalties,ConservationProblem,ANY,ANY,dgCMatrix-method add_connectivity_penalties,ConservationProblem,ANY,ANY,data.frame-method add_connectivity_penalties,ConservationProblem,ANY,ANY,array-method
NULL

#' @export
methods::setGeneric("add_connectivity_penalties",
  signature = methods::signature("x", "penalty", "zones", "data"),
  function(x, penalty, zones = diag(number_of_zones(x)), data)
    standardGeneric("add_connectivity_penalties"))

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,ANY,ANY,matrix}(x, penalty, zones, data)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "matrix"),
  function(x, penalty, zones, data) {
     add_connectivity_penalties(x, penalty, zones, methods::as(data, "dgCMatrix"))
})

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,ANY,ANY,Matrix}(x, penalty, zones, data)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "Matrix"),
  function(x, penalty, zones, data) {
     add_connectivity_penalties(x, penalty, zones, methods::as(data, "dgCMatrix"))
})

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,ANY,ANY,dgCMatrix}(x, penalty, zones, data)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
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
    # coerce zones to matrix
    zones <- as.matrix(zones)
    # add row names and column names to zones matrix
    rownames(zones) <- x$zone_names()
    colnames(zones) <- rownames(zones)
    # add penalties
    x$add_penalty(pproto(
      "ConnectivityPenalty",
      Penalty,
      name = "Connectivity penalties",
      data = list(data = data),
      parameters = parameters(numeric_parameter("penalty", penalty),
                              numeric_matrix_parameter("zones", zones,
                                                      lower_limit = -1,
                                                      upper_limit = 1,
                                                      symmetric = FALSE)),
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
                                inherits(y, "ConservationProblem"))
        # exctract parameters
        p <- self$parameters$get("penalty")
        if (abs(p) > 1e-50) {
          # extract data and zone parameters
          z <- self$parameters$get("zones")
          indices <- y$planning_unit_indices()
          d <- self$get_data("data")[indices, indices]
          # convert two matrices to list of list of sparseMatrix objects
          # to represent a sparse 4-dimensional array
          m <- list()
          for (z1 in seq_len(ncol(z))) {
            m[[z1]] <- list()
            for (z2 in seq_len(nrow(z))) {
              m[[z1]][[z2]] <- d * z[z1, z2]
            }
          }
          # apply penalties
          rcpp_apply_connectivity_penalties(x$ptr, p, m)
        }
        invisible(TRUE)
    }))
})

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,ANY,ANY,data.frame}(x, penalty, zones, data)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "data.frame"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ConservationProblem"), assertthat::is.scalar(penalty),
      is.finite(penalty), is.data.frame(data))
  # add penalties to problem
  add_connectivity_penalties(x, penalty, zones,
                             marxan_boundary_data_to_matrix(x, data))
})

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,ANY,ANY,array}(x, penalty, zones, data)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
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
        m[[z1]][[z2]] <- methods::as(data[indices, indices, z1, z2],
                                     "dgCMatrix")
      }
    }
    # create new penalty object
    x$add_penalty(pproto(
      "ConnectivityPenalty",
      Penalty,
      name = "Connectivity penalties",
      data = list(data = m),
      parameters = parameters(numeric_parameter("penalty", penalty)),
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
                                inherits(y, "ConservationProblem"))
        p <- self$parameters$get("penalty")
        if (abs(p) > 1e-50) {
          rcpp_apply_connectivity_penalties(x$ptr, p, self$get_data("data"))
        }
        invisible(TRUE)
    }))
})
