#' @include internal.R Penalty-proto.R
NULL

#' Add connectivity penalties
#'
#' Add penalties to a conservation problem to favor solutions that select
#' planning units with high connectivity between them.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param penalty \code{numeric} penalty that is used to multiply
#'   the connectivity values in the argument to \code{connectivity_data}.
#'   Supplying arguments with greater values to \code{penalty} will return
#'   solutions that containing planning units that share greater connectivity.
#'   This parameter is equivalent to the connectivity strength modifier
#'   (CSM; Beger \emph{et al.} 2010) used in \emph{Marxan}. The default argument
#'   is \code{1} so that penalty values are
#'   the same as the values supplied to \code{connectivity_data}. Note that the
#'   argument to \code{penalty} must always be single \code{numeric} value even
#'   for problems with multiple zones.
#'
#' @param connectivity_data A \code{data.frame}, \code{matrix}, or
#'   \code{array} object. See the Details section for more information.
#'
#' @param ... not used.
#'
#' @details This function uses connectivity data to penalize solutions
#'   that have low connectivity between selected planning units in the solution.
#'   It can be used for symmetric or asymmetric relationships
#'   between planning units and is inspired by Beger \emph{et al.} (2010).
#'
#'   The \code{connectivity_data} can be specified in several different ways:
#'
#'   \describe{
#'
#'   \item{\code{matrix}, \code{Matrix}}{where rows and columns represent
#'     different planning units and the value of each cell represents the
#'     strength of connectivity between two different planning units. Cells
#'     that occur along the diagonal correspond to connectivity weights such
#'     that planning units with higher values are more desireable in the
#'     solution. Note that \code{matrix} objects cannot be supplied as an
#'     argument for \code{connectivity_data} when the argument to \code{x}
#'     contains multiple zones, and instead an \code{array} object must be
#'     supplied.}
#'
#'   \item{\code{data.frame}}{containing the fields (columns)
#'     \code{"id1"}, \code{"id2"}, and \code{"boundary"}. Here, each row
#'     denotes the connectivity between two planning units (following the
#'     \emph{Marxan} format). The data can be used to denote symmetric or
#'     asymmetric relationships between planning units. By default,
#'     input data is assumed to be symmetric (e.g. if connectivity data
#'     is present for planning units 2 and 3, then the same amount
#'     of connectivity is expected for planning units 3 and 2). If the
#'     argument to \code{x} contains multiple zones, then \code{"zone1"}
#'     and \code{"zone2"} columns are required indicating the names
#'     of zones to which the planning unit allocations pertain.}
#'
#'   \item{\code{array}}{containing four-dimensions where cell values
#'     indicate the strength of connectivity between planning units
#'     when they are assigned to specific management zones. The first two
#'     dimensions (i.e. rows and columns) indicate the strength of
#'     connectivity between different planning units and the second two
#'     dimensions indicate the different management zones. Thus
#'     the \code{connectivity_data[1, 2, 3, 4]} indicates the strength of
#'     connectivity between planning unit 1 and planning unit 2 when planning
#'     unit 1 is assigned to zone 3 and planning unit 2 is assigned to zone 4.}
#'
#'   }
#'
#' @inherit add_boundary_penalties return seealso
#'
#' @references
#' Beger M, Linke S, Watts M, Game E, Treml E, Ball I, and Possingham, HP (2010)
#' Incorporating asymmetric connectivity into spatial decision making for
#' conservation, \emph{Conservation Letters}, 3: 359--368.
#'
#' @examples
#' # load data
#' data(sim_pu_polygons, sim_features, sim_pu_zones_polygons,
#'     sim_features_zones)
#'
#' # define function to rescale values between zero and one so that we
#' # can compare different connecitivity matrices
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
#' # value in a field in the planning unit attribute
#' # for example, this field could describe the extent of native vegetation in
#' # each planning unit and we could use connectivity penalties to identfy
#' # solutions that cluster planning units togeather that contain large amounts
#' # of native vegetation
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
#' # visualize assymetric connectivity matrix
#' image(ac_matrix)
#'
#' # create penalties
#' penalties <- c(10, 400)
#'
#' # create problems using the different connectivity matrices and penalties
#' p2 <- list(p1,
#'            p1 %>% add_connectivity_penalties(penalties[1], b_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[2], b_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[1], d_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[2], d_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[1], c_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[2], c_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[1], ac_matrix),
#'            p1 %>% add_connectivity_penalties(penalties[2], ac_matrix))
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
#' # create minimal multi-zone problem and limit solver to ten seconds
#' # to reduce run-time
#' p3 <- problem(sim_pu_zones_polygons, sim_features_zones,
#'               c("cost_1", "cost_2", "cost_3")) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(0.1, nrow = 5, ncol = 3)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(10)
#'
#' # create matrix showing the shared boundary between different planning units
#' b_matrix2 <- as.matrix(boundary_matrix(sim_pu_zones_polygons))
#'
#' # remove exposed edges of planning units that don't share boundaries
#' diag(b_matrix2) <- 0
#'
#' # create a symmetric connectivity matrix that corresponds to
#' # shared boundaries between planning units
#' b_array <- array(0, c(rep(length(sim_pu_zones_polygons), 2), 3, 3))
#' for (z1 in seq_len(3))
#'   for (z2 in seq_len(3))
#'     b_array[, , z1, z2] <- b_matrix2[]
#'
#' # standardize array values to lay between zero and one
#' b_array[] <- rescale(b_array[])
#'
#' # create a symmetric connectivity matrix that corresponds to shared
#' # boundaries between planning units - but connections between planning units
#' # from the same zone are preferred ten times more than connections between
#' # different zones
#' b_array2 <- array(0, c(rep(length(sim_pu_zones_polygons), 2), 3, 3))
#' for (z1 in seq_len(3))
#'   for (z2 in seq_len(3))
#'     b_array2[, , z1, z2] <- b_matrix2[] * ifelse(z1 == z2, 10, 1)
#'
#' # standardize array values to lay between zero and one
#' b_array2[] <- rescale(b_array2[])
#'
#' # create penalties
#' penalties <- c(10, 400)
#'
#' # create multi-zone problems using the different connectivity arrays and
#' # penalties
#' p4 <- list(p3,
#'            p3 %>% add_connectivity_penalties(penalties[1], b_array),
#'            p3 %>% add_connectivity_penalties(penalties[2], b_array),
#'            p3 %>% add_connectivity_penalties(penalties[1], b_array2),
#'            p3 %>% add_connectivity_penalties(penalties[2], b_array2))
#'
#' # assign names to the problems
#' names(p4) <- c("basic problem",
#'                paste0("b_array (", penalties,")"),
#'                paste0("b_array2 (", penalties,")"))
#' \donttest{
#' # solve problems
#' s4 <- lapply(p4, solve)
#'
#' # plot solutions
#' par(mfrow = c(3, 2))
#' for (i in seq_along(s4)) {
#'   plot(s4[[i]], main = names(p4)[i], cex = 1.5, col = "white")
#'   plot(s4[[i]][s4[[i]]$solution_1_zone_1 == 1, ], col = "darkblue",
#'        add = TRUE)
#'   plot(s4[[i]][s4[[i]]$solution_1_zone_2 == 1, ], col = "darkred",
#'        add = TRUE)
#'   plot(s4[[i]][s4[[i]]$solution_1_zone_3 == 1, ], col = "darkgreen",
#'        add = TRUE)
#' }
#' }
#' @name add_connectivity_penalties
#'
#' @exportMethod add_connectivity_penalties
#'
#' @aliases add_connectivity_penalties,ConservationProblem,numeric,Matrix-method add_connectivity_penalties,ConservationProblem,numeric,matrix-method add_connectivity_penalties,ConservationProblem,numeric,data.frame-method add_connectivity_penalties,ConservationProblem,numeric,array-method
NULL

#' @export
methods::setGeneric("add_connectivity_penalties",
                    signature = methods::signature("x", "penalty",
                                                   "connectivity_data"),
                    function(x, penalty, connectivity_data, ...)
                      standardGeneric("add_connectivity_penalties"))

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,numeric,Matrix}(x, penalty, connectivity_data, ...)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "numeric", "Matrix"),
  function(x, penalty, connectivity_data, ...) {
     add_connectivity_penalties(x, penalty, as.matrix(connectivity_data))
})

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,numeric,matrix}(x, penalty, connectivity_data, ...)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "numeric", "matrix"),
  function(x, penalty, connectivity_data, ...) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
                            isTRUE(all(is.finite(penalty))),
                            assertthat::is.scalar(penalty),
                            is.matrix(connectivity_data),
                            is.numeric(connectivity_data),
                            ncol(connectivity_data) == nrow(connectivity_data),
                            x$number_of_total_units() ==
                              ncol(connectivity_data),
                            sum(is.finite(connectivity_data)) > 0)
   assertthat::assert_that(x$number_of_zones() == 1,
                           msg = paste("argument to connectivity_data must",
                                       "be an array for problems with",
                                       "multiple zones"))
   # add penalties
   add_connectivity_penalties(x, penalty, array(c(connectivity_data),
                                                dim = c(nrow(connectivity_data),
                                                        ncol(connectivity_data),
                                                        1, 1)))
})

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,numeric,data.frame}(x, penalty, connectivity_data, ...)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "numeric", "data.frame"),
  function(x, penalty, connectivity_data, ...) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ConservationProblem"), isTRUE(all(is.finite(penalty))),
      assertthat::is.scalar(penalty),
      nrow(connectivity_data) > 0,
      assertthat::has_name(connectivity_data, "id1"),
      assertthat::has_name(connectivity_data, "id2"),
      assertthat::has_name(connectivity_data, "boundary"),
      assertthat::noNA(connectivity_data$id1),
      assertthat::noNA(connectivity_data$id2),
      assertthat::noNA(connectivity_data$boundary),
      is.numeric(connectivity_data$id1),
      is.numeric(connectivity_data$id2), is.numeric(connectivity_data$boundary),
      all(connectivity_data$id1 == round(connectivity_data$id1)),
      all(connectivity_data$id2 == round(connectivity_data$id2)))
  if (assertthat::has_name(connectivity_data, "zone1") ||
      assertthat::has_name(connectivity_data, "zone2") ||
      x$number_of_zones() > 1) {
        assertthat::assert_that(
          assertthat::has_name(connectivity_data, "zone1"),
          assertthat::has_name(connectivity_data, "zone2"),
          assertthat::noNA(connectivity_data$zone1),
          assertthat::noNA(connectivity_data$zone1),
          is.character(connectivity_data$zone1) ||
            is.factor(connectivity_data$zone1),
          is.character(connectivity_data$zone2) ||
            is.factor(connectivity_data$zone2),
          all(as.character(connectivity_data$zone1) %in% x$zone_names()),
          all(as.character(connectivity_data$zone2 %in% x$zone_names())))
  }
  # add zone fields if missing
  if (!assertthat::has_name(connectivity_data, "zone1"))
    connectivity_data$zone1 <- x$zone_names()
  if (!assertthat::has_name(connectivity_data, "zone1"))
    connectivity_data$zone2 <- x$zone_names()
  # convert zone names to indices
  zone1_index <- match(connectivity_data$zone1, x$zone_names())
  zone2_index <- match(connectivity_data$zone2, x$zone_names())
  # if planing unit data is a data.frame then standardize ids
  if (inherits(x$data$cost, "data.frame")) {
    connectivity_data$id1 <- match(connectivity_data$id1, x$data$cost$id)
    connectivity_data$id2 <- match(connectivity_data$id2, x$data$cost$id)
    assertthat::assert_that(assertthat::noNA(connectivity_data$id1),
      msg = paste("the field connectivity_data$id1 in argument to",
                  "connectivity_data contains values for planning units",
                  "not present in the planning unit data"))
    assertthat::assert_that(assertthat::noNA(connectivity_data$id2),
      msg = paste("the field connectivity_data$id2 in argument to",
                  "connectivity_data contains values for planning units",
                  "not present in the planning unit data"))
  } else {
    assertthat::assert_that(
      min(connectivity_data$id1) > 0,
      min(connectivity_data$id2) > 0,
      max(connectivity_data$id1) <= x$number_of_total_units(),
      max(connectivity_data$id2) <= x$number_of_total_units())
  }
  # create array with connectivity data
  connectivity_array <- array(0, dim = c(x$number_of_total_units(),
                                         x$number_of_total_units(),
                                         x$number_of_zones(),
                                         x$number_of_zones()))
  indices <- matrix(c(connectivity_data$id1, connectivity_data$id2,
                      zone1_index, zone2_index), ncol = 4)
  connectivity_array[indices] <- connectivity_data$boundary
  # add data for other diagonal if missing
  pos <- connectivity_data$id2 < connectivity_data$id1
  connectivity_data[pos, c("id1", "id2")] <- connectivity_data[pos, c("id2",
                                                                      "id1")]
  pos <- zone2_index < zone1_index
  connectivity_data[pos, c("zone1", "zone2")] <-
    connectivity_data[pos, c("zone2", "zone1")]
  if (anyDuplicated(
        paste(connectivity_data$id1, connectivity_data$id2,
              connectivity_data$zone1, connectivity_data$zone2)) != 0) {
    connectivity_array[indices[, c(2, 1, 4, 3)]] <- connectivity_data$boundary
  }
  # add penalties to problem
  add_connectivity_penalties(x, penalty, connectivity_array)
})

#' @name add_connectivity_penalties
#' @usage \S4method{add_connectivity_penalties}{ConservationProblem,numeric,array}(x, penalty, connectivity_data, ...)
#' @rdname add_connectivity_penalties
methods::setMethod("add_connectivity_penalties",
  methods::signature("ConservationProblem", "numeric", "array"),
  function(x, penalty, connectivity_data, ...) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      isTRUE(all(is.finite(penalty))), assertthat::is.scalar(penalty),
      is.array(connectivity_data),
      length(dim(connectivity_data)) == 4,
      dim(connectivity_data)[1] == x$number_of_total_units(),
      dim(connectivity_data)[2] == x$number_of_total_units(),
      dim(connectivity_data)[3] == x$number_of_zones(),
      dim(connectivity_data)[4] == x$number_of_zones(),
      all(is.finite(connectivity_data)))
    # generate indices for units that are planning units
    indices <- x$planning_unit_indices()
    # convert array to list of list of sparseMatrix objects
    m <- list()
    symmetry_status <- TRUE
    for (z1 in seq_len(dim(connectivity_data)[3])) {
      m[[z1]] <- list()
      for (z2 in seq_len(dim(connectivity_data)[4])) {
        m[[z1]][[z2]] <- methods::as(connectivity_data[indices, indices, z1,
                                                       z2], "dgCMatrix")
        symmetry_status <- symmetry_status &&
                           Matrix::isSymmetric(m[[z1]][[z2]])
      }
    }
    # if all matrices are symmetric then remove the upper diagonal
    if (symmetry_status) {
      for (z1 in seq_len(dim(connectivity_data)[3])) {
        for (z2 in seq_len(dim(connectivity_data)[4])) {
          m[[z1]][[z2]] <- Matrix::forceSymmetric(m[[z1]][[z2]], uplo = "L")
          class(m[[z1]][[z2]]) <- "dgCMatrix"
        }
      }
    }
    # create new penalty object
    x$add_penalty(pproto(
      "ConnectivityPenalty",
      Penalty,
      name = "Connectivity penalties",
      data = list(connectivity_data = m,
                  symmetry_status = symmetry_status),
      parameters = parameters(numeric_parameter("penalty", penalty)),
      apply = function(self, x, y) {
        assertthat::assert_that(inherits(x, "OptimizationProblem"),
                                inherits(y, "ConservationProblem"))
        # exctract penalty
        p <- self$parameters$get("penalty")
        if (abs(p) > 1e-50) {
          # extract connectivity data
          m <- self$get_data("connectivity_data")
          # apply constraints
          if (self$get_data("symmetry_status")) {
            rcpp_apply_symmetric_connectivity_constraints(x$ptr, m, p)
          } else {
            # apply penalties using asymmetric formulation
            rcpp_apply_asymmetric_connectivity_constraints(x$ptr, m, p)
          }
        }
        invisible(TRUE)
    }))
})
