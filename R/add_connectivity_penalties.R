#' @include internal.R Penalty-proto.R
NULL

#' Add connectivity penalties
#'
#' Add constraints to a conservation problem to favor solutions that select
#' planning units with high connectivity between them. Uses connectivity data 
#' in the form of a \code{matrix} or \code{data.frame} object, where strength 
#' of connectivity is a value such as the inverse distance between consecutive 
#' planning units. This function can be used for symmetric or asymmetric 
#' relationships between planning units.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param penalty \code{numeric} penalty for missing connections between
#'  planning units. This is equivalent to the connectivity strength modifier
#'  (CSM; (Beger et al. 2010)). Defaults to one so that penalties are the
#'  same as the values in the \code{data}.
#'
#' @param connectivity_data A \code{data.frame} or \code{matrix} 
#' object that shows the strength of connectivity between consecutive planning
#'  units. If \code{connectivity_data} is a matrix, then rows and columns represent
#'  each planing unit and the cell values represent the connectivity between
#'  them. If \code{connectivity_data} is a \code{data.frame} the column names must
#'  be \code{"id1"}, \code{"id2"}, \code{"boundary"}, where each row
#'  shows the connectivity between two planning units (following the Marxan
#'  format). The data can be used to denote symmetric or asymmetric
#'  relationships between planning units.
#'
#' @return \code{\link{ConservationProblem-class}} object.
#'
#' @seealso \code{\link{constraints}}, \code{\link{add_boundary_penalties}},
#'  \code{\link{penalties}}.
#'
#' @examples
#' # load data
#' data(sim_pu_points, sim_features)
#'
#' # create a symmetric connectivity matrix where the connectivity between
#' # two planning units is the inverse distance between them
#' sc_matrix <- (1 / (as.matrix(dist(sim_pu_points@coords)) + 1))
#'
#' # remove connections between planning unitswith little connectivity
#' sc_matrix[sc_matrix < 0.85] <- 0
#'
#' # create basic problem
#' p1 <- problem(sim_pu_points, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2)
#'
#' # create problem with low connectivity penalties
#' p2 <- p1 %>% add_connectivity_penalties(25, sc_matrix)
#'
#' # create problem with higher connectivity penalties
#' p3 <- p1 %>% add_connectivity_penalties(50, sc_matrix)
#'
#' # create an asymmetric connectivity matrix where links from even numered
#' # units to odd numbered units have half the connectivity as from odd
#' # numbered units to even units
#' even_units <- seq(2, length(sim_pu_points), 2)
#' odd_units <- seq(1, length(sim_pu_points), 2)
#' ac_matrix <- sc_matrix
#' ac_matrix[even_units, odd_units] <- 0.5 * ac_matrix[even_units, odd_units]
#'
#' # create problem with asymmetric connectivity and high penalties
#' p4 <- p1 %>% add_connectivity_penalties(50, ac_matrix)
#'
#' \donttest{
#' # solve problems
#' s <- list(solve(p1), solve(p2), solve(p3), solve(p4))
#'
#' # plot solutions
#' par(mfrow=c(2,2))
#'
#' plot(s[[1]], pch=19, main="basic solution", cex = 1.5)
#' points(s[[1]][s[[1]]$solution==1, ], col = "darkgreen", pch = 19, cex = 1.5)
#'
#' plot(s[[2]], pch=19, main="small penalties", cex = 1.5)
#' points(s[[2]][s[[2]]$solution==1, ], col = "darkgreen", pch = 19, cex = 1.5)
#'
#' plot(s[[3]], pch=19, main="high penalties", cex = 1.5)
#' points(s[[3]][s[[3]]$solution==1, ], col = "darkgreen", pch = 19, cex = 1.5)
#'
#' plot(s[[4]], pch=19, main="asymmetric connectivity", cex = 1.5)
#' points(s[[4]][s[[4]]$solution==1, ], col = "darkgreen", pch=19, cex = 1.5)
#' }
#'
#' @export
add_connectivity_penalties <- function(x, penalty, connectivity_data) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    isTRUE(all(is.finite(penalty))), assertthat::is.scalar(penalty),
    isTRUE(penalty >= 0))
  # convert connectivity_data to Matrix if is a data.frame
  if (inherits(connectivity_data, "data.frame")) {
    assertthat::assert_that(assertthat::has_name(connectivity_data, "id1"),
      assertthat::has_name(connectivity_data, "id2"),
      assertthat::has_name(connectivity_data, "boundary"))
    if (inherits(x$data$cost, "data.frame")) {
      # if planning unit data is a dat.frame, then standardise ids
      connectivity_data$id1 <- match(connectivity_data$id1, x$data$cost$id)
      connectivity_data$id2 <- match(connectivity_data$id2, x$data$cost$id)
    }
    connectivity_data <- connectivity_data[, c("id1", "id2", "boundary")]
    connectivity_data <- triplet_dataframe_to_matrix(
      connectivity_data, forceSymmetric = FALSE,
      dims = rep(x$number_of_planning_units(), 2),
      use.last.ij = FALSE, index1 = TRUE)
  }
  if (inherits(connectivity_data, c("matrix", "Matrix"))) {
    # coerce to sparse matrix
    if (!inherits(connectivity_data, c("dsCMatrix", "dgCMatrix")))
      connectivity_data <- methods::as(connectivity_data, "dgCMatrix")
    # check validity of matrix
    assertthat::assert_that(
      isTRUE(ncol(connectivity_data) == x$number_of_planning_units()),
      isTRUE(ncol(connectivity_data) == nrow(connectivity_data)))
  } else {
    # return error is connectivity_data is not valid matrix
    stop("argument to connectivity_data is not of a recognized class")
  }
  # create parameters
  p <- parameters(numeric_parameter("penalty", penalty, lower_limit = 0))
  # create new penalty object
  x$add_penalty(pproto(
    "ConnectivityPenalty",
    Penalty,
    name = "Connectivity penalties",
    data = list(connectivity_matrix = connectivity_data),
    parameters = p,
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      if (isTRUE(self$parameters$get("penalty") > 1e-10)) {
        # get matrix
        m <- self$get_data("connectivity_matrix")
        # apply constraints
        if (Matrix::isSymmetric(m)) {
          # apply penalties using symmetric formulation
          m <- Matrix::forceSymmetric(m)
          class(m) <- "dgCMatrix"
          rcpp_apply_symmetric_boundary_constraints(x$ptr, m,
            self$parameters$get("penalty"), 1)
        } else {
          # apply penalties using asymmetric formulation
          rcpp_apply_asymmetric_boundary_constraints(x$ptr, m,
            self$parameters$get("penalty"), 1)
        }
      }
      invisible(TRUE)
    }))
}
