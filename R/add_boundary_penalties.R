#' @include internal.R Penalty-proto.R
NULL

#' Add boundary penalties
#'
#' Add penalties to a conservation problem to favor solutions that clump
#' selected planning units together into contiguous reserves.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param penalty \code{numeric} penalty for exposed edges. This
#'  is equivalent to the
#'  \href{http://marxan.net/downloads/uq_marxan_web_2/module2.html}{boundary length modifier (BLM)}
#'  parameter in \href{http://marxan.net}{Marxan}.
#'
#' @param edge_factor \code{numeric} proportion to scale edges that do
#'   not have any neighboring planning units. For example, an edge factor
#'   of \code{0.5} is commonly used for planning units along the coast line.
#'
#' @param boundary_data \code{matrix}, \code{\link[Matrix]{Matrix}}, or
#'   \code{data.frame} object showing the shared boundary lengths
#'   between planning units. If \code{boundary_data} is a matrix then
#'   each row and column denote a planning unit and cell values
#'   represent their shared boundary. If \code{boundary_data} is
#'   \code{data.frame} then it must have the columns \code{"id1"},
#'   \code{"id2"}, and \code{"boundary"}, where each row shows
#'   the shared boundary between two planning units (as per the standard
#'   Marxan input format). This argument is required
#'   if the planning units in \code{x} are stored in a \code{data.frame}
#'   object. Otherwise, it is optional and the boundary data will be
#'   automatically calculated. Note that the \code{boundary_data} must be
#'   denote symmetric relationships between planning units. If
#'   asymmetric relationships are required, use the
#'   \code{\link{add_connectivity_penalties}} function.
#'
#' @return \code{\link{ConservationProblem-class}} object.
#'
#' @seealso \code{\link{constraints}}, \code{\link{penalties}}.
#'
#' @examples
#' # create basic problem
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_default_solver()
#'
#' # create problem with low boundary penalties
#' p2 <- p1 %>% add_boundary_penalties(5, 1)
#'
#' # create problem with high boundary penalties but outer edges receive
#' # half the penalty as inner edges
#' p3 <- p1 %>% add_boundary_penalties(50, 0.5)
#'
#' \donttest{
#' # solve problems
#' s <- stack(solve(p1), solve(p2), solve(p3))
#'
#' # plot solutions
#' plot(s, main = c("basic solution", "small penalties", "high penalties"))
#' }
#'
#' @export
add_boundary_penalties <- function(x, penalty, edge_factor,
                                   boundary_data = NULL) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, "ConservationProblem"),
    isTRUE(all(is.finite(penalty))), assertthat::is.scalar(penalty),
    isTRUE(all(is.finite(edge_factor))), assertthat::is.scalar(edge_factor),
    isTRUE(penalty >= 0),
    isTRUE(edge_factor >= 0), isTRUE(edge_factor <= 1))
  # assert that boundary data is in correct format
  if (!is.null(boundary_data)) {
    # if boundary data is in data.frame format then coerce to sparse matrix
    if (inherits(boundary_data, "data.frame")) {
      assertthat::assert_that(assertthat::has_name(boundary_data, "id1"),
        assertthat::has_name(boundary_data, "id2"),
        assertthat::has_name(boundary_data, "boundary"),
        assertthat::noNA(boundary_data$id1),
        assertthat::noNA(boundary_data$id2),
        assertthat::noNA(boundary_data$boundary))
      # reorder columns
      boundary_data <- boundary_data[, c("id1", "id2", "boundary")]
      # if planning units are data.frame with ids then convert ids to indices
      if (inherits(x$data$cost, "data.frame")) {
        boundary_data$id1 <- match(boundary_data$id1, x$data$cost$id)
        boundary_data$id2 <- match(boundary_data$id2, x$data$cost$id)
      }
      # check that all boundary ids are valid
      n_pu <- x$number_of_planning_units()
      if (isTRUE(any(is.na(boundary_data$id1))) ||
          isTRUE(any(is.na(boundary_data$id2))) ||
          isTRUE(max(boundary_data$id1) > n_pu) ||
          isTRUE(max(boundary_data$id2) > n_pu) ||
          isTRUE(min(boundary_data$id1) < 1) ||
          isTRUE(min(boundary_data$id2 < 1)))
        stop("argument to boundary_data contains ids for planning units that",
          " are not present in x")
      # convert boundary data to sparse matrix
      boundary_data <- triplet_dataframe_to_matrix(boundary_data,
        forceSymmetric = TRUE, dims = rep(x$number_of_planning_units(), 2))
    }
    # if/when boundary_data is matrix run further checks to ensure that
    # it is compatible with planning unit data
    if (inherits(boundary_data, c("matrix", "Matrix"))) {
      # if it is matrix coerce to sparse matrix
      boundary_matrix <- boundary_data
      if (!inherits(boundary_data, c("dsCMatrix", "dgCMatrix")))
        boundary_matrix <- methods::as(boundary_data, "dgCMatrix")
      # check that matrix properties are correct
      assertthat::assert_that(ncol(boundary_matrix) == nrow(boundary_matrix),
        isTRUE(x$number_of_planning_units() == ncol(boundary_matrix)),
        all(is.finite(boundary_matrix@x)))
      # force symmetry          base::print ("here 2")
      boundary_matrix <- Matrix::forceSymmetric(boundary_matrix)
      class(boundary_matrix) <- "dgCMatrix"
      # create list with data
      d <- list(boundary_matrix = boundary_matrix)
    } else {
     # throw error because object class not recognized
      stop("argument to boundary_data is of a class that is not supported")
    }
  } else {
    d <- list()
  }
  # create parameters
  p <- parameters(
    numeric_parameter("penalty", penalty, lower_limit = 0),
    proportion_parameter("edge factor", edge_factor))
  # create new constraint object
  x$add_penalty(pproto(
    "BoundaryPenalty",
    Penalty,
    name = "Boundary penalties",
    data = d,
    parameters = p,
    calculate = function(self, x) {
        assertthat::assert_that(inherits(x, "ConservationProblem"))
        if (is.Waiver(x$get_data("boundary_matrix"))) {
          m <- self$get_data("boundary_matrix")
          if (!is.Waiver(m)) {
            x$set_data("boundary_matrix", m)
          } else {
            x$set_data("boundary_matrix", boundary_matrix(x$get_data("cost")))
          }
        }
        # return invisible
        invisible()
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      if (isTRUE(self$parameters$get("penalty") > 1e-10)) {
        m <- y$get_data("boundary_matrix")
        class(m) <- "dgCMatrix"
        rcpp_apply_symmetric_boundary_constraints(x$ptr, m,
          self$parameters$get("penalty"), self$parameters$get("edge factor"))
      }
      invisible(TRUE)
    }))
}
