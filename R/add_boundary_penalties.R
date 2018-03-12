#' @include internal.R Penalty-proto.R
NULL

#' Add boundary penalties
#'
#' Add penalties to a conservation problem to favor solutions that clump
#' selected planning units together into contiguous reserves.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param penalty \code{numeric} or \code{matrix} of penalty multipliers for
#'  exposed edges. This is equivalent to the
#'  \href{http://marxan.net/downloads/uq_marxan_web_2/module2.html}{boundary length modifier (BLM)}
#'  parameter in \href{http://marxan.net}{Marxan}. Note that \code{matrix}
#'  arguments must be symmetric. See the Details section for more information.
#'
#' @param edge_factor \code{numeric} \code{vector} proportion to scale edges
#'   that do not have any neighboring planning units. For example, an edge
#'   factor of \code{0.5} is commonly used for planning units along the coast
#'   line. Note that this argument must have an element for each zone
#'   in the argument to \code{x}.
#'
#' @param boundary_data A \code{matrix} or
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
#' @param ... not used.
#'
#' @details This function adds penalties to a conservation planning problem
#' to penalize fragmented solutions. These penalties are equivalent to the
#' boundary length modifier (\code{BLM}) used in
#' \href{http://marxan.net}{\emph{Marxan}}. Boundary data is calculated
#' automatically for spatial planning unit data. If the planning unit data
#' in the argument to \code{x} inherits from a \code{numeric}, \code{matrix},
#' or \code{data.frame} format, the boundary data must be supplied as an
#' argument to \code{boundary_data}.
#'
#' The correct argument to \code{penalty} depends on the number of zones
#' in the argument to \code{x}. If the argument to \code{x} is associated
#' with a single zone, then the argument to \code{penalty} should be
#' a single \code{numeric} value that represents the value which planning unit
#' boundaries should be multipled by when calculating penalties for when
#' they are exposed in a solution (i.e. they have no neighbor in the solution).
#' If, on the other hand, the argument to \code{x} contains multiple zones,
#' then the argument to \code{penalty} must be \code{matrix}. Here, each row
#' and column corresponds to a different zone, and cell values represent penalty
#' multipliers. Cells that occur along the matrix diagonal
#' represent the penalty multipliers that make solutions more costly
#' when adjacent planning units are not allocated to the same zone. On the
#' other hand, cells that do not occur along the matrix diagonal represent
#' the penalty multipliers that make solutions more costly when adjacent
#' planning units are not allocated to the zones indicated by the row and
#' column index of the cell. Thus setting higher values in the matrix makes
#' the solution more spatially clumped. \strong{Note that if the cells along
#' the matrix diagonal contain values that are markedly lower than cells
#' found elsewhere in the matrix, then the optimal solution will favor
#' solutions that surround planning units with planning units that are
#' allocated to different zones.}
#'
#' To help provide a greater understanding of this penalty, here
#' we will provide the mathematical formulation for a problem containing
#' a single zone. For a given side of a planning unit (indexed by \eqn{i})
#' with a known length (\eqn{l_i}{li}) and the arguments to \code{penalty}
#' (\eqn{p}) and \code{edge_factor} (\eqn{e}), the cost incurred by this side
#'  being exposed in a solution is calculated as:
#'  \deqn{l_i \space \times p \space \times \mathit{ifelse}(
#'  \mathit{i \space overlaps \space with \space the \space side \space of
#'  \space another \space planning \space unit},
#'  1, e)}{li * p * ifelse(i overlaps with the side of another planning
#'  unit, 1, e)}
#' This function can only be used for symmetric
#' relationships between planning units; for asymmetric
#' relationships use the \code{\link{add_connectivity_penalties}} function.
#' This function is inspired by Ball \emph{et al.} (2009) and Beyer
#' \emph{et al.} (2016).
#'
#' @return \code{\link{ConservationProblem-class}} object.
#'
#' @seealso \code{\link{penalties}}.
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) \emph{Marxan and relatives:
#' Software for spatial conservation prioritisation} in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
#' conservation planning problems with integer linear programming.
#' \emph{Ecological Modelling}, 228: 14--22.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#'
#' # create minimal problem
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver()
#'
#' # create problem with low boundary penalties
#' p2 <- p1 %>% add_boundary_penalties(5, 1)
#'
#' # create problem with high boundary penalties but outer edges receive
#' # half the penalty as inner edges
#' p3 <- p1 %>% add_boundary_penalties(50, 0.5)
#' \donttest{
#' # solve problems
#' s <- stack(solve(p1), solve(p2), solve(p3))
#'
#' # plot solutions
#' plot(s, main = c("basic solution", "small penalties", "high penalties"),
#'      axes = FALSE, box = FALSE)
#' }
#' # create minimal problem with multiple zones
#' p4 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(0.2,
#'                                   nrow = n_feature(sim_features_zones),
#'                                   ncol = n_zone(sim_features_zones))) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver()
#'
#' # create boundary penalty matrix which favours clumping units
#' # togeather that are allocated to the same zone
#' pm5 <- matrix(0, n_zone(sim_features_zones), n_zone(sim_features_zones))
#' diag(pm5) <- 500
#' print(pm5)
#'
#' # create problem with the boundary penalty matrix
#' p5 <- p4 %>% add_boundary_penalties(pm5, rep(1, n_zone(sim_features_zones)))
#'
#' # create boundary penalty matrix which favours clumping units togeather
#' # that are allocated to different zones
#' pm6 <- matrix(600, n_zone(sim_features_zones), n_zone(sim_features_zones))
#' diag(pm6) <- 0
#' print(pm6)
#'
#' # create problem with the boundary penalty matrix
#' p6 <- p4 %>% add_boundary_penalties(pm6, rep(1, n_zone(sim_features_zones)))
#'
#' # create boundary penalty matrix which strongly favours clumping units
#' # togeather that are allocated to the same zone, and also slightly
#' # prefers clumping planning units togeather that are allocated
#' # to different zones
#' pm7 <- matrix(500, n_zone(sim_features_zones), n_zone(sim_features_zones))
#' diag(pm7) <- 600
#' print(pm7)
#'
#' # create problem with the boundary penalty matrix
#' p7 <- p4 %>% add_boundary_penalties(pm7, rep(1, n_zone(sim_features_zones)))
#' \donttest{
#' # solve problems
#' s2 <- stack(category_layer(solve(p4)), category_layer(solve(p5)),
#'             category_layer(solve(p6)), category_layer(solve(p7)))
#'
#' # plot solutions
#' plot(s2, main = c("basic solution", "same zone penalties",
#'                  "different zone penalties", "both zone penalties"),
#'      axes = FALSE, box = FALSE)
#' }
#' @name add_boundary_penalties
#'
#' @exportMethod add_boundary_penalties
#'
#' @aliases add_boundary_penalties,ConservationProblem,numeric-method add_boundary_penalties,ConservationProblem,matrix-method
NULL

#' @export
methods::setGeneric("add_boundary_penalties",
                    signature = methods::signature("x", "penalty"),
                    function(x, penalty, ...)
                      standardGeneric("add_boundary_penalties"))


#' @name add_boundary_penalties
#' @usage \S4method{add_boundary_penalties}{ConservationProblem,numeric}(x, penalty, edge_factor, boundary_data = NULL, ...)
#' @rdname add_boundary_penalties
methods::setMethod("add_boundary_penalties",
  methods::signature("ConservationProblem", "numeric"),
  function(x, penalty, edge_factor, boundary_data = NULL, ...) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      is.numeric(penalty) || is.matrix(penalty),
      all(is.finite(penalty)), all(c(penalty) >= 0),
       is.numeric(edge_factor), all(is.finite(edge_factor)),
      all(edge_factor >= 0), all(edge_factor <= 1))
    assertthat::assert_that(x$number_of_zones() == 1,
      msg = paste("argument to penalty must be a matrix for problems",
                  "containing multiple zones"))
    assertthat::assert_that(x$number_of_zones() == length(edge_factor),
      msg = paste("argument to edge_factor must be scalar for problems",
                  "containing a single zone"))
   # add boundary penalties
   add_boundary_penalties(x, penalty = matrix(penalty),
                          edge_factor = edge_factor,
                          boundary_data = boundary_data)
})

#' @name add_boundary_penalties
#' @usage \S4method{add_boundary_penalties}{ConservationProblem,matrix}(x, penalty, edge_factor, boundary_data = NULL, ...)
#' @rdname add_boundary_penalties
methods::setMethod("add_boundary_penalties",
  methods::signature("ConservationProblem", "matrix"),
  function(x, penalty, edge_factor, boundary_data = NULL, ...) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      is.matrix(penalty), all(is.finite(penalty)), all(penalty >= 0),
      ncol(penalty) == x$number_of_zones(),
      nrow(penalty) == x$number_of_zones(),
      isSymmetric(penalty),
      is.numeric(edge_factor), all(is.finite(edge_factor)),
      all(edge_factor >= 0), all(edge_factor <= 1),
      length(edge_factor) == x$number_of_zones())
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
    # add row names and column names to penalty matrix
    rownames(penalty) <- x$zone_names()
    colnames(penalty) <- rownames(penalty)
    # define function to validate changes to the penalty matrix
    vfun <- function(m) {
      m <- as.matrix(m)
      assertthat::see_if(is.matrix(m), all(is.finite(m)), all(m >= 0),
                         ncol(m) == x$number_of_zones(),
                         nrow(m) == x$number_of_zones(),
                         isSymmetric(m))
    }
    # define function to render targets object
    rfun <- function(x)
      getFromNamespace("rhandsontable", "rHandsontableOutput")(as.data.frame(x))
    # create new constraint object
    x$add_penalty(pproto(
      "BoundaryPenalty",
      Penalty,
      name = "Boundary penalties",
      data = d,
      parameters = parameters(misc_parameter("penalty", penalty, vfun, rfun),
                              proportion_parameter_array("edge factor",
                                                         edge_factor,
                                                         x$zone_names())),
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
        if (any(c(self$parameters$get("penalty")) > 1e-50)) {
          m <- y$get_data("boundary_matrix")
          class(m) <- "dgCMatrix"
          rcpp_apply_symmetric_boundary_constraints(x$ptr, m,
            self$parameters$get("penalty"),
            self$parameters$get("edge factor")[[1]])
        }
        invisible(TRUE)
      }))
})
