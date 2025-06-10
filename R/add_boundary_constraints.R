#' @include internal.R Constraint-class.R marxan_boundary_data_to_matrix.R
NULL

#' Add boundary constraints
#'
#' Add constraints to a conservation planning problem to favor solutions
#' that spatially clump planning units together based on the overall
#' boundary length (i.e., total perimeter).
#'
#' @param threshold `numeric` value that is used to scale the importance
#'   of selecting planning units that are spatially clumped together compared
#'   to the main problem objective (e.g., solution cost when the argument to
#'   `x` has a minimum set objective per [add_min_set_objective()]).
#'   The `threshold` value must range between 0 and 1, with higher values
#'   resulting in solutions with a higher degree of spatial
#'   clumping, and smaller values prefer solutions with a smaller
#'   degree of clumping.
#'   Note that specifying a `threshold` value that is too high may result
#'   in problem infeasibility and, in such cases, a smaller value should
#'   be attempted.
#'   Although `threshold` has a theoretical upper bound of 1,
#'   in practice, the maximum value that will result in feasible solutions
#'   will likely be between 0.8 and 0.95.
#'   Additionally, the calculations that underpin this constraint are
#'   different to those for the [add_boundary_penalties()], and so
#'   the `threshold` parameter for this function and the `penalty`
#'   parameter for [add_boundary_penalties()] are not similar to each other.
#'
#' @inheritParams add_boundary_penalties
#'
#' @param zones `matrix` or `Matrix` object describing the
#'   clumping scheme for different zones. Each row and column corresponds to a
#'   different zone in the argument to `x`, and cell values indicate the
#'   relative importance of clumping planning units that are allocated to
#'   a combination of zones. Cell values along the diagonal of the matrix
#'   represent the relative importance of clumping planning units that are
#'   allocated to the same zone. Cell values must range between 1 and 0.
#'   The default
#'   argument to `zones` is an identity
#'   matrix (i.e., a matrix with ones along the matrix diagonal and zeros
#'   elsewhere), so that exposed boundaries are calculated when neighboring
#'   planning units are not assigned to the same zone. If the cells along
#'   the matrix diagonal contain markedly smaller values than those found
#'   elsewhere in the matrix, then solutions are preferred that surround
#'   planning units with those allocated to different zones
#'   (i.e., greater spatial fragmentation).
#'
#' @details
#' This function adds constraints to a conservation planning problem
#' to avoid fragmented solutions.
#' Note that this function can only
#' be used to represent symmetric relationships between planning units. If
#' asymmetric relationships are required, use the
#' [add_connectivity_penalties()] function.
#' Additionally, this function cannot be used to promote the selection
#' of highly fragmented solutions (unlike [add_boundary_penalties()]).
#'
#' @inheritSection add_boundary_penalties Data format
#'
#' @section Mathematical formulation:
#' The boundary constraints are implemented using the following equations. Let
#' \eqn{I} represent the set of planning units
#' (indexed by \eqn{i} or \eqn{j}), \eqn{Z} represent
#' the set of management zones (indexed by \eqn{z} or \eqn{y}), and
#' \eqn{X_{iz}}{Xiz} represent the decision
#' variable for planning unit \eqn{i} for in zone \eqn{z} (e.g., with binary
#' values one indicating if planning unit is allocated or not). Also, let
#' \eqn{t} represent the argument to `threshold`,
#' \eqn{B_{ij}}{Bij} represent the matrix argument
#' to `data` (e.g., generated using [boundary_matrix()]), and
#' \eqn{W_{zz}}{Wzz} represent the matrix argument to `zones`..
#' Additionally, let \eqn{T_i} denote the total boundary for planning unit
#' \eqn{i}, \eqn{E_{iz}} indicate the proportion of the perimeter for
#' planning unit \eqn{i} that is surrounded by other selected planning units.
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{z}^{Z} (-t \times E_{iz}) + X_{iz} \geq 0 \\
#' T_{i} = \sum_{j}^{J} B_{ij} \\
#' E_{iz} \leq X_{iz} \\
#' (T_i \times E_{iz}) - \sum_{j}^{I} \sum_{y}^{Z} W_{zy} \times B_{ij} \times X_{jy} \geq 0
#' }{
#' sum_i^I sum_z^Z (-t * Eiz) + Xiz >= 0
#' Ti = sum_j^J Bij
#' Eiz <= Xiz
#' (Ti \times Eiz) - sum_j^I sum_y^Z Wzy * Bij * Xjy >= 0
#' }
#'
#' Note that when the problem objective is to maximize some measure of
#' benefit and not minimize some measure of cost, the term \eqn{t} is
#' replaced with \eqn{t}.
#'
#' @inherit add_locked_in_constraints return seealso
#'
#' @family constraints
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
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
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with low boundary constraints threshold
#' p2 <- p1 %>% add_boundary_constraints(0.4, 1)
#'
#' # create problem with high boundary constraints threshold
#' # but outer edges receive half the value as inner edges
#' p3 <- p1 %>% add_boundary_constraints(0.8, 0.5)
#'
#' # create a problem using precomputed boundary data
#' bmat <- boundary_matrix(sim_pu_raster)
#' p4 <- p1 %>% add_boundary_constraints(0.8, 1, data = bmat)
#'
#' # solve problems
#' s1 <- c(solve(p1), solve(p2), solve(p3), solve(p4))
#' names(s1) <- c("basic solution", "small threshold", "high threshold",
#'   "precomputed data"
#' )
#'
#' # plot solutions
#' plot(s1, axes = FALSE)
#'
#' # create minimal problem with multiple zones and limit the run-time for
#' # solver to 10 seconds so this example doesn't take too long
#' p5 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(time_limit = 10, verbose = FALSE)
#'
#' # create zone matrix which favors clumping planning units that are
#' # allocated to the same zone together - note that this is the default
#' zm6 <- diag(3)
#' print(zm6)
#'
#' # create problem with the zone matrix and low constraints threshold
#' p6 <- p5 %>% add_boundary_constraints(0.2, zone = zm6)
#'
#' # create another problem with the same zone matrix and higher
#' # constraints threshold
#' p7 <- p5 %>% add_boundary_constraints(0.6, zone = zm6)
#'
#' # create zone matrix which strongly favors clumping units
#' # that are allocated to the same zone together. It will also prefer
#' # clumping planning units in zones 1 and 2 together over having
#' # these planning units with no neighbors in the solution
#' zm8 <- diag(3)
#' zm8[upper.tri(zm8)] <- c(0.3, 0, 0)
#' zm8[lower.tri(zm8)] <- zm8[upper.tri(zm8)]
#' print(zm8)
#'
#' # create problem with the zone matrix
#' p8 <- p5 %>% add_boundary_constraints(0.6, zone = zm8)
#'
#' # solve problems
#' s2 <- list(solve(p5), solve(p6), solve(p7), solve(p8))
#'
#' # convert to category layers for visualization
#' s2 <- terra::rast(lapply(s2, category_layer))
#' names(s2) <- c(
#'   "basic solution", "within zone clumping (low)",
#'   "within zone clumping (high)", "within + between clumping"
#' )
#'
#' # plot solutions
#' plot(s2, axes = FALSE)
#' }
#'
#' @name add_boundary_constraints
#'
#' @aliases add_boundary_constraints,ConservationProblem,ANY,ANY,ANY,array-method add_boundary_constraints,ConservationProblem,ANY,ANY,ANY,matrix-method add_boundary_constraints,ConservationProblem,ANY,ANY,ANY,data.frame-method add_boundary_constraints,ConservationProblem,ANY,ANY,ANY,ANY-method
NULL

#' @export
methods::setGeneric("add_boundary_constraints",
  signature = methods::signature(
    "x", "threshold", "edge_factor", "zones", "data"
  ),
  function(
    x, threshold, edge_factor = rep(0.5, number_of_zones(x)),
    zones = diag(number_of_zones(x)), data = NULL
  ) {
    assert_required(x)
    assert_required(threshold)
    assert_required(edge_factor)
    assert_required(zones)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      is_inherits(data, c("NULL", "matrix", "Matrix", "data.frame"))
    )
    standardGeneric("add_boundary_constraints")
  }
)

#' @name add_boundary_constraints
#' @usage \S4method{add_boundary_constraints}{ConservationProblem,ANY,ANY,ANY,data.frame}(x, threshold, edge_factor, zones, data)
#' @rdname add_boundary_constraints
methods::setMethod("add_boundary_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY", "data.frame"),
  function(x, threshold, edge_factor, zones, data) {
    # add constraints
    add_boundary_constraints(
      x, threshold, edge_factor, zones,
      internal_marxan_boundary_data_to_matrix(x, data)
    )
  }
)

#' @name add_boundary_constraints
#' @usage \S4method{add_boundary_constraints}{ConservationProblem,ANY,ANY,ANY,matrix}(x, threshold, edge_factor, zones, data)
#' @rdname add_boundary_constraints
methods::setMethod("add_boundary_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY", "matrix"),
  function(x, threshold, edge_factor, zones, data) {
    # add constraints
    add_boundary_constraints(
      x, threshold, edge_factor, zones, as_Matrix(data, "dgCMatrix")
    )
  }
)

#' @name add_boundary_constraints
#' @usage \S4method{add_boundary_constraints}{ConservationProblem,ANY,ANY,ANY,ANY}(x, threshold, edge_factor, zones, data)
#' @rdname add_boundary_constraints
methods::setMethod("add_boundary_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY", "ANY"),
  function(x, threshold, edge_factor, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is_inherits(data, c("NULL", "Matrix")),
      assertthat::is.number(threshold),
      all_finite(threshold),
      is.numeric(edge_factor),
      all_finite(edge_factor),
      all_proportion(edge_factor),
      length(edge_factor) == number_of_zones(x),
      is_matrix_ish(zones),
      all_finite(zones)
    )
    assert(
      threshold >= 0,
      threshold <= 1,
      msg = "{.arg threshold} must have a value between 0 and 1."
    )
    if (!is.null(data)) {
      # round data to avoid numerical precision issues
      data <- round(data, 6)
      # check argument to data if not NULL
      assert(
        ncol(data) == nrow(data),
        number_of_total_units(x) == ncol(data),
        is_numeric_values(data),
        all_finite(data),
        Matrix::isSymmetric(data)
      )
      # assert no negative values
      assert(
        all(data@x >= 0),
        msg = "{.arg data} must have a positive boundary values."
      )
      # verify diagonal is >= edge lengths
      verify(
        all(
          round(
            Matrix::diag(data) - (Matrix::rowSums(data) - Matrix::diag(data)),
            6
          ) >= -1e-5
        ),
        msg = c(
          "{.arg data} has unexpected values.",
          "x" = paste(
            "If {.arg data} is from an older version of",
            "{.pkg prioritizr}, then use",
            "{.fn boundary_matrix} to recreate it."
          ),
          "i" = paste(
            "{.arg x} might have spatially overlapping planning units."
          )
        )
      )
    } else {
      # check that planning unit data is spatially referenced
      assert(
        is_pu_spatially_explicit(x),
        msg = c(
          paste(
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
    # convert zones to matrix
    zones <- as.matrix(zones)
    assert(
      isSymmetric(zones),
      ncol(zones) == number_of_zones(x),
      min(zones) >= 0,
      max(zones) <= 1
    )
    colnames(zones) <- x$zone_names()
    rownames(zones) <- colnames(zones)
    # create new constraint object
    x$add_constraint(
      R6::R6Class(
        "BoundaryConstraint",
        inherit = Constraint,
        public = list(
          name = "boundary constraints",
          data = list(
            threshold = threshold,
            edge_factor = edge_factor,
            data = data,
            zones = zones
          ),
          calculate = function(x) {
            # assert valid argument
            assert(is_conservation_problem(x), .internal = TRUE)
            # if needed, calculate boundary matrix
            if (
              is.null(self$get_data("data")) &&
              is.Waiver(x$get_data("boundary"))
            ) {
              x$set_data("boundary", boundary_matrix(x$get_data("cost")))
            }
            # return invisible success
            invisible()
          },
          apply = function(x, y) {
            # assert valid arguments
            assert(
              inherits(x, "OptimizationProblem"),
              inherits(y, "ConservationProblem"),
              .internal = TRUE
            )
            # extract data
            tr <- self$get_data("threshold")
            bm <- self$get_data("data")
            if (is.null(bm)) {
              bm <- y$get_data("boundary")
            }
            # convert to dgCMatrix
            bm <- as_Matrix(bm, "dgCMatrix")
            # subset data to planning units
            ind <- y$planning_unit_indices()
            bm <- bm[ind, ind]
            # compute additional boundary information
            total_boundary <- Matrix::diag(bm)
            exposed_boundary <-
              Matrix::diag(bm) - (Matrix::rowSums(bm) - Matrix::diag(bm))
            # prepare boundary data
            Matrix::diag(bm) <- 0
            bm <- as_Matrix(Matrix::tril(Matrix::drop0(bm)), "dgCMatrix")
            # apply constraint
            if (abs(tr) > 1e-50) {
              # apply constraints
              rcpp_apply_boundary_constraints(
                x$ptr,
                tr,
                self$get_data("edge_factor"),
                self$get_data("zones"),
                bm,
                exposed_boundary,
                total_boundary
              )
            }
            # return success
            invisible(TRUE)
          }
        )
      )$new()
    )
  }
)
