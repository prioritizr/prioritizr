#' @include internal.R Penalty-class.R marxan_boundary_data_to_matrix.R
NULL

#' Add surrounded penalties
#'
#' Add penalties to a conservation planning problem to favor solutions
#' that spatially clump planning units together based on spatial
#' boundary of selected planning units that are fully surrounded by
#' other selected planning units.
#'
#' @param penalty `numeric` penalty that is used to scale the importance
#'   of selecting planning units that are spatially clumped together compared
#'   to the main problem objective (e.g., solution cost when the argument to
#'   `x` has a minimum set objective per [add_min_set_objective()]).
#'   Higher `penalty` values prefer solutions with a higher degree of spatial
#'   clumping, and smaller `penalty` values prefer solutions with a smaller
#'   degree of clumping. Note that negative `penalty` values are not permitted.
#'
#' @inheritParams add_neighbor_constraints
#'
#' @details
#' This function adds penalties to a conservation planning problem
#' to penalize fragmented solutions.
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
#' The surrounded penalties are implemented using the following equations. Let
#' \eqn{I} represent the set of planning units
#' (indexed by \eqn{i} or \eqn{j}), \eqn{Z} represent
#' the set of management zones (indexed by \eqn{z} or \eqn{y}), and
#' \eqn{X_{iz}}{Xiz} represent the decision
#' variable for planning unit \eqn{i} for in zone \eqn{z} (e.g., with binary
#' values one indicating if planning unit is allocated or not). Also, let
#' \eqn{p} represent the argument to `penalty`,
#' \eqn{A_{ij}}{Aij} indicate if planning units \eqn{i} and \eqn{j} are
#' neighbors (using values of zero or one) based on the matrix argument
#' to `data` (e.g., generated using [boundary_matrix()]), and
#' \eqn{W_{zz}}{Wzz} represent the matrix argument to `zones`..
#' Additionally, let \eqn{T_i} denote the total boundary for planning unit
#' \eqn{i}, and \eqn{S_{iz}} indicate if planning unit \eqn{i} is selected
#' and fully surrounded or not by other selected planning units.
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{z}^{Z} -p \times S_{iz} \\
#' ((1 + T_{i}) \times S_{iz}) - X_{iz} - \sum_{j}^{I} \sum_{y}^{Z} W_{zy} \\ \times A_{ij} \times X_{jy} \geq 0 \\
#' T_{i} = \sum_{j}^{I} A_{ij}
#' }{
#' sum_i^I sum_z^Z p * Siz
#' ((1 + Ti) * Siz) - Xiz - sum_j^I sum_y^Z Wzy * Aij * Xjy >= 0
#' Ti = sum_i^I Aij
#' }
#'
#' Note that when the problem objective is to maximize some measure of
#' benefit and not minimize some measure of cost, the term \eqn{-p} is
#' replaced with \eqn{p}.
#'
#' @inherit add_boundary_penalties return seealso
#'
#' @family penalties
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
#' # define budget as 40% of total costs
#' b = terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.4
#'
#' # create minimal problem
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_shortfall_objective(budget = b) %>%
#'   add_relative_targets(0.8) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with low surrounded penalties
#' p2 <- p1 %>% add_surrounded_penalties(0.0001)
#'
#' # create problem with high surrounded penalties
#' p3 <- p1 %>% add_surrounded_penalties(0.05)
#'
#' # create a problem using precomputed boundary data
#' bmat <- boundary_matrix(sim_pu_raster)
#' p4 <- p1 %>% add_surrounded_penalties(0.05, data = bmat)
#'
#' # solve problems
#' s1 <- c(solve(p1), solve(p2), solve(p3), solve(p4))
#' names(s1) <- c("basic solution", "small penalties", "high penalties",
#'   "precomputed data"
#' )
#'
#' # plot solutions
#' plot(s1, axes = FALSE)
#'
#'
#' # compute budget for multi-zone example
#' b2 <- terra::global(sim_zones_pu_raster, "sum", na.rm = TRUE)[[1]] * 0.4
#' # create minimal problem with multiple zones and limit the run-time for
#' # solver to 10 seconds so this example doesn't take too long
#' p5 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_shortfall_objective(budget = b2) %>%
#'   add_relative_targets(matrix(0.8, nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(time_limit = 10, verbose = FALSE)
#'
#' # create zone matrix which favors clumping planning units that are
#' # allocated to the same zone together - note that this is the default
#' zm6 <- diag(3)
#' print(zm6)
#'
#' # create problem with the zone matrix and low penalties
#' p6 <- p5 %>% add_surrounded_penalties(0.00001, zone = zm6)
#'
#' # create another problem with the same zone matrix and higher penalties
#' p7 <- p5 %>% add_surrounded_penalties(0.05, zone = zm6)
#'
#' # solve problems
#' s2 <- list(solve(p5), solve(p6), solve(p7))
#'
#' # convert to category layers for visualization
#' s2 <- terra::rast(lapply(s2, category_layer))
#' names(s2) <- c(
#'   "basic solution", "within zone clumping (low)",
#'   "within zone clumping (high)"
#' )
#'
#' # plot solutions
#' plot(s2, axes = FALSE)
#' }
#'
#' @name add_surrounded_penalties
#'
#' @aliases add_surrounded_penalties,ConservationProblem,ANY,ANY,array-method add_surrounded_penalties,ConservationProblem,ANY,ANY,matrix-method add_surrounded_penalties,ConservationProblem,ANY,ANY,data.frame-method add_surrounded_penalties,ConservationProblem,ANY,ANY,ANY-method
NULL

#' @export
methods::setGeneric("add_surrounded_penalties",
  signature = methods::signature(
    "x", "penalty", "zones", "data"
  ),
  function(
    x, penalty,
    zones = diag(number_of_zones(x)), data = NULL
  ) {
    assert_required(x)
    assert_required(penalty)
    assert_required(zones)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      is_inherits(data, c("NULL", "matrix", "Matrix", "data.frame"))
    )
    standardGeneric("add_surrounded_penalties")
  }
)

#' @name add_surrounded_penalties
#' @usage \S4method{add_surrounded_penalties}{ConservationProblem,ANY,ANY,data.frame}(x, penalty, zones, data)
#' @rdname add_surrounded_penalties
methods::setMethod("add_surrounded_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "data.frame"),
  function(x, penalty, zones, data) {
    # add constraints
    add_surrounded_penalties(
      x, penalty, zones,
      internal_marxan_boundary_data_to_matrix(x, data)
    )
  }
)

#' @name add_surrounded_penalties
#' @usage \S4method{add_surrounded_penalties}{ConservationProblem,ANY,ANY,matrix}(x, penalty, zones, data)
#' @rdname add_surrounded_penalties
methods::setMethod("add_surrounded_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "matrix"),
  function(x, penalty, zones, data) {
    # add constraints
    add_surrounded_penalties(
      x, penalty, zones, as_Matrix(data, "dgCMatrix")
    )
  }
)

#' @name add_surrounded_penalties
#' @usage \S4method{add_surrounded_penalties}{ConservationProblem,ANY,ANY,ANY}(x, penalty, zones, data)
#' @rdname add_surrounded_penalties
methods::setMethod("add_surrounded_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY"),
  function(x, penalty, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is_inherits(data, c("NULL", "Matrix")),
      assertthat::is.number(penalty),
      all_finite(penalty),
      is_matrix_ish(zones),
      all_finite(zones)
    )
    if (!is.null(data)) {
      # round data to avoid numerical precision issues
      data <- round(data, 6)
      # assert no negative values
      assert(
        all(data@x >= 0),
        msg = "{.arg data} must have a positive boundary values."
      )
      # check argument to data if not NULL
      assert(
        ncol(data) == nrow(data),
        number_of_total_units(x) == ncol(data),
        is_numeric_values(data),
        all_finite(data),
        Matrix::isSymmetric(data)
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
    # verify no negative values
    assert(
      penalty >= 0,
      msg = "{.arg penalty} must have a positive value."
    )
    assert(
      all(c(zones) >= 0),
      msg = "{.arg zones} must have a positive values."
    )
    # create new constraint object
    x$add_penalty(
      R6::R6Class(
        "SurroundedPenalty",
        inherit = Penalty,
        public = list(
          name = "surrounded penalties",
          data = list(
            penalty = penalty,
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
            p <- self$get_data("penalty")
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
            bm@x <- as.numeric(bm@x > 0.5)
            # apply constraint
            if (abs(p) > 1e-50) {
              # apply penalties
              rcpp_apply_surrounded_penalties(
                x$ptr, p, self$get_data("zones"), bm
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
