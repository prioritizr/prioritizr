#' @include internal.R Penalty-class.R marxan_boundary_data_to_matrix.R
NULL

#' Add boundary penalties
#'
#' Add penalties to a conservation planning problem to favor solutions
#' that spatially clump planning units together based on the overall
#' boundary length (i.e., total perimeter).
#'
#' @param x [problem()] object.
#'
#' @param penalty `numeric` penalty that is used to scale the importance
#'   of selecting planning units that are spatially clumped together compared
#'   to the main problem objective (e.g., solution cost when the argument to
#'   `x` has a minimum set objective per [add_min_set_objective()]).
#'   Higher `penalty` values prefer solutions with a higher degree of spatial
#'   clumping, and smaller `penalty` values prefer solutions with a smaller
#'   degree of clumping. Note that negative `penalty` values prefer
#'   solutions that are more spread out. This parameter is equivalent to
#'   the boundary length modifier (BLM)
#'   parameter in [*Marxan*](https://marxansolutions.org/).
#'
#' @param edge_factor `numeric` proportion to scale
#'   planning unit edges (borders) that do not have any neighboring planning
#'   units. For example, an edge factor of `0.5` is commonly used to
#'   avoid overly penalizing planning units along a coastline.
#'   Note that this argument must have an element for each zone in the argument
#'   to `x`.
#'
#' @param formulation `character` value denoting the name of the linearization
#'  technique used to formulate the penalties. Available options are `"simple"`
#'  and `"knapsack"`. Defaults to `"simple"`. If you are having issues with
#'  solving problems within a reasonable period of time using the `"simple"`
#'  technique, then consider trying the `"knapsack"` technique.
#'  Note that the `"knapsack"` technique is likely to yield better
#'  performance when each planning unit has a large number of neighbors
#'  (e.g., planning units are based on a hexagonal grid or irregular polygons).
#'
#' @param zones `matrix` or `Matrix` object describing the
#'   clumping scheme for different zones. Each row and column corresponds to a
#'   different zone in the argument to `x`, and cell values indicate the
#'   relative importance of clumping planning units that are allocated to
#'   a combination of zones. Cell values along the diagonal of the matrix
#'   represent the relative importance of clumping planning units that are
#'   allocated to the same zone. Cell values must range between 1 and -1, where
#'   negative values favor solutions that spread out planning units. The default
#'   argument to `zones` is an identity
#'   matrix (i.e., a matrix with ones along the matrix diagonal and zeros
#'   elsewhere), so that penalties are incurred when neighboring planning units
#'   are not assigned to the same zone. If the cells along
#'   the matrix diagonal contain markedly smaller values than those found
#'   elsewhere in the matrix, then solutions are preferred that surround
#'   planning units with those allocated to different zones
#'   (i.e., greater spatial fragmentation).
#'
#' @param data `NULL`, `data.frame`, `matrix`, or `Matrix`
#'   object containing the boundary data. These data describe the total
#'   amount of boundary (perimeter) length  for each planning unit,
#'   and the amount of boundary (perimeter) length shared between different
#'   planning units (i.e., planning units that are adjacent to each other).
#'   See the Data format section for more information.
#'
#' @details
#' This function adds penalties to a conservation planning problem
#' to penalize fragmented solutions. It was is inspired by Ball *et al.*
#' (2009) and Beyer *et al.* (2016). The `penalty` argument is
#' equivalent to the boundary length modifier (`BLM`) used in
#' [*Marxan*](https://marxansolutions.org).
#' Note that this function can only
#' be used to represent symmetric relationships between planning units. If
#' asymmetric relationships are required, use the
#' [add_connectivity_penalties()] function.
#'
#' @section Data format:
#' The argument to `data` can be specified using the following formats.
#' Note that boundary data must always describe symmetric relationships
#' between planning units.
#'
#' \describe{
#'
#' \item{`data` as a `NULL` value}{indicating that the data should be
#'   automatically calculated using the [boundary_matrix()] function.
#'   This argument is the default.
#'   Note that the boundary data must be supplied
#'   using one of the other formats below if the planning unit data
#'   in the argument to `x` do not explicitly contain spatial information
#'   (e.g., planning unit data are a `data.frame` or `numeric` class).}
#'
#' \item{`data` as a `matrix`/`Matrix` object}{where rows and columns represent
#'   different planning units and the value of each cell represents the
#'   amount of shared boundary length between two different planning units.
#'   Cells that occur along the matrix diagonal denote the total
#'   boundary length associated with each planning unit.}
#'
#' \item{`data` as a `data.frame` object}{with the columns `"id1"`,
#'   `"id2"`, and `"boundary"`. The `"id1"` and `"id2"` columns contain
#'   identifiers (indices) for a pair of planning units, and the `"boundary"`
#'   column contains the amount of shared boundary length between these
#'   two planning units.
#'   Additionally, if the values in the `"id1"` and `"id2"` columns
#'   contain the same values, then the value denotes the
#'   amount of exposed boundary length (not total boundary).
#'   This format follows the the standard *Marxan* format for boundary
#'   data (i.e., per the "bound.dat" file).}
#'
#' }
#'
#' @section Mathematical formulation:
#' The boundary penalties are implemented using the following equations. Let
#' \eqn{I} represent the set of planning units
#' (indexed by \eqn{i} or \eqn{j}), \eqn{Z} represent
#' the set of management zones (indexed by \eqn{z} or \eqn{y}), and
#' \eqn{X_{iz}}{Xiz} represent the decision
#' variable for planning unit \eqn{i} for in zone \eqn{z} (e.g., with binary
#' values one indicating if planning unit is allocated or not). Also, let
#' \eqn{p} represent the argument to `penalty`, \eqn{E_z}{Ez} represent the
#' argument to `edge_factor`, \eqn{B_{ij}}{Bij} represent the matrix argument
#' to `data` (e.g., generated using [boundary_matrix()]), and
#' \eqn{W_{zz}}{Wzz} represent the matrix argument to `zones`.
#'
#' \deqn{
#' \sum_{i}^{I} \sum_{z}^{Z} (p \times W_{zz} B_{ii}) +
#' \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z}
#' \sum_{y}^{Z} (-2 \times p \times X_{iz} \times X_{jy} \times W_{zy} \times
#' B_{ij})}{
#' sum_i^I sum_z^Z (p * Wzz * Bii) + sum_i^I
#' sum_j^I sum_z^Z sum_y^Z (-2 * p * Xiz * Xjy * Wzy * Bij)
#' }
#'
#' Note that when the problem objective is to maximize some measure of
#' benefit and not minimize some measure of cost, the term \eqn{p} is
#' replaced with \eqn{-p}. Additionally, to linearize the problem,
#' the expression \eqn{X_{iz} \times X_{jy}}{Xiz * Xjy} is modeled using
#' a set of continuous variables (bounded between 0 and 1) based on
#' Beyer *et al.* (2016).
#'
#' @return An updated [problem()] object with the penalties added to it.
#'
#' @seealso
#' See [penalties] for an overview of all functions for adding penalties.
#' Also see [add_neighbor_penalties()] for a penalty that
#' can reduce spatial fragmentation and has faster solver run times.
#' Additionally, see [calibrate_cohon_penalty()] for assistance with selecting
#' an appropriate `penalty` value.
#'
#' @family penalties
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) *Marxan and relatives:
#' Software for spatial conservation prioritisation* in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
#' conservation planning problems with integer linear programming.
#' *Ecological Modelling*, 228: 14--22.
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
#' # create problem with low boundary penalties
#' p2 <- p1 %>% add_boundary_penalties(50, 1)
#'
#' # create problem with high boundary penalties but outer edges receive
#' # half the penalty as inner edges
#' p3 <- p1 %>% add_boundary_penalties(500, 0.5)
#'
#' # create a problem using precomputed boundary data
#' bmat <- boundary_matrix(sim_pu_raster)
#' p4 <- p1 %>% add_boundary_penalties(50, 1, data = bmat)
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
#' # create problem with the zone matrix and low penalties
#' p6 <- p5 %>% add_boundary_penalties(50, zone = zm6)
#'
#' # create another problem with the same zone matrix and higher penalties
#' p7 <- p5 %>% add_boundary_penalties(500, zone = zm6)
#'
#' # create zone matrix which favors clumping units that are allocated to
#' # different zones together
#' zm8 <- matrix(1, ncol = 3, nrow = 3)
#' diag(zm8) <- 0
#' print(zm8)
#'
#' # create problem with the zone matrix
#' p8 <- p5 %>% add_boundary_penalties(500, zone = zm8)
#'
#' # create zone matrix which strongly favors clumping units
#' # that are allocated to the same zone together. It will also prefer
#' # clumping planning units in zones 1 and 2 together over having
#' # these planning units with no neighbors in the solution
#' zm9 <- diag(3)
#' zm9[upper.tri(zm9)] <- c(0.3, 0, 0)
#' zm9[lower.tri(zm9)] <- zm9[upper.tri(zm9)]
#' print(zm9)
#'
#' # create problem with the zone matrix
#' p9 <- p5 %>% add_boundary_penalties(500, zone = zm9)
#'
#' # create zone matrix which favors clumping planning units in zones 1 and 2
#' # together, and favors planning units in zone 3 being spread out
#' # (i.e., negative clumping)
#' zm10 <- diag(3)
#' zm10[3, 3] <- -1
#' print(zm10)
#'
#' # create problem with the zone matrix
#' p10 <- p5 %>% add_boundary_penalties(500, zone = zm10)
#'
#' # solve problems
#' s2 <- list(solve(p5), solve(p6), solve(p7), solve(p8), solve(p9), solve(p10))
#'
#' #convert to category layers for visualization
#' s2 <- terra::rast(lapply(s2, category_layer))
#' names(s2) <- c(
#'   "basic solution", "within zone clumping (low)",
#'   "within zone clumping (high)", "between zone clumping",
#'   "within + between clumping", "negative clumping"
#' )
#'
#' # plot solutions
#' plot(s2, axes = FALSE)
#' }
#'
#' @name add_boundary_penalties
#'
#' @aliases add_boundary_penalties,ConservationProblem,ANY,ANY,ANY,ANY,array-method add_boundary_penalties,ConservationProblem,ANY,ANY,ANY,ANY,matrix-method add_boundary_penalties,ConservationProblem,ANY,ANY,ANY,ANY,data.frame-method add_boundary_penalties,ConservationProblem,ANY,ANY,ANY,ANY,ANY-method
NULL

#' @export
methods::setGeneric("add_boundary_penalties",
  signature = methods::signature(
    "x", "penalty", "edge_factor", "formulation", "zones", "data"
  ),
  function(
    x, penalty, edge_factor = rep(0.5, number_of_zones(x)),
    formulation = "simple",
    zones = diag(number_of_zones(x)), data = NULL
  ) {
    assert_required(x)
    assert_required(penalty)
    assert_required(edge_factor)
    assert_required(formulation)
    assert_required(zones)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      is_inherits(data, c("NULL", "matrix", "Matrix", "data.frame"))
    )
    standardGeneric("add_boundary_penalties")
  }
)

#' @name add_boundary_penalties
#' @usage \S4method{add_boundary_penalties}{ConservationProblem,ANY,ANY,ANY,ANY,data.frame}(x, penalty, edge_factor, formulation, zones, data)
#' @rdname add_boundary_penalties
methods::setMethod("add_boundary_penalties",
  methods::signature(
    "ConservationProblem", "ANY", "ANY", "ANY", "ANY", "data.frame"
  ),
  function(x, penalty, edge_factor, formulation, zones, data) {
    # add constraints
    add_boundary_penalties(
      x, penalty, edge_factor, formulation, zones,
      internal_marxan_boundary_data_to_matrix(x, data)
    )
  }
)

#' @name add_boundary_penalties
#' @usage \S4method{add_boundary_penalties}{ConservationProblem,ANY,ANY,ANY,ANY,matrix}(x, penalty, edge_factor, formulation, zones, data)
#' @rdname add_boundary_penalties
methods::setMethod("add_boundary_penalties",
  methods::signature(
    "ConservationProblem", "ANY", "ANY", "ANY", "ANY", "matrix"
  ),
  function(x, penalty, edge_factor, formulation, zones, data) {
    # add constraints
    add_boundary_penalties(
      x, penalty, edge_factor, formulation, zones, as_Matrix(data, "dgCMatrix")
    )
  }
)

#' @name add_boundary_penalties
#' @usage \S4method{add_boundary_penalties}{ConservationProblem,ANY,ANY,ANY,ANY,ANY}(x, penalty, edge_factor, zones, data)
#' @rdname add_boundary_penalties
methods::setMethod("add_boundary_penalties",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY", "ANY", "ANY"),
  function(x, penalty, edge_factor, formulation, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is_inherits(data, c("NULL", "Matrix")),
      assertthat::is.number(penalty),
      all_finite(penalty),
      is.numeric(edge_factor),
      all_finite(edge_factor),
      all_proportion(edge_factor),
      assertthat::is.string(formulation),
      assertthat::noNA(formulation),
      is_match_of(formulation, c("simple", "knapsack")),
      length(edge_factor) == number_of_zones(x),
      is_matrix_ish(zones),
      all_finite(zones)
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
      # assert valid arguments for knapsack formulation
      if (identical(formulation, "knapsack")) {
        assert(
          all_positive(penalty),
          msg = paste(
            "{.arg penalty} must be a positive value",
            "when using {.code formulation = \"knapsack\"}."
          )
        )
        assert(
          all_positive(zones),
          msg = paste(
            "{.arg zones} must contain only positive values",
            "when using {.code formulation = \"knapsack\"}."
          )
        )
        assert(
          all_positive(data),
          msg = paste(
            "{.arg data} must contain only positive values",
            "when using {.code formulation = \"knapsack\"}."
          )
        )
      }
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
      min(zones) >= -1,
      max(zones) <= 1
    )
    colnames(zones) <- x$zone_names()
    rownames(zones) <- colnames(zones)
    # create new constraint object
    x$add_penalty(
      R6::R6Class(
        "BoundaryPenalty",
        inherit = Penalty,
        public = list(
          name = "boundary penalties",
          data = list(
            penalty = penalty,
            edge_factor = edge_factor,
            formulation = formulation,
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
            form <- self$get_data("formulation")
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
            if (abs(p) > 1e-50) {
              # apply penalties
              if (identical(form, "knapsack")) {
                rcpp_apply_boundary_penalties2(
                  x$ptr,
                  p,
                  self$get_data("edge_factor"),
                  self$get_data("zones"),
                  bm,
                  exposed_boundary,
                  total_boundary
                )
              } else {
                rcpp_apply_boundary_penalties(
                  x$ptr,
                  p,
                  self$get_data("edge_factor"),
                  self$get_data("zones"),
                  bm,
                  exposed_boundary,
                  total_boundary
                )
              }
            }
            # return success
            invisible(TRUE)
          }
        )
      )$new()
    )
  }
)
