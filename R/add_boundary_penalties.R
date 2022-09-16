#' @include internal.R Penalty-proto.R marxan_boundary_data_to_matrix.R
NULL

#' Add boundary penalties
#'
#' Add penalties to a conservation planning [problem()] to favor solutions
#' that spatially clump planning units together based on the overall
#' boundary length (perimeter).
#'
#' @param x [problem()] (i.e., [`ConservationProblem-class`]) object.
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
#'   Cells that occur along the matrix diagonal represent the amount of
#'   exposed boundary associated with each planning unit that has
#'   no neighbor (e.g., these value might pertain to boundaries along a
#'   coastline).}
#'
#' \item{`data` as a `data.frame` object}{with the columns `"id1"`,
#'   `"id2"`, and `"boundary"`. The `"id1"` and `"id2"` columns contain
#'   identifiers (indices) for a pair of planning units, and the `"boundary"`
#'   column contains the amount of shared boundary length between these
#'   two planning units.
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
#' \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z} (\mathit{ifelse}(i == j, E_z, 1)
#' \times p \times W_{zz} B_{ij}) + \sum_{i}^{I} \sum_{j}^{I} \sum_{z}^{Z}
#' \sum_{y}^{Z} (-2 \times p \times X_{iz} \times X_{jy} \times W_{zy} \times
#' B_{ij})}{
#' sum_i^I sum_j^I sum_z^Z (ifelse(i == j, Ez, 1) * p * Wzz * Bij) + sum_i^I
#' sum_j^I sum_z^Z sum_y^Z (-2 * p * Xiz * Xjy * Wzy * Bij)
#' }
#'
#' Note that when the problem objective is to maximize some measure of
#' benefit and not minimize some measure of cost, the term \eqn{p} is
#' replaced with \eqn{-p}.
#'
#' @return Object (i.e., [`ConservationProblem-class`]) with the penalties
#'  added to it.
#'
#' @seealso
#' See [penalties] for an overview of all functions for adding penalties.
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
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#'
#' # create minimal problem
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(verbose = FALSE)
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
#' s <- stack(solve(p1), solve(p2), solve(p3), solve(p4))
#'
#' # plot solutions
#' plot(s, main = c("basic solution", "small penalties", "high penalties",
#'                  "precomputed data"), axes = FALSE, box = FALSE)
#'
#' # create minimal problem with multiple zones and limit the run-time for
#' # solver to 10 seconds so this example doesn't take too long
#' p5 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
#'       add_binary_decisions() %>%
#'       add_default_solver(time_limit = 10, verbose = FALSE)
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
#' s2 <- stack(category_layer(solve(p5)), category_layer(solve(p6)),
#'             category_layer(solve(p7)), category_layer(solve(p8)),
#'             category_layer(solve(p9)), category_layer(solve(p10)))
#'
#' # plot solutions
#' plot(s2, main = c("basic solution", "within zone clumping (low)",
#'                   "within zone clumping (high)", "between zone clumping",
#'                   "within + between clumping", "negative clumping"),
#'      axes = FALSE, box = FALSE)
#' }
#' @export
add_boundary_penalties <- function(
  x, penalty, edge_factor = rep(0.5, number_of_zones(x)),
  zones = diag(number_of_zones(x)), data = NULL) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    assertthat::is.scalar(penalty), is.finite(penalty),
    is.numeric(edge_factor), all(is.finite(edge_factor)),
    all(edge_factor >= 0), all(edge_factor <= 1),
    length(edge_factor) == number_of_zones(x),
    inherits(zones, c("matrix", "Matrix")), all(is.finite(zones)))
  # convert zones to matrix
  zones <- as.matrix(zones)
  assertthat::assert_that(
    isSymmetric(zones), ncol(zones) == number_of_zones(x),
    min(zones) >= -1, max(zones) <= 1)
  colnames(zones) <- x$zone_names()
  rownames(zones) <- colnames(zones)
  # prepare boundary matrix data
  bm <- internal_prepare_planning_unit_boundary_data(x, data)
  # create new constraint object
  x$add_penalty(pproto(
    "BoundaryPenalty",
    Penalty,
    name = "Boundary penalties",
    data = list(boundary_matrix = bm),
    parameters = parameters(
      numeric_parameter("penalty", penalty),
      proportion_parameter_array("edge factor", edge_factor, x$zone_names()),
      numeric_matrix_parameter("zones", zones, lower_limit = -1,
                               upper_limit = 1, symmetric = TRUE)),
    calculate = function(self, x) {
        assertthat::assert_that(inherits(x, "ConservationProblem"))
        m <- self$get_data("boundary_matrix")
        if (is.Waiver(m)) {
          m <- internal_prepare_planning_unit_boundary_data(
            x, boundary_matrix(x$get_data("cost")))
        }
        class(m) <- "dgCMatrix"
        self$set_data("boundary_matrix", m)
        invisible()
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
                              inherits(y, "ConservationProblem"))
      p <- self$parameters$get("penalty")
      if (abs(p) > 1e-50) {
        # apply penalties
        rcpp_apply_boundary_penalties(x$ptr, p,
          self$parameters$get("edge factor")[[1]],
          self$parameters$get("zones"),
          self$get_data("boundary_matrix"))
      }
      invisible(TRUE)
  }))
}

internal_prepare_planning_unit_boundary_data <- function(x, data)  {
  if (!is.null(data)) {
    # if boundary data is in data.frame format then coerce to sparse matrix
    if (inherits(data, "data.frame")) {
      assertthat::assert_that(
        !assertthat::has_name(data, "zone1"),
        !assertthat::has_name(data, "zone2"))
      data <- marxan_boundary_data_to_matrix(x, data)
    }
    # if/when data is matrix run further checks to ensure that
    # it is compatible with planning unit data
    if (inherits(data, c("matrix", "Matrix"))) {
      # if it is matrix coerce to sparse matrix
      bm <- data
      if (!inherits(data, c("dsCMatrix", "dgCMatrix"))) {
        bm <- as_Matrix(data, "dgCMatrix")
      }
      # check that matrix properties are correct
      assertthat::assert_that(
        ncol(bm) == nrow(bm),
        all(is.finite(bm@x)),
        msg = paste("argument to data is not a valid symmetric matrix",
                    "with finite (non-NA) values"))
      assertthat::assert_that(
        isTRUE(x$number_of_total_units() == ncol(bm)),
        msg = paste("argument to data has a different number of rows/columns",
                    "than the number of planning units in x"))
      # return result
      return(filter_planning_units_in_boundary_matrix(
        x$planning_unit_indices(), bm))
    } else {
     # throw error because object class not recognized
      stop("argument to data is of a class that is not supported")
    }
  } else {
    assertthat::assert_that(inherits(x$data$cost, c("Spatial", "Raster", "sf")),
        msg = paste("argument to data must be supplied because planning unit",
                    "data are not in a spatially referenced format"))
    return(new_waiver())
  }
}

filter_planning_units_in_boundary_matrix <- function(idx, bm) {
  # add boundaries for non-planning units to diagonal of planning units
  extra_boundaries <- Matrix::colSums(bm[-1 * idx, idx, drop = FALSE])
  bm <- bm[idx, idx]
  Matrix::diag(bm) <- Matrix::diag(bm) + extra_boundaries
  # force symmetry
  bm <- Matrix::forceSymmetric(bm, uplo = "L")
  bm
}
