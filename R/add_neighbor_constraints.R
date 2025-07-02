#' @include internal.R Constraint-class.R marxan_connectivity_data_to_matrix.R
NULL

#' Add neighbor constraints
#'
#' Add constraints to a conservation planning problem to ensure
#' that all selected planning units in the solution have at least a certain
#' number of neighbors that are also selected in the solution.
#'
#' @param x [problem()] object.
#'
#' @param k `integer` minimum number of neighbors for selected
#'   planning units in the solution. For problems with multiple zones,
#'   the argument to `k` must have an element for each zone.
#'
#' @param clamp `logical` should the minimum number of neighbors
#'   for selected planning units in the solution be clamped to feasibility?
#'   For example, if a planning unit has two neighbors,
#'   `k = 3`, and `clamp = FALSE`, then the planning unit could not
#'   ever be selected in the solution. However, if `clamp = TRUE`,
#'   then the planning unit could potentially be selected in the solution if
#'   both of its two neighbors were also selected.
#'   Defaults to `TRUE`.
#'
#' @param zones `matrix` or `Matrix` object describing the
#'   neighborhood scheme for different zones. Each row and column corresponds
#'   to a different zone in the argument to `x`, and cell values must
#'   contain binary `numeric` values (i.e., one or zero) that indicate
#'   if neighboring planning units (as specified in the argument to
#'   `data`) should be considered neighbors if they are allocated to
#'   different zones. The cell values along the diagonal
#'   of the matrix indicate if planning units that are allocated to the same
#'   zone should be considered neighbors or not. The default argument to
#'   `zones` is an identity matrix (i.e., a matrix with ones along the
#'   matrix diagonal and zeros elsewhere), so that planning units are
#'   only considered neighbors if they are both allocated to the same zone.
#'
#' @param data `NULL`, `matrix`, `Matrix`, `data.frame`, or
#'   `array` object showing which planning units are neighbors with each
#'   other. The argument defaults to `NULL` which means that the
#'   neighborhood data is calculated automatically using the
#'   [adjacency_matrix()] function.
#'   See the Data format section for more information.
#'
#' @details This function uses neighborhood data to identify solutions that
#'   surround planning units with a minimum number of neighbors. It
#'   was inspired by the mathematical formulations detailed in
#'   Billionnet (2013) and Beyer *et al.* (2016).
#'
#' @section Data format:
#' The argument to `data` can be specified using the following formats:
#'
#' \describe{
#'
#' \item{`data` as a `NULL` value}{neighborhood data should be calculated
#'   automatically
#'   using the [adjacency_matrix()] function. This is the default
#'   argument. Note that the neighborhood data must be manually defined
#'   using one of the other formats below when the planning unit data
#'   in the argument to `x` is not spatially referenced (e.g.,
#'   in `data.frame` or `numeric` format).}
#'
#' \item{`data` as a `matrix`/`Matrix` object}{where rows and columns represent
#'   different planning units and the value of each cell indicates if the
#'   two planning units are neighbors or not. Cell values should be binary
#'   `numeric` values (i.e., one or zero). Cells that occur along the
#'   matrix diagonal have no effect on the solution at all because each
#'   planning unit cannot be a neighbor with itself.}
#'
#' \item{`data` as a `data.frame` object}{containing columns that are named
#'   `"id1"`, `"id2"`, and `"boundary"`. Here, each row
#'   denotes the connectivity between two planning units following the
#'   *Marxan* format. The `"boundary"` column should contain
#'   binary `numeric` values that indicate if the two planning units
#'   specified in the `"id1"` and `"id2"` columns are neighbors
#'   or not. This data can be used to describe symmetric or
#'   asymmetric relationships between planning units. By default,
#'   input data is assumed to be symmetric unless asymmetric data is
#'   also included (e.g., if data is present for planning units 2 and 3, then
#'   the same amount of connectivity is expected for planning units 3 and 2,
#'   unless connectivity data is also provided for planning units 3 and 2).
#'   If the argument to `x` contains multiple zones, then the
#'   `"zone1"` and `"zone2"` columns can optionally be provided to manually
#'   specify if the neighborhood data pertain to specific zones. The
#'   `"zone1"` and `"zone2"` columns should contain the `character`
#'   names of the zones. If the columns `"zone1"` and `"zone2"`
#'   are present, then the argument to `zones` must be `NULL`.}
#'
#' \item{`data` as an `array` object}{containing four-dimensions where binary
#'   `numeric` values indicate if planning unit should be treated
#'   as being neighbors with every other planning unit when they
#'   are allocated to every combination of management zone. The first two
#'   dimensions (i.e., rows and columns) correspond to the planning units,
#'   and second two dimensions correspond to the management zones. For
#'   example, if the argument to `data` had a value of 1 at the index
#'   `data[1, 2, 3, 4]` this would indicate that planning unit 1 and
#'   planning unit 2 should be treated as neighbors when they are
#'   allocated to zones 3 and 4 respectively.}
#'
#' }
#'
#' @inherit add_contiguity_constraints return seealso
#'
#' @references
#' Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
#' conservation planning problems with integer linear programming.
#' *Ecological Modelling*, 228: 14--22.
#'
#' Billionnet A (2013) Mathematical optimization ideas for biodiversity
#' conservation. *European Journal of Operational Research*, 231:
#' 514--534.
#'
#' @family constraints
#'
#' @examples
#' \dontrun{
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
#'   add_relative_targets(0.1) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with constraints that require 1 neighbor
#' # and neighbors are defined using a rook-style neighborhood
#' p2 <- p1 %>% add_neighbor_constraints(1)
#'
#' # create problem with constraints that require 2 neighbor
#' # and neighbors are defined using a rook-style neighborhood
#' p3 <- p1 %>% add_neighbor_constraints(2)
#'
#' # create problem with constraints that require 3 neighbor
#' # and neighbors are defined using a queen-style neighborhood
#' p4 <-
#'   p1 %>%
#'   add_neighbor_constraints(
#'     3, data = adjacency_matrix(sim_pu_raster, directions = 8)
#'   )
#'
#' # solve problems
#' s1 <- terra::rast(list(solve(p1), solve(p2), solve(p3), solve(p4)))
#' names(s1) <- c("basic solution", "1 neighbor", "2 neighbors", "3 neighbors")
#'
#' # plot solutions
#' plot(s1, axes = FALSE)
#'
#' # create minimal problem with multiple zones
#' p5 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem where selected planning units require at least 2 neighbors
#' # for each zone and planning units are only considered neighbors if they
#' # are allocated to the same zone
#' z6 <- diag(3)
#' print(z6)
#' p6 <- p5 %>% add_neighbor_constraints(rep(2, 3), zones = z6)
#'
#' # create problem where the planning units in zone 1 don't explicitly require
#' # any neighbors, planning units in zone 2 require at least 1 neighbors, and
#' # planning units in zone 3 require at least 2 neighbors. As before, planning
#' # units are still only considered neighbors if they are allocated to the
#' # same zone
#' p7 <- p5 %>% add_neighbor_constraints(c(0, 1, 2), zones = z6)
#'
#' # create problem given the same constraints as outlined above, except
#' # that when determining which selected planning units are neighbors,
#' # planning units that are allocated to zone 1 and zone 2 can also treated
#' # as being neighbors with each other
#' z8 <- diag(3)
#' z8[1, 2] <- 1
#' z8[2, 1] <- 1
#' print(z8)
#' p8 <- p5 %>% add_neighbor_constraints(c(0, 1, 2), zones = z8)
#'
#' # solve problems
#' s2 <- list(p5, p6, p7, p8)
#' s2 <- lapply(s2, solve)
#' s2 <- lapply(s2, category_layer)
#' s2 <- terra::rast(s2)
#' names(s2) <- c("basic problem", "p6", "p7", "p8")
#'
#' # plot solutions
#' plot(s2, main = names(s2), axes = FALSE)
#' }
#' @name add_neighbor_constraints
#'
#' @exportMethod add_neighbor_constraints
#'
#' @aliases add_neighbor_constraints,ConservationProblem,ANY,ANY,ANY,array-method add_neighbor_constraints,ConservationProblem,ANY,ANY,ANY,matrix-method add_neighbor_constraints,ConservationProblem,ANY,ANY,ANY,data.frame-method add_neighbor_constraints,ConservationProblem,ANY,ANY,ANY,ANY-method
NULL

#' @export
methods::setGeneric("add_neighbor_constraints",
  signature = methods::signature("x", "k", "clamp", "zones", "data"),
  function(x, k, clamp = TRUE, zones = diag(number_of_zones(x)), data = NULL) {
    assert_required(x)
    assert_required(k)
    assert_required(clamp)
    assert_required(zones)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      is_inherits(data, c("NULL", "matrix", "Matrix", "data.frame", "array"))
    )
    standardGeneric("add_neighbor_constraints")
  }
)

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,ANY,ANY,ANY,ANY}(x, k, clamp, zones, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY", "ANY"),
  function(x, k, clamp, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is.numeric(k),
      all_finite(k),
      is_integer(k),
      all_positive(k),
      all(k >= 0),
      length(k) == number_of_zones(x),
      assertthat::is.flag(clamp),
      all_finite(clamp),
      is_inherits(zones, c("matrix", "Matrix")),
      is_inherits(data, c("NULL", "Matrix"))
    )
    if (!is.null(data)) {
      # check argument to data if not NULL
      assert(
        ncol(data) == nrow(data),
        number_of_total_units(x) == ncol(data),
        all_binary(data)
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
    # add constraints
    internal_add_neighbor_constraints(x, k, clamp, zones, data)
  }
)

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,ANY,ANY,ANY,data.frame}(x, k, clamp, zones, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY", "data.frame"),
  function(x, k, clamp, zones, data) {
    # assert valid arguments
    assert(
      is.data.frame(data),
      assertthat::has_name(data, "id1"),
      assertthat::has_name(data, "id2"),
      assertthat::has_name(data, "boundary")
    )
    if (
      any(c("zone1", "zone2") %in% names(data))
    ) {
      assert(
        assertthat::has_name(data, "zone1"),
        assertthat::has_name(data, "zone2"),
        msg = paste(
          "{.arg data} must have {.col zone1} and {.col zone2} columns ",
          "when specifying constraints for multiple zones."
        )
      )
    }
    # add constraints
    add_neighbor_constraints(
      x, k, clamp, zones, marxan_connectivity_data_to_matrix(x, data, TRUE)
    )
  }
)

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,ANY,ANY,ANY,matrix}(x, k, clamp, zones, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY", "matrix"),
  function(x, k, clamp, zones, data) {
    # add constraints
    add_neighbor_constraints(x, k, clamp, zones, as_Matrix(data, "dgCMatrix"))
  }
)

#' @name add_neighbor_constraints
#' @usage \S4method{add_neighbor_constraints}{ConservationProblem,ANY,ANY,ANY,array}(x, k, clamp, zones, data)
#' @rdname add_neighbor_constraints
methods::setMethod("add_neighbor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY", "array"),
  function(x, k, clamp, zones, data) {
    # assert arguments are valid
    assert(
      is_conservation_problem(x),
      inherits(data, "array"),
      is.null(zones),
      is.numeric(k),
      is_integer(k),
      all_finite(k),
      all_positive(k),
      all(k >= 0),
      length(k) == number_of_zones(x),
      length(k) == number_of_zones(x),
      assertthat::is.flag(clamp),
      length(dim(data)) == 4,
      dim(data)[1] == x$number_of_total_units(),
      dim(data)[2] == x$number_of_total_units(),
      dim(data)[3] == x$number_of_zones(),
      dim(data)[4] == x$number_of_zones(),
      all_finite(data),
      all_binary(data)
    )
    # add constraints
    internal_add_neighbor_constraints(x, k, clamp, zones, data)
  }
)

internal_add_neighbor_constraints <- function(x, k, clamp, zones, data) {
  # assert arguments valid
  assert(
    is_conservation_problem(x),
    is.numeric(k),
    assertthat::is.flag(clamp),
    is_inherits(data, c("NULL", "Matrix", "array")),
    .internal = TRUE
  )
  # convert zones to matrix
  if (!is.null(zones)) {
    zones <- as.matrix(zones)
    assert(
      is.numeric(zones),
      all_binary(zones),
      all_finite(zones),
      isSymmetric(zones),
      ncol(zones) == number_of_zones(x),
      call = fn_caller_env()
    )
    colnames(zones) <- x$zone_names()
    rownames(zones) <- colnames(zones)
  }
  # add the constraint
  x$add_constraint(
    R6::R6Class(
      "NeighborConstraint",
      inherit = Constraint,
      public = list(
        name = "neighbor constraints",
        data = list(k = k, clamp = clamp, zones = zones, data = data),
        calculate = function(x) {
          assert(is_conservation_problem(x))
          # if needed, generate adjacency matrix if null
          if (
            is.null(self$get_data("data")) &&
            is.Waiver(x$get_data("adjacency"))
          ) {
            x$set_data("adjacency", adjacency_matrix(x$data$cost))
          }
          # return success
          invisible(TRUE)
        },
        apply = function(x, y) {
          # assert valid arguments
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # extract data
          d <- self$get_data("data")
          if (is.null(d)) {
            d <- y$get_data("adjacency")
          }
          # prepare data
          m <- list()
          ind <- y$planning_unit_indices()
          nz <- y$number_of_zones()
          if (inherits(d, "Matrix"))  {
            # if data is a matrix...
            d <- d[ind, ind]
            z <- self$get_data("zones")
            for (z1 in seq_len(ncol(z))) {
              m[[z1]] <- list()
              for (z2 in seq_len(nrow(z))) {
                m[[z1]][[z2]] <- as_Matrix(d * z[z1, z2], "dgCMatrix")
              }
            }
          } else if (inherits(d, "array")) {
            ## if data is an array...
            for (z1 in seq_len(dim(d)[3])) {
              m[[z1]] <- list()
              for (z2 in seq_len(dim(d)[4])) {
                m[[z1]][[z2]] <- as_Matrix(d[ind, ind, z1, z2], "dgCMatrix")
              }
            }
          } else {
            ## throw error if not recognized
            # nocov start
            cli::cli_abort(
              "Failed calculations for {.fn add_neighbor_constraints}.",
              .internal = TRUE,
            )
            # nocov end
          }
          # compute maximum number of neighbors for each planing unit
          max_n <- 0
          for (z1 in seq_along(m)) {
            if (length(m[[z1]]) > 1) {
              curr_m <- Reduce(`+`, m[[z1]])
            } else {
              curr_m <- m[[z1]][[1]]
            }
            max_n <- max(max_n, Matrix::colSums(curr_m > 1e-5))
          }
          # apply constraints
          if (max(k) > 0) {
            rcpp_apply_neighbor_constraints(x$ptr, m, clamp, k, max_n + 1)
          }
          # return success
          invisible(TRUE)
        }
      )
    )$new()
  )
}
