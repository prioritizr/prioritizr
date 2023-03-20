#' @include internal.R Constraint-class.R
NULL

#' Add contiguity constraints
#'
#' Add constraints to a conservation planning problem to ensure
#' that all selected planning units are spatially connected with each other
#' and form a single contiguous unit.
#'
#' @param x [problem()] object.
#'
#' @param zones `matrix` or `Matrix` object describing the
#'   connection scheme for different zones. Each row and column corresponds
#'   to a different zone in the argument to `x`, and cell values must
#'   contain binary `numeric` values (i.e., one or zero) that indicate
#'   if connected planning units (as specified in the argument to
#'   `data`) should be still considered connected if they are allocated to
#'   different zones. The cell values along the diagonal
#'   of the matrix indicate if planning units should be subject to
#'   contiguity constraints when they are allocated to a given zone. Note
#'   arguments to `zones` must be symmetric, and that a row or column has
#'   a value of one then the diagonal element for that row or column must also
#'   have a value of one. The default argument to `zones` is an identity
#'   matrix (i.e., a matrix with ones along the matrix diagonal and zeros
#'   elsewhere), so that planning units are only considered connected if they
#'   are both allocated to the same zone.
#'
#' @param data `NULL`, `matrix`, `Matrix`, `data.frame`
#'   object showing which planning units are connected with each
#'   other. The argument defaults to `NULL` which means that the
#'   connection data is calculated automatically using the
#'   [adjacency_matrix()] function.
#'   See the Data format section for more information.
#'
#' @details This function uses connection data to identify solutions that
#'   form a single contiguous unit. It was inspired by the
#'   mathematical formulations detailed in Önal and Briers (2006).
#'
#' @section Data format:
#'
#' The argument to `data` can be specified using the following formats.
#'
#' \describe{
#'
#' \item{`data` as a `NULL` value}{indicating that connection data should be
#'   calculated automatically using the [adjacency_matrix()] function.
#'   This is the default argument.
#'   Note that the connection data must be manually defined
#'   using one of the other formats below when the planning unit data
#'   in the argument to `x` is not spatially referenced (e.g.,
#'   in `data.frame` or `numeric` format).}
#'
#' \item{`data` as a `matrix`/`Matrix` object}{where rows and columns represent
#'   different planning units and the value of each cell indicates if the
#'   two planning units are connected or not. Cell values should be binary
#'   `numeric` values (i.e., one or zero). Cells that occur along the
#'   matrix diagonal have no effect on the solution at all because each
#'   planning unit cannot be a connected with itself.}
#'
#' \item{`data` as a `data.frame` object}{containing columns that are named
#'   `"id1"`, `"id2"`, and `"boundary"`. Here, each row
#'   denotes the connectivity between two planning units following the
#'   *Marxan* format. The `"boundary"` column should contain
#'   binary `numeric` values that indicate if the two planning units
#'   specified in the `"id1"` and `"id2"` columns are connected
#'   or not. This data can be used to describe symmetric or
#'   asymmetric relationships between planning units. By default,
#'   input data is assumed to be symmetric unless asymmetric data is
#'   also included (e.g., if data is present for planning units 2 and 3, then
#'   the same amount of connectivity is expected for planning units 3 and 2,
#'   unless connectivity data is also provided for planning units 3 and 2).}
#'
#'  }
#'
#' @section Notes:
#' In early versions, this function was named as the
#' `add_connected_constraints()` function.
#'
#' @return An updated [problem()] object with the constraints added to it.
#'
#' @seealso
#' See [constraints] for an overview of all functions for adding constraints.
#'
#' @family constraints
#'
#' @encoding UTF-8
#'
#' @references
#' Önal H and Briers RA (2006) Optimal selection of a connected
#' reserve network. *Operations Research*, 54: 379--388.
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
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with added connected constraints
#' p2 <- p1 %>% add_contiguity_constraints()
#'
#' # solve problems
#' s1 <- c(solve(p1), solve(p2))
#' names(s1) <- c("basic solution", "connected solution")
#'
#' # plot solutions
#' plot(s1, axes = FALSE)
#'
#' # create minimal problem with multiple zones, and limit the solver to
#' # 30 seconds to obtain solutions in a feasible period of time
#' p3 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(time_limit = 30, verbose = FALSE)
#'
#' # create problem with added constraints to ensure that the planning units
#' # allocated to each zone form a separate contiguous unit
#' z4 <- diag(3)
#' print(z4)
#' p4 <- p3 %>% add_contiguity_constraints(z4)
#'
#' # create problem with added constraints to ensure that the planning
#' # units allocated to each zone form a separate contiguous unit,
#' # except for planning units allocated to zone 3 which do not need
#' # form a single contiguous unit
#' z5 <- diag(3)
#' z5[3, 3] <- 0
#' print(z5)
#' p5 <- p3 %>% add_contiguity_constraints(z5)
#'
#' # create problem with added constraints that ensure that the planning
#' # units allocated to zones 1 and 2 form a contiguous unit
#' z6 <- diag(3)
#' z6[1, 2] <- 1
#' z6[2, 1] <- 1
#' print(z6)
#' p6 <- p3 %>% add_contiguity_constraints(z6)
#'
#' # solve problems
#' s2 <- lapply(list(p3, p4, p5, p6), solve)
#' s2 <- lapply(s2, category_layer)
#' s2 <- terra::rast(s2)
#' names(s2) <- c("basic solution", "p4", "p5", "p6")
#'
#' # plot solutions
#' plot(s2, axes = FALSE)
#'
#' # create a problem that has a main "reserve zone" and a secondary
#' # "corridor zone" to connect up import areas. Here, each feature has a
#' # target of 50% of its distribution. If a planning unit is allocated to the
#' # "reserve zone", then the prioritization accrues 100% of the amount of
#' # each feature in the planning unit. If a planning unit is allocated to the
#' # "corridor zone" then the prioritization accrues 40% of the amount of each
#' # feature in the planning unit. Also, the cost of managing a planning unit
#' # in the "corridor zone" is 30% of that when it is managed as the
#' # "reserve zone". Finally, the problem has constraints which
#' # ensure that all of the selected planning units form a single contiguous
#' # unit, so that the planning units allocated to the "corridor zone" can
#' # link up the planning units allocated to the "reserve zone"
#'
#' # create planning unit data
#' pus <- sim_zones_pu_raster[[c(1, 1)]]
#' pus[[2]] <- pus[[2]] * 0.3
#' print(pus)
#'
#' # create biodiversity data
#' fts <- zones(
#'   sim_features, sim_features * 0.4,
#'   feature_names = names(sim_features),
#'   zone_names = c("reserve zone", "corridor zone")
#' )
#' print(fts)
#'
#' # create targets
#' targets <- tibble::tibble(
#'   feature = names(sim_features),
#'   zone = list(zone_names(fts))[rep(1, 5)],
#'   target = terra::global(sim_features, "sum", na.rm = TRUE)[[1]] * 0.5,
#'   type = rep("absolute", 5)
#' )
#' print(targets)
#'
#' # create zones matrix
#' z7 <- matrix(1, ncol = 2, nrow = 2)
#' print(z7)
#'
#' # create problem
#' p7 <-
#'   problem(pus, fts) %>%
#'   add_min_set_objective() %>%
#'   add_manual_targets(targets) %>%
#'   add_contiguity_constraints(z7) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problems
#' s7 <- category_layer(solve(p7))
#'
#' # plot solutions
#' plot(s7, main = "solution", axes = FALSE)
#' }
#' @name add_contiguity_constraints
#'
#' @exportMethod add_contiguity_constraints
#'
#' @aliases add_contiguity_constraints,ConservationProblem,ANY,matrix-method add_contiguity_constraints,ConservationProblem,ANY,data.frame-method add_contiguity_constraints,ConservationProblem,ANY,ANY-method
NULL

methods::setGeneric("add_contiguity_constraints",
  signature = methods::signature("x", "zones", "data"),
  function(x, zones = diag(number_of_zones(x)), data = NULL) {
    rlang::check_required(x)
    rlang::check_required(zones)
    rlang::check_required(data)
    assert(
      is_conservation_problem(x),
      is_inherits(
        data,
        c("NULL",  "dgCMatrix", "data.frame", "matrix", "Matrix")
      )
    )
    standardGeneric("add_contiguity_constraints")
  }
)

#' @name add_contiguity_constraints
#' @usage \S4method{add_contiguity_constraints}{ConservationProblem,ANY,ANY}(x, zones, data)
#' @rdname add_contiguity_constraints
methods::setMethod("add_contiguity_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY"),
  function(x, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is_matrix_ish(zones)
    )
    if (!is.null(data)) {
      # check argument to data if not NULL
      assert(is_matrix_ish(data))
      data <- as_Matrix(data, "dgCMatrix")
      assert(
        is_numeric_values(data),
        all_finite(data),
        all_binary(data),
        ncol(data) == nrow(data),
        number_of_total_units(x) == ncol(data),
        Matrix::isSymmetric(data)
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
      is.matrix(zones),
      isSymmetric(zones),
      ncol(zones) == number_of_zones(x),
      is_numeric_values(zones),
      all_finite(zones),
      all_binary(zones),
      all(colMeans(zones) <= diag(zones)),
      all(rowMeans(zones) <= diag(zones))
    )
    colnames(zones) <- x$zone_names()
    rownames(zones) <- colnames(zones)
    # add constraints
    x$add_constraint(
      R6::R6Class(
        "ContiguityConstraint",
        inherit = Constraint,
        public = list(
          name = "contiguity constraints",
          data = list(data = data, zones = zones),
          calculate = function(x) {
            # assert valid arguments
            assert(is_conservation_problem(x), .internal = TRUE)
            # if needed, calculate adjacency matrix
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
            assert(
              inherits(x, "OptimizationProblem"),
              inherits(y, "ConservationProblem"),
              .internal = TRUE
            )
            # extract data
            z <- self$get_data("zones")
            d <- self$get_data("data")
            if (is.Waiver(d) || is.null(d)) {
              d <- y$get_data("adjacency")
            }
            # convert to dgCMatrix
            d <- as_Matrix(d, "dgCMatrix")
            # subset data by planning unit indices
            ind <- y$planning_unit_indices()
            d <- d[ind, ind, drop = FALSE]
            # extract clusters from z
            z_cl <- igraph::graph_from_adjacency_matrix(
              z, diag = FALSE, mode = "undirected", weighted = NULL
            )
            z_cl <- igraph::clusters(z_cl)$membership
            # set cluster memberships to zero if constraints not needed
            z_cl <- z_cl * diag(z)
            # convert d to lower triangle sparse matrix
            d <- Matrix::forceSymmetric(d, uplo = "L")
            d <- as_Matrix(Matrix::tril(d), "dgCMatrix")
            # apply constraints if any zones have contiguity constraints
            if (max(z_cl) > 0) rcpp_apply_contiguity_constraints(x$ptr, d, z_cl)
            # return invisible
            invisible(TRUE)
          }
        )
      )$new()
    )
  }
)

#' @name add_contiguity_constraints
#' @usage \S4method{add_contiguity_constraints}{ConservationProblem,ANY,data.frame}(x, zones, data)
#' @rdname add_contiguity_constraints
methods::setMethod("add_contiguity_constraints",
  methods::signature("ConservationProblem", "ANY", "data.frame"),
  function(x, zones, data) {
    # assert that does not have zone1 and zone2 columns
    assert(
      is.data.frame(data),
      assertthat::has_name(data, "id1"),
      assertthat::has_name(data, "id2"),
      assertthat::has_name(data, "boundary"),
      !assertthat::has_name(data, "zone1"),
      !assertthat::has_name(data, "zone2")
    )
    # add constraints
    add_contiguity_constraints(
      x, zones, marxan_connectivity_data_to_matrix(x, data, TRUE)
    )
  }
)

#' @name add_contiguity_constraints
#' @usage \S4method{add_contiguity_constraints}{ConservationProblem,ANY,matrix}(x, zones, data)
#' @rdname add_contiguity_constraints
methods::setMethod("add_contiguity_constraints",
  methods::signature("ConservationProblem", "ANY", "matrix"),
  function(x, zones, data) {
    # add constraints
    add_contiguity_constraints(x, zones, as_Matrix(data, "dgCMatrix"))
  }
)
