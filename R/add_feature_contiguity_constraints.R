#' @include internal.R Constraint-proto.R
NULL

#' Add feature contiguity constraints
#'
#' Add constraints to a [problem()] to ensure that each feature is
#' represented in a contiguous unit of dispersible habitat. These constraints
#' are a more advanced version of those implemented in the
#' [add_contiguity_constraints()] function, because they ensure that
#' each feature is represented in a contiguous unit and not that the entire
#' solution should form a contiguous unit. Additionally, this function
#' can use data showing the distribution of dispersible habitat for each
#' feature to ensure that all features can disperse through out the areas
#' designated for their conservation.
#'
#' @param x [problem()] object.
#'
#' @param zones `matrix`, `Matrix` or `list` object describing
#'   the connection scheme for different zones. For `matrix` or
#'   and `Matrix` arguments, each row and column corresponds
#'   to a different zone in the argument to `x`, and cell values must
#'   contain binary `numeric` values (i.e., one or zero) that indicate
#'   if connected planning units (as specified in the argument to
#'   `data`) should be still considered connected if they are allocated to
#'   different zones. The cell values along the diagonal
#'   of the matrix indicate if planning units should be subject to
#'   contiguity constraints when they are allocated to a given zone. Note
#'   arguments to `zones` must be symmetric, and that a row or column has
#'   a value of one then the diagonal element for that row or column must also
#'   have a value of one. If the connection scheme between different zones
#'   should differ among the features, then the argument to `zones` should
#'   be a `list` of `matrix` or `Matrix` objects that shows the
#'   specific scheme for each feature using the conventions described above.
#'   The default argument to `zones` is an identity
#'   matrix (i.e., a matrix with ones along the matrix diagonal and zeros
#'   elsewhere), so that planning units are only considered connected if they
#'   are both allocated to the same zone.
#'
#' @param data `NULL`, `matrix`, `Matrix`, `data.frame`
#'   or `list` of `matrix`, `Matrix`, or `data.frame`
#'   objects. The argument to data shows which planning units should be treated
#'   as being connected when implementing constraints to ensure that features
#'   are represented in contiguous units. If different features have
#'   different dispersal capabilities, then it may be desirable to specify
#'   which sets of planning units should be treated as being connected
#'   for which features using a `list` of objects. The default argument
#'   is `NULL` which means that the connection data is calculated
#'   automatically using the [adjacency_matrix()] function and so
#'   all adjacent planning units are treated as being connected for all
#'   features. See the Data format section for more information.
#'
#' @details This function uses connection data to identify solutions that
#'   represent features in contiguous units of dispersible habitat.
#'   It was inspired by the mathematical formulations detailed in
#'   Önal and Briers (2006) and Cardeira *et al.* 2010. For an
#'   example that has used these constraints, see Hanson *et al.* (2019).
#'   Please note
#'   that these constraints require the expanded formulation and therefore
#'   cannot be used with feature data that have negative vales.
#'   **Please note that adding these constraints to a problem will
#'   drastically increase the amount of time required to solve it.**
#'
#' @section Data format:
#'
#' The argument to `data` can be specified using the following formats.
#'
#' \describe{
#'
#' \item{`data` as a `NULL` value}{connection
#'   data should be calculated automatically
#'   using the [adjacency_matrix()] function. This is the default
#'   argument and means that all adjacent planning units are treated
#'   as potentially dispersible for all features.
#'   Note that the connection data must be manually defined
#'   using one of the other formats below when the planning unit data
#'   in the argument to `x` is not spatially referenced (e.g.,
#'   in `data.frame` or `numeric` format).}
#'
#' \item{`data` as a`matrix`/`Matrix` object}{where rows and columns represent
#'   different planning units and the value of each cell indicates if the
#'   two planning units are connected or not. Cell values should be binary
#'   `numeric` values (i.e., one or zero). Cells that occur along the
#'   matrix diagonal have no effect on the solution at all because each
#'   planning unit cannot be a connected with itself. Note that pairs
#'   of connected planning units are treated as being potentially dispersible
#'   for all features.}
#'
#' \item{`data` as a `data.frame` object}{containing the fields (columns)
#'   `"id1"`, `"id2"`, and `"boundary"`. Here, each row
#'   denotes the connectivity between two planning units following the
#'   *Marxan* format. The field `boundary` should contain
#'   binary `numeric` values that indicate if the two planning units
#'   specified in the fields `"id1"` and `"id2"` are connected
#'   or not. This data can be used to describe symmetric or
#'   asymmetric relationships between planning units. By default,
#'   input data is assumed to be symmetric unless asymmetric data is
#'   also included (e.g., if data is present for planning units 2 and 3, then
#'   the same amount of connectivity is expected for planning units 3 and 2,
#'   unless connectivity data is also provided for planning units 3 and 2).
#'   Note that pairs of connected planning units are treated as being
#'   potentially dispersible for all features.}
#'
#' \item{`data` as a `list` object}{containing `matrix`, `Matrix`, or
#'   `data.frame` objects showing which planning units
#'   should be treated as connected for each feature. Each element in the
#'   `list` should correspond to a different feature (specifically,
#'    a different target in the problem), and should contain a `matrix`,
#'   `Matrix`, or `data.frame` object that follows the conventions
#'    detailed above.}
#'
#' }
#'
#' @inherit add_contiguity_constraints return
#"
#' @seealso
#' See [constraints] for an overview of all functions for adding constraints.
#'
#' @family constraints
#'
#' @section Notes:
#' In early versions, it was named as the `add_corridor_constraints` function.
#'
#' @encoding UTF-8
#'
#' @references
#' Önal H and Briers RA (2006) Optimal selection of a connected
#' reserve network. *Operations Research*, 54: 379--388.
#'
#' Cardeira JO, Pinto LS, Cabeza M and Gaston KJ (2010) Species specific
#' connectivity in reserve-network design using graphs.
#' *Biological Conservation*, 2: 408--415.
#'
#' Hanson JO, Fuller RA, & Rhodes JR (2019) Conventional methods for enhancing
#' connectivity in conservation planning do not always maintain gene flow.
#' *Journal of Applied Ecology*, 56: 913--922.
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_pu_zones_raster <- get_sim_zones_pu_raster()
#' sim_features_zones <- get_sim_zones_features()
#'
#' # create minimal problem
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.3) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with contiguity constraints
#' p2 <- p1 %>% add_contiguity_constraints()
#'
#' # create problem with constraints to represent features in contiguous
#' # units
#' p3 <- p1 %>% add_feature_contiguity_constraints()
#'
#' # create problem with constraints to represent features in contiguous
#' # units that contain highly suitable habitat values
#' # (specifically in the top 5th percentile)
#' cm4 <- lapply(seq_len(terra::nlyr(sim_features)), function(i) {
#'   # create connectivity matrix using the i'th feature's habitat data
#'   m <- connectivity_matrix(sim_pu_raster, sim_features[[i]])
#'   # convert matrix to 0/1 values denoting values in top 5th percentile
#'   m <- round(m > quantile(as.vector(m), 1 - 0.05, names = FALSE))
#'   # remove 0s from the sparse matrix
#'   m <- Matrix::drop0(m)
#'   # return matrix
#'   m
#' })
#' p4 <- p1 %>% add_feature_contiguity_constraints(data = cm4)
#'
#' # solve problems
#' s1 <- c(solve(p1), solve(p2), solve(p3), solve(p4))
#' names(s1) <- c(
#'   "basic solution", "contiguity constraints",
#'   "feature contiguity constraints",
#'   "feature contiguity constraints with data"
#' )
#' # plot solutions
#' plot(s1, axes = FALSE)
#'
#' # create minimal problem with multiple zones, and limit the solver to
#' # 30 seconds to obtain solutions in a feasible period of time
#' p5 <-
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(time_limit = 30, verbose = FALSE)
#'
#' # create problem with contiguity constraints that specify that the
#' # planning units used to conserve each feature in different management
#' # zones must form separate contiguous units
#' p6 <- p5 %>% add_feature_contiguity_constraints(diag(3))
#'
#' # create problem with contiguity constraints that specify that the
#' # planning units used to conserve each feature must form a single
#' # contiguous unit if the planning units are allocated to zones 1 and 2
#' # and do not need to form a single contiguous unit if they are allocated
#' # to zone 3
#' zm7 <- matrix(0, ncol = 3, nrow = 3)
#' zm7[seq_len(2), seq_len(2)] <- 1
#' print(zm7)
#' p7 <- p5 %>% add_feature_contiguity_constraints(zm7)
#'
#' # create problem with contiguity constraints that specify that all of
#' # the planning units in all three of the zones must conserve first feature
#' # in a single contiguous unit but the planning units used to conserve the
#' # remaining features do not need to be contiguous in any way
#' zm8 <- lapply(
#'   seq_len(number_of_features(sim_features_zones)),
#'   function(i) matrix(ifelse(i == 1, 1, 0), ncol = 3, nrow = 3)
#' )
#' print(zm8)
#' p8 <- p5 %>% add_feature_contiguity_constraints(zm8)
#'
#' # solve problems
#' s2 <- lapply(list(p5, p6, p7, p8), solve)
#' s2 <- terra::rast(lapply(s2, category_layer))
#' names(s2) <- c("p5", "p6", "p7", "p8")
#' # plot solutions
#' plot(s2, axes = FALSE)
#' }
#' @name add_feature_contiguity_constraints
#'
#' @exportMethod add_feature_contiguity_constraints
#'
#' @aliases add_feature_contiguity_constraints,ConservationProblem,ANY,matrix-method add_feature_contiguity_constraints,ConservationProblem,ANY,data.frame-method add_feature_contiguity_constraints,ConservationProblem,ANY,Matrix-method add_feature_contiguity_constraints,ConservationProblem,ANY,ANY-method
NULL

methods::setGeneric("add_feature_contiguity_constraints",
  signature = methods::signature("x", "zones", "data"),
  function(x, zones = diag(number_of_zones(x)), data = NULL) {
    rlang::check_required(x)
    rlang::check_required(zones)
    rlang::check_required(data)
    assert(
      is_conservation_problem(x),
      is_inherits(
        data,
        c("NULL", "dgCMatrix", "matrix", "Matrix", "list")
      )
    )
    standardGeneric("add_feature_contiguity_constraints")
  }
)

#' @name add_feature_contiguity_constraints
#' @usage \S4method{add_feature_contiguity_constraints}{ConservationProblem,ANY,data.frame}(x, zones, data)
#' @rdname add_feature_contiguity_constraints
methods::setMethod("add_feature_contiguity_constraints",
  methods::signature("ConservationProblem", "ANY", "data.frame"),
  function(x, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is_inherits(zones, c("matrix", "Matrix", "list")),
      is.data.frame(data)
    )
    # apply constraints
    add_feature_contiguity_constraints(x, zones, data)
})

#' @name add_feature_contiguity_constraints
#' @usage \S4method{add_feature_contiguity_constraints}{ConservationProblem,ANY,matrix}(x, zones, data)
#' @rdname add_feature_contiguity_constraints
methods::setMethod("add_feature_contiguity_constraints",
  methods::signature("ConservationProblem", "ANY", "matrix"),
  function(x, zones, data) {
    # add constraints
    add_feature_contiguity_constraints(x, zones, as_Matrix(data, "dgCMatrix"))
})

#' @name add_feature_contiguity_constraints
#' @usage \S4method{add_feature_contiguity_constraints}{ConservationProblem,ANY,ANY}(x, zones, data)
#' @rdname add_feature_contiguity_constraints
methods::setMethod("add_feature_contiguity_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY"),
  function(x, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is_inherits(zones, c("matrix", "Matrix", "list")),
      is_inherits(data, c("NULL", "matrix", "Matrix", "data.frame", "list"))
    )
    # format zones
    if (inherits(zones, "list")) {
      assert(
        length(zones) == number_of_features(x),
        all_elements_inherit(zones, c("matrix", "Matrix"))
      )
      names(zones) <- paste(x$feature_names(), "zones")
      for (i in seq_along(zones)) {
        zones[[i]] <- as.matrix(zones[[i]])
        assert(
          is.numeric(zones[[i]]),
          all_binary(zones[[i]]),
          all_finite(zones[[i]]),
          isSymmetric(zones[[i]]),
          ncol(zones[[i]]) == number_of_zones(x),
          all(colMeans(zones[[i]]) <= diag(zones[[i]])),
          all(rowMeans(zones[[i]]) <= diag(zones[[i]]))
        )
        colnames(zones[[i]]) <- x$zone_names()
        rownames(zones[[i]]) <- colnames(zones[[i]])
      }
    } else {
      assert(
        is_matrix_ish(zones),
        all_binary(zones),
        all_finite(zones),
        isSymmetric(zones),
        ncol(zones) == number_of_zones(x),
        all(colMeans(zones) <= diag(zones)),
        all(rowMeans(zones) <= diag(zones))
      )
      colnames(zones) <- x$zone_names()
      rownames(zones) <- colnames(zones)
    }
    # format data
    if (is.list(data)) {
      for (i in seq_along(data)) {
        # assert that element is valid
        assert(
          is_inherits(data[[i]], c("matrix", "Matrix", "data.frame")),
          msg = paste(
            "{.arg data[[", i, "]]} is not a {.cls matrix}, {.cls Matrix}",
            "or a data frame."
          )
        )
        # coerce data to dgCMatrix
        if (is.matrix(data[[i]]))
          data[[i]] <- as_Matrix(data, "dgCMatrix")
        if (is.data.frame(data[[i]]))
          data[[i]] <- marxan_connectivity_data_to_matrix(x, data[[i]], TRUE)
        # run checks
        assert(
          all_binary(data[[i]]),
          all_finite(data[[i]]),
          ncol(data[[i]]) == nrow(data[[i]]),
          number_of_total_units(x) == ncol(data[[i]]),
          Matrix::isSymmetric(data[[i]])
        )
      }
    } else if (inherits(data, c("matrix", "dgCMatrix", "data.frame"))) {
      # coerce data to dgCMatrix
      if (is.matrix(data))
        data <- as_Matrix(data, "dgCMatrix")
      if (is.data.frame(data))
        data <- marxan_connectivity_data_to_matrix(x, data[[i]], TRUE)
      # run checks
      assert(
        all_binary(data),
        all_finite(data),
        ncol(data) == nrow(data),
        number_of_total_units(x) == ncol(data),
        Matrix::isSymmetric(data)
      )
    } else if (is.null(data)) {
      # check that planning unit data is spatially explicit
      assert(
        is_pu_spatially_explicit(x),
        msg =
        c(
          paste(
            "{.arg data} must be manually specified (e.g., as a Matrix)."
          ),
          "i" = paste(
            "This is because {.arg x} has planning unit data that are not",
            "spatially explicit",
            "(e.g., {.cls sf}, or {.cls SpatRaster} objects)."
          )
        )
      )
    } else {
      cli::cli_abort("{.arg data} is not a recognized class.")
    }
    # add constraint
    x$add_constraint(pproto(
      "FeatureContiguityConstraint",
      Constraint,
      name = "feature contiguity constraints",
      compressed_formulation = FALSE,
      data = list(data = data, zones = zones),
      calculate = function(self, x) {
        # generate connectivity data
        d <- self$get_data("data")
        if (is.null(d)) {
          if (is.Waiver(x$get_data("adjacency"))) {
            data <- adjacency_matrix(x$data$cost)
            data <- as_Matrix(data, "dgCMatrix")
            x$set_data("adjacency", data)
          }
        }
        # return success
        invisible(TRUE)
      },
      apply = function(self, x, y) {
        assert(
          inherits(x, "OptimizationProblem"),
          inherits(y, "ConservationProblem"),
          .internal = TRUE
        )
        # extract data
        zn <- self$get_data("zones")
        d <- self$get_data("data")
        if (is.null(d)) {
          d <- y$get_data("adjacency")
        }
        # convert data to list format if needed
        if (!is.list(d)) {
          d <- list(d)[rep(1, number_of_features(y))]
        }
        if (!is.list(zn)) {
          zn <- list(zn)[rep(1, number_of_features(y))]
          names(zn) <- paste(y$feature_names(), "zones")
        }
        # convert to dgCMatrix
        d <- lapply(d, as_Matrix, "dgCMatrix")
        # subset matrix data to indices
        ind <- y$planning_unit_indices()
        d <- lapply(d, `[`, ind, ind, drop = FALSE)
        # extract clusters from z
        z_cl <- lapply(seq_along(zn), function(i) {
          igraph::clusters(
            igraph::graph_from_adjacency_matrix(
              zn[[i]], diag = FALSE, mode = "undirected", weighted = NULL
            )
          )$membership * diag(zn[[i]])
        })
        # convert d to lower triangle sparse matrix
        d <- lapply(d, Matrix::forceSymmetric, uplo = "L")
        d <- lapply(d, Matrix::tril)
        d <- lapply(d, as_Matrix, "dgCMatrix")
        # apply the constraints
        if (max(vapply(z_cl, max, numeric(1))) > 0) {
          rcpp_apply_feature_contiguity_constraints(x$ptr, d, z_cl)
        }
      }
    ))
  }
)
