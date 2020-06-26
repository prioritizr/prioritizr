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
#' @param x [ConservationProblem-class()] object.
#'
#' @param zones `matrix`, `Matrix` or `list` object describing
#'   the connection scheme for different zones. For `matrix` or
#'   and `Matrix` arguments, each row and column corresponds
#'   to a different zone in the argument to `x`, and cell values must
#'   contain binary `numeric` values (i.e. one or zero) that indicate
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
#'   matrix (i.e. a matrix with ones along the matrix diagonal and zeros
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
#'   features. See the Details section for more information.
#'
#' @details This function uses connection data to identify solutions that
#'   represent features in contiguous units of dispersible habitat. In earlier
#'   versions of the \pkg{prioritizr} package, it was known as the
#'   `add_corridor_constraints` function but has since been renamed for
#'   clarity. It was inspired by the mathematical formulations detailed in
#'   Onal and Briers (2006) and Cardeira *et al.* 2010. For an
#'   example that has used these constraints, see Hanson, Fuller,
#'   and Rhodes (2018). Please note
#'   that these constraints require the expanded formulation and therefore
#'   cannot be used with feature data that have negative vales.
#'   **Please note that adding these constraints to a problem will
#'   drastically increase the amount of time required to solve it.**
#'
#'   The argument to `data` can be specified in several ways:
#'
#'   \describe{
#'
#'   \item{`NULL`}{connection data should be calculated automatically
#'     using the [adjacency_matrix()] function. This is the default
#'     argument and means that all adjacent planning units are treated
#'     as potentially dispersible for all features.
#'     Note that the connection data must be manually defined
#'     using one of the other formats below when the planning unit data
#'     in the argument to `x` is not spatially referenced (e.g.
#'     in `data.frame` or `numeric` format).}

#'   \item{`matrix`, `Matrix`}{where rows and columns represent
#'     different planning units and the value of each cell indicates if the
#'     two planning units are connected or not. Cell values should be binary
#'     `numeric` values (i.e. one or zero). Cells that occur along the
#'     matrix diagonal have no effect on the solution at all because each
#'     planning unit cannot be a connected with itself. Note that pairs
#'     of connected planning units are treated as being potentially dispersible
#'     for all features.}
#'
#'   \item{`data.frame`}{containing the fields (columns)
#'     `"id1"`, `"id2"`, and `"boundary"`. Here, each row
#'     denotes the connectivity between two planning units following the
#'     *Marxan* format. The field `boundary` should contain
#'     binary `numeric` values that indicate if the two planning units
#'     specified in the fields `"id1"` and `"id2"` are connected
#'     or not. This data can be used to describe symmetric or
#'     asymmetric relationships between planning units. By default,
#'     input data is assumed to be symmetric unless asymmetric data is
#'     also included (e.g. if data is present for planning units 2 and 3, then
#'     the same amount of connectivity is expected for planning units 3 and 2,
#'     unless connectivity data is also provided for planning units 3 and 2).
#'     Note that pairs of connected planning units are treated as being
#'     potentially dispersible for all features.}
#'
#'  \item{`list`}{containing `matrix`, `Matrix`, or
#'     `data.frame` objects showing which planning units
#'     should be treated as connected for each feature. Each element in the
#'     `list` should correspond to a different feature (specifically,
#'     a different target in the problem), and should contain a `matrix`,
#'     `Matrix`, or `data.frame` object that follows the conventions
#'     detailed above.}
#'
#'   }
#'
#' @inherit add_contiguity_constraints return seealso
#'
#' @references
#'
#' @references
#' Onal H and Briers RA (2006) Optimal selection of a connected
#' reserve network. *Operations Research*, 54: 379--388.
#'
#' Cardeira JO, Pinto LS, Cabeza M and Gaston KJ (2010) Species specific
#' connectivity in reserve-network design using graphs.
#' *Biological Conservation*, 2: 408--415.
#'
#' Hanson JO, Fuller RA, & Rhodes JR (2018) Conventional methods for enhancing
#' connectivity in conservation planning do not always maintain gene flow.
#' *Journal of Applied Ecology*, In press:
#' <https://doi.org/10.1111/1365-2664.13315>.
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_zones_stack, sim_features, sim_features_zones)
#'
#' # create minimal problem
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.3)
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
#' # (specifically in the top 1.5th percentile)
#' cm4 <- lapply(seq_len(nlayers(sim_features)), function(i) {
#'   # create connectivity matrix using the i'th feature's habitat data
#'   m <- connectivity_matrix(sim_pu_raster, sim_features[[i]])
#'   # convert matrix to TRUE/FALSE values in top 20th percentile
#'   m <- m > quantile(as.vector(m), 1 - 0.015, names = FALSE)
#'   # convert matrix from TRUE/FALSE to sparse matrix with 0/1s
#'   m <- as(m, "dgCMatrix")
#'   # remove 0s from the sparse matrix
#'   m <- Matrix::drop0(m)
#'   # return matrix
#'   m
#' })
#' p4 <- p1 %>% add_feature_contiguity_constraints(data = cm4)
#' \donttest{
#' # solve problems
#' s1 <- stack(solve(p1), solve(p2), solve(p3), solve(p4))
#'
#' # plot solutions
#' plot(s1,  axes = FALSE, box = FALSE,
#'      main = c("basic solution", "contiguity constraints",
#'               "feature contiguity constraints",
#'               "feature contiguity constraints with data"))
#' }
#' # create minimal problem with multiple zones, and limit the solver to
#' # 30 seconds to obtain solutions in a feasible period of time
#' p5 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
#'       add_default_solver(time_limit = 30) %>%
#'       add_binary_decisions()
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
#' zm8 <- lapply(seq_len(number_of_features(sim_features_zones)), function(i)
#'   matrix(ifelse(i == 1, 1, 0), ncol = 3, nrow = 3))
#' print(zm8)
#' p8 <- p5 %>% add_feature_contiguity_constraints(zm8)
#' \donttest{
#' # solve problems
#' s2 <- lapply(list(p5, p6, p7, p8), solve)
#' s2 <- stack(lapply(s2, category_layer))
#'
#' # plot solutions
#' plot(s2, main = c("p5", "p6", "p7", "p8"), axes = FALSE, box = FALSE)
#' }
#' @name add_feature_contiguity_constraints
#'
#' @exportMethod add_feature_contiguity_constraints
#'
#' @aliases add_feature_contiguity_constraints,ConservationProblem,ANY,matrix-method add_feature_contiguity_constraints,ConservationProblem,ANY,data.frame-method add_feature_contiguity_constraints,ConservationProblem,ANY,Matrix-method add_feature_contiguity_constraints,ConservationProblem,ANY,ANY-method
NULL

methods::setGeneric("add_feature_contiguity_constraints",
  signature = methods::signature("x", "zones", "data"),
  function(x, zones = diag(number_of_zones(x)), data = NULL)
  standardGeneric("add_feature_contiguity_constraints"))

#' @name add_feature_contiguity_constraints
#' @usage \S4method{add_feature_contiguity_constraints}{ConservationProblem,ANY,Matrix}(x, zones, data)
#' @rdname add_feature_contiguity_constraints
methods::setMethod("add_feature_contiguity_constraints",
  methods::signature("ConservationProblem", "ANY", "Matrix"),
  function(x, zones, data) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
     inherits(zones, c("matrix", "Matrix", "list")),
     inherits(data, c("matrix", "Matrix")))
    # apply constraints
    data <- list(data)[rep(1, number_of_features(x))]
    add_feature_contiguity_constraints(x, zones, data)
})

#' @name add_feature_contiguity_constraints
#' @usage \S4method{add_feature_contiguity_constraints}{ConservationProblem,ANY,data.frame}(x, zones, data)
#' @rdname add_feature_contiguity_constraints
methods::setMethod("add_feature_contiguity_constraints",
  methods::signature("ConservationProblem", "ANY", "data.frame"),
  function(x, zones, data) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
     inherits(zones, c("matrix", "Matrix", "list")),
     inherits(data, "data.frame"))
    # apply constraints
    data <- list(data)[rep(1, number_of_features(x))]
    add_feature_contiguity_constraints(x, zones, data)

})

#' @name add_feature_contiguity_constraints
#' @usage \S4method{add_feature_contiguity_constraints}{ConservationProblem,ANY,matrix}(x, zones, data)
#' @rdname add_feature_contiguity_constraints
methods::setMethod("add_feature_contiguity_constraints",
  methods::signature("ConservationProblem", "ANY", "matrix"),
  function(x, zones, data) {
    # add constraints
    add_feature_contiguity_constraints(x, zones, methods::as(data, "dgCMatrix"))
})

#' @name add_feature_contiguity_constraints
#' @usage \S4method{add_feature_contiguity_constraints}{ConservationProblem,ANY,ANY}(x, zones, data)
#' @rdname add_feature_contiguity_constraints
methods::setMethod("add_feature_contiguity_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY"),
  function(x, zones, data) {
    # assert valid arguments
    assertthat::assert_that(inherits(x, "ConservationProblem"),
      inherits(zones, c("matrix", "Matrix", "list")),
      inherits(data, c("NULL", "list")))
    # format zones
    if (inherits(zones, c("matrix", "Matrix"))) {
      zones <- list(zones)[rep(1, x$number_of_features())]
    } else {
      assertthat::assert_that(length(zones) == number_of_features(x),
        all(vapply(zones, inherits, logical(1), c("matrix", "Matrix"))))
    }
    # format data
    if (!is.null(data)) {
      # check argument to data if not NULL
      for (i in seq_along(data)) {
        # assert that element is valid
        assertthat::assert_that(
          inherits(data[[i]], c("matrix", "Matrix", "data.frame")),
          msg = paste0("argument to data[[", i, "]] is not a matrix, Matrix",
            "or data.frame"))
        # coerce to correct format
        if (is.matrix(data[[i]]))
          data[[i]] <- methods::as(data, "dgCMatrix")
        if (is.data.frame(data[[i]]))
          data[[i]] <- marxan_boundary_data_to_matrix(x, data[[i]])
        # run checks
        assertthat::assert_that(all(data[[i]]@x %in% c(0, 1)),
          ncol(data[[i]]) == nrow(data[[i]]),
          number_of_total_units(x) == ncol(data[[i]]),
          all(is.finite(data[[i]]@x)), Matrix::isSymmetric(data[[i]]))
      }
      # create list with data
      d <- list(matrices = data)
    } else {
      # check that planning unit data is spatially referenced
      assertthat::assert_that(inherits(x$data$cost,
                                       c("Spatial", "Raster", "sf")),
        msg = paste("argument to data must be supplied because planning unit",
                    "data are not in a spatially referenced format"))
      d <- list()
    }
    # convert zones to matrix
    for (i in seq_along(zones)) {
      zones[[i]] <- as.matrix(zones[[i]])
      assertthat::assert_that(
        isSymmetric(zones[[i]]), ncol(zones[[i]]) == number_of_zones(x),
        is.numeric(zones[[i]]), all(zones[[i]] %in% c(0, 1)),
        all(colMeans(zones[[i]]) <= diag(zones[[i]])),
        all(rowMeans(zones[[i]]) <= diag(zones[[i]])))
      colnames(zones[[i]]) <- x$zone_names()
      rownames(zones[[i]]) <- colnames(zones[[i]])
    }
    # create list of parameters
    p <- lapply(seq_along(zones), function(i) {
      binary_matrix_parameter(paste(x$feature_names()[i], "zones"),
                              zones[[i]], symmetric = TRUE)
    })
    # add constraint
    x$add_constraint(pproto(
    "FeatureContiguityConstraint",
    Constraint,
    name = "Feature contiguity constraints",
    compressed_formulation = FALSE,
    parameters = do.call(parameters, append(
      list(binary_parameter("apply constraints?", 1L)), p)),
    data = d,
    calculate = function(self, x) {
      # generate connectivity data
      if (is.Waiver(self$get_data("matrices"))) {
        # create matrix
        data <- adjacency_matrix(x$data$cost)
        # coerce matrix to full matrix
        data <- methods::as(data, "dgCMatrix")
        # create list for each feature
        data <- list(data)[rep(1, number_of_features(x))]
        # store data
        self$set_data("matrices", data)
      }
      # return success
      invisible(TRUE)
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      if (as.logical(self$parameters$get("apply constraints?"))) {
        # extract list of connectivity matrices
        ind <- y$planning_unit_indices()
        # format matrices
        d <- self$get_data("matrices")
        d <- lapply(d, `[`, ind, ind, drop = FALSE)
        # extract clusters from z
        z <- list()
        for (i in seq_len(y$number_of_features()))
        z[[i]] <- self$parameters$get(paste(y$feature_names()[i], "zones"))
        z_cl <- lapply(seq_along(z), function(i) {
          igraph::clusters(igraph::graph_from_adjacency_matrix(z[[i]],
            diag = FALSE, mode = "undirected", weighted = NULL))$membership *
            diag(z[[i]])
        })
        # convert d to lower triangle sparse matrix
        d <- lapply(d, Matrix::forceSymmetric, uplo = "L")
        d <- lapply(d, `class<-`, "dgCMatrix")
        # apply the constraints
        if (max(vapply(z_cl, max, numeric(1))) > 0)
          rcpp_apply_feature_contiguity_constraints(x$ptr, d, z_cl)
      }
    }))
})
