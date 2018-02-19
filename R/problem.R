#' @include internal.R ConservationProblem-proto.R
NULL

#' Conservation planning problem
#'
#' Create a systematic conservation planning problem. This function is used to
#' specify the basic data used in a spatial prioritization problem: the
#' spatial distribution of the planning units and their costs, as well as
#' the features (e.g. species, ecosystems) that need to be conserved. After
#' constructing this \code{ConservationProblem-class} object, it can be
#' customized to meet specific goals using \code{\link{objectives}},
#' \code{\link{targets}}, \code{\link{constraints}}, and
#' \code{\link{penalties}}.
#'
#' The basic prioritizr work flow starts with formulating the
#' \code{problem}, then adding \code{objectives} and \code{targets},
#' as well as \code{constraints} and \code{penalties} as needed.
#' Alternative decision formats can be specified using the
#' \code{\link{decisions}} functions,
#' otherwise the package will default to binary selection of planning units.
#' Lastly, the type of solving algorithm must be specified
#' (see \code{\link{solvers}}). Once formulated, the problem is solved
#' using the \code{solve()} function, which will return
#' a \code{RasterLayer-class},
#' \code{Spatial-class}, or a \code{numeric} vector
#' containing the solution depending on the \code{ConservationProblem-class}
#'
#' @param x \code{\link[raster]{Raster-class}},
#'   \code{\link[sp]{SpatialPolygonsDataFrame-class}},
#'   \code{\link[sp]{SpatialLinesDataFrame-class}}, or
#'   \code{\link{data.frame}} object, or \code{\link{numeric}} vector,
#'   specifying the planning units to use in the reserve design exercise and
#'   their corresponding cost. It may be desirable to exclude some planning
#'   units from the analysis, for example those outside the study area. To
#'   exclude planning units, set the cost for those raster cells to \code{NA},
#'   or use the \code{add_locked_out_constraint}.
#'
#' @param features The correct argument for \code{features} depends on the
#'   input to \code{x}.
#'   \describe{
#'     \item{\code{\link[raster]{Raster-class}},
#'       \code{\link[sp]{Spatial-class}}}{\code{\link[raster]{Raster-class}}
#'       object showing the distribution of conservation features. Missing
#'       values (i.e. \code{NA} values) can be used to indicate the absence of
#'       a feature in a particular cell instead of explicitly setting these
#'       cells to zero.}
#'
#'     \item{\code{\link{Spatial}}, \code{data.frame}}{\code{character} vector
#'       with column names that correspond to the abundance or occurrence of
#'       different features in each planning unit.}
#'
#'     \item{\code{\link{Spatial}}, \code{data.frame} or
#'       \code{numeric}}{\code{data.frame} object
#'       containing information on the features. The argument to
#'       \code{feature_data} must follow the conventions used by Marxan. Each
#'       row corresponds to a different feature. It must also contain the
#'       following columns:
#'       \describe{
#'         \item{\code{"id"}}{\code{integer} unique identifier for each feature
#'           These identifiers are used in the argument to \code{rij}.}
#'         \item{\code{"name"}}{\code{character} name for each feature.}
#'         \item{\code{"prop"}}{\code{numeric} relative target for each feature
#'           (optional).}
#'         \item{\code{"amount"}}{\code{numeric} absolute target for each
#'           feature (optional).}
#'        }
#'     }
#'  }
#'
#' @param cost_column \code{character} name or \code{integer} indicating the
#'   column with the cost data. This argument must be supplied when the
#'   argument to \code{x} is a \code{link{Spatial}} or \code{\link{data.frame}}
#'   object.
#'
#' @param rij \code{data.frame} containing information on the amount of
#'    each feature in each planning unit. This argument is only used if
#'    \code{x} is a \code{data.frame}. Similar to \code{features}, the
#'    argument to \code{rij} must follow the conventions used by
#'    Marxan. It must contain the following columns:
#'    \describe{
#'      \item{\code{"pu"}}{\code{integer} planning unit identifier.}
#'      \item{\code{"species"}}{\code{integer} feature identifier.}
#'      \item{\code{"amount"}}{\code{numeric} amount of the feature in the
#'        planning unit.}
#'     }
#'
#' @param rij_matrix \code{matrix} or \code{\link[Matrix]{dgCMatrix-class}}
#'    object specifying the amount of each feature (rows) within each planning
#'    unit (columns). Only used when \code{x} is a numeric vector of costs.
#'    
#' @param run_checks \code{logical} flag indicating whether checks should be run 
#'    to ensure the integrity of the input data. These checks are run by 
#'    default; however, for large data sets they may increase the time to 
#'    run \code{problem()}. If it is taking a prohibitively long time to 
#'    compile the prioritization problem, it is suggested to try setting 
#'    \code{run_checks} to \code{FALSE}.
#'
#' @param ... not used.
#'
#' @details A reserve design exercise starts by dividing the study region
#' into planning units (typically square or hexagonal cells) and, for
#' each planning unit, assigning values that quantify socioeconomic
#' cost and conservation benefit for a set of conservation features. The
#' cost can be the acquisition cost of the land, the cost of management,
#' the opportunity cost of foregone commercial activities (e.g. from logging
#' or agriculture), or simply the area. The conservation features are
#' typically species (e.g. Clouded Leopard) or habitats (e.g. mangroves or
#' cloud forest). The benefit that each feature derives from a planning unit
#' can take a variety of forms, but is typically either occupancy (i.e.
#' presence or absence) or area of occurrence within each planning unit.
#' Finally, in some types of reserve design models, for each conservation
#' feature, representation targets must be set, such as 20% of the current
#' extent of cloud forest or 10,000km^2 of Clouded Leopard habitat
#' (see \code{\link{targets}}).
#'
#' The goal of the reserve design exercise is then to optimize the trade-off
#' between conservation benefit and socioeconomic cost, i.e. to get the most
#' benefit for your limited conservation funds. In general, the goal of an
#' optimization problem is to minimize an objective function over a set of
#' decision variables, subject to a series of constraints. The decision
#' variables
#' are what we control, usually there is one binary variable for each planning
#' unit specifying whether or not to protect that unit (but other approaches are
#' available, see \code{\link{decisions}}). The constraints can be thought
#' of as rules that need to be followed, for example, that the reserve must
#' stay within a certain budget or meet the representation targets.
#'
#' Integer linear programming (ILP) is the subset of optimization algorithms
#' used in this package to solve reserve design problems. The general form of
#' an ILP problem can be expressed in matrix notation as:
#'
#' \deqn{\mathit{Minimize} \space \mathbf{c}^{\mathbf{T}}\mathbf{x} \space
#' \mathit{subject \space to} \space
#' \mathbf{Ax}\geq= or\leq \mathbf{b}}{Minimize (c^T)*x subject to Ax \ge, =,
#' or \le b}
#'
#' Where \emph{x} is a vector of decision variables, \emph{c} and \emph{b} are
#' vectors of known coefficients, and \emph{A} is the \strong{constraint
#' matrix}. The final term specifies a series of \strong{structural constraints}
#' where relational operators for the constraint can be either \eqn{\ge, =, or
#' \le} the coefficients. For example, in the minimum set cover problem,
#' \emph{c} would be a vector of costs for each planning unit, \emph{b} a
#' vector of targets for each conservation feature, the relational operator
#' would be \eqn{\ge} for all features, and \emph{A} would be the
#' representation matrix with \eqn{A_{ij}=r_{ij}}{Aij = rij}, the
#' representation level of feature \emph{i} in planning unit \emph{j}.
#'
#' @return A \code{\link{ConservationProblem-class}} object containing the
#'   basic data used to build a prioritization problem.
#'
#' @seealso \code{\link{constraints}},  \code{\link{decisions}},
#'  \code{\link{objectives}} \code{\link{penalties}},
#'  \code{\link{portfolios}}, \code{\link{problem}},
#'  \code{\link{solvers}}, \code{\link{targets}}.
#'
#' @aliases problem,Raster,Raster-method problem,Spatial,Raster-method problem,data.frame,data.frame-method problem,numeric,data.frame-method problem,data.frame,character-method problem,Spatial,character-method
#'
#' @exportMethod problem
#'
#' @name problem
#'
#' @examples
#' # create problem using raster planning unit data
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions()
#'
#' # create problem using polygon planning unit data
#' p2 <- problem(sim_pu_polygons, sim_features, "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions()
#'
#' # create problem using line planning unit data
#' p3 <- problem(sim_pu_lines, sim_features, "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions()
#'
#' # create problem using point planning unit data
#' p4 <- problem(sim_pu_points, sim_features, "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions()
#'
#' # add columns to polygon planning unit data representing the abundance
#' # of species inside them
#' sim_pu_polygons$spp_1 <- rpois(length(sim_pu_polygons), 5)
#' sim_pu_polygons$spp_2 <- rpois(length(sim_pu_polygons), 8)
#' sim_pu_polygons$spp_3 <- rpois(length(sim_pu_polygons), 2)
#'
#' # create problem using pre-processed data when feature abundances are
#' # stored in the columns of an attribute table for a spatial vector data set
#' p5 <- problem(sim_pu_polygons, features = c("spp_1", "spp_2", "spp_3"),
#'               "cost") %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions()
#'
#' # alternatively one can supply pre-processed aspatial data
#' costs <- sim_pu_polygons$cost
#' features <- data.frame(id = seq_len(nlayers(sim_features)),
#'                        name = names(sim_features))
#' rij_mat <- rij_matrix(sim_pu_polygons, sim_features)
#' p6 <- problem(costs, features, rij_matrix = rij_mat) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.2) %>%
#'       add_binary_decisions()
#'
#' \donttest{
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#' s4 <- solve(p4)
#' s5 <- solve(p5)
#' s6 <- solve(p6)
#'
#' # plot solutions for problems associated with spatial data
#' par(mfrow = c(3, 2), mar = c(0, 0, 4.1, 0))
#' plot(s1, main = "raster data", axes = FALSE, box = FALSE)
#'
#' plot(s2, main = "polygon data")
#' plot(s2[s2$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s3, main = "line data")
#' lines(s3[s3$solution_1 == 1, ], col = "darkgreen", lwd = 2)
#'
#' plot(s4, main = "point data", pch = 19)
#' points(s4[s4$solution_1 == 1, ], col = "darkgreen", cex = 2, pch = 19)
#'
#' plot(s5, main = "preprocessed data", pch = 19)
#' plot(s5[s5$solution_1 == 1, ], col = "darkgreen", add = TRUE)
#'
#' # show solutions for problems associated with aspatial data
#' str(s6)
#' }
#'
#' @export
methods::setGeneric("problem",
                    signature = methods::signature("x", "features"),
                    function(x, features, ...) standardGeneric("problem"))

#' @name problem
#' @usage \S4method{problem}{Raster,Raster}(x, features, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Raster", features = "Raster"),
  function(x, features, run_checks = TRUE, ...) {
    assertthat::assert_that(
      inherits(x, "Raster"), inherits(features, "Raster"),
      raster::nlayers(x) == 1, raster::nlayers(features) >= 1)
    if (run_checks) {
      assertthat::assert_that(
        isTRUE(raster::cellStats(x, "min") > 0),
        isTRUE(all(raster::cellStats(features, "min") >= 0)),
        raster::compareRaster(x, features, res = TRUE, tolerance = 1e-5,
                              stopiffalse = FALSE))
    }
    if (inherits(x, c("RasterStack", "RasterBrick")))
      x <- x[[1]]
    pproto(NULL, ConservationProblem,
           constraints = pproto(NULL, Collection),
           penalties = pproto(NULL, Collection),
           data = list(cost = x, features = features,
                       rij_matrix = rij_matrix(x, features)))
})

#' @name problem
#' @usage \S4method{problem}{Spatial,Raster}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "Raster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    assertthat::assert_that(
      inherits(x, c("SpatialPolygonsDataFrame", "SpatialLinesDataFrame",
                    "SpatialPointsDataFrame")),
      assertthat::is.string(cost_column))
    cost_column <- match.arg(cost_column, names(x))
    x <- x[is.finite(x[[cost_column]]), ]
    assertthat::assert_that(
      length(x) > 0,
      raster::nlayers(features) >= 1,
      raster::compareCRS(x@proj4string, features@crs))
    if (run_checks) {
      assertthat::assert_that(
        isTRUE(all(x[[cost_column]] > 0)),
        isTRUE(all(raster::cellStats(features, "min") >= 0)),
        isTRUE(rgeos::gIntersects(methods::as(raster::extent(x),
                                              "SpatialPolygons"),
                                  methods::as(raster::extent(features), 
                                              "SpatialPolygons"))))
    }
    pproto(NULL, ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(cost = x, features = features, cost_column = cost_column,
                  rij_matrix = rij_matrix(x[, cost_column], features)))
})

#' @name problem
#' @usage \S4method{problem}{data.frame,data.frame}(x, features, rij, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "data.frame", features = "data.frame"),
  function(x, features, rij, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      # inputs are data.frames
      inherits(x, "data.frame"), inherits(features, "data.frame"),
      inherits(rij, "data.frame"),
      # x$cost
      assertthat::has_name(x, "cost"), is.numeric(x$cost),
      all(is.finite(x$cost)),
      # x$id
      assertthat::has_name(x, "id"), is.numeric(x$id), all(is.finite(x$id)),
      anyDuplicated(x$id) == 0,
      # features$id
      assertthat::has_name(features, "id"), is.numeric(features$id),
      all(is.finite(features$id)), anyDuplicated(features$id) == 0,
      # features$name
      assertthat::has_name(features, "name"),
      is.character(features$name) || is.factor(features$name),
      all(!is.na(features$name)), anyDuplicated(features$name) == 0,
      # rij$species
      assertthat::has_name(rij, "species"), is.numeric(rij$species),
      all(is.finite(rij$species)),
      all(rij$species %in% features$id),
      # rij$pu
      assertthat::has_name(rij, "pu"), is.numeric(rij$pu),
      all(is.finite(rij$pu)), all(rij$pu %in% x$id),
      # rij$amount
      assertthat::has_name(rij, "amount"), is.numeric(rij$amount),
      all(is.finite(rij$amount)))
    # standardize ids
    rij$pu <- match(rij$pu, x$id)
    rij$species <- match(rij$species, features$id)
    # create rij matrix
    rij_mat <- Matrix::sparseMatrix(i = rij$species, j = rij$pu,
                                    x = rij$amount, giveCsparse = TRUE,
                                    index1 = TRUE, use.last.ij = FALSE,
                                    dims = c(nrow(features), nrow(x)))
    # create new problem object
    p <- pproto(NULL, ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(cost = x, features = features, cost_column = "cost",
                  rij_matrix = rij_mat))
    # return problem
    return(p)
})

#' @name problem
#' @usage \S4method{problem}{numeric,data.frame}(x, features, rij_matrix, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "numeric", features = "data.frame"),
  function(x, features, rij_matrix, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      # data types
      is.numeric(x), all(is.finite(x)),
      inherits(features, "data.frame"),
      inherits(rij_matrix, c("dgCMatrix", "dgTMatrix", "matrix")),
      # features$id
      assertthat::has_name(features, "id"), is.numeric(features$id),
      all(is.finite(features$id)), anyDuplicated(features$id) == 0,
      # features$name
      assertthat::has_name(features, "name"),
      is.character(features$name) || is.factor(features$name),
      all(!is.na(features$name)), anyDuplicated(features$name) == 0,
      # correct matrix dimensions
      nrow(rij_matrix) == nrow(features),
      ncol(rij_matrix) == length(x))
    # convert to sparse matrix if necessary
    if (!inherits(rij_matrix, "dgCMatrix")) {
      rij_matrix[which(is.na(rij_matrix))] <- 0
      rij_matrix <- methods::as(rij_matrix, "dgCMatrix")
    }
    # create new problem object
    p <- pproto(NULL, ConservationProblem,
                constraints = pproto(NULL, Collection),
                penalties = pproto(NULL, Collection),
                data = list(cost = x, features = features,
                            rij_matrix = rij_matrix))
    # return problem
    return(p)
})

#' @name problem
#' @usage \S4method{problem}{data.frame,character}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "data.frame", features = "character"),
  function(x, features, cost_column, ...) {
    assertthat::assert_that(assertthat::is.string(cost_column))
    cost_column <- match.arg(cost_column, names(x))
    x <- x[is.finite(x[[cost_column]]), ]
    assertthat::assert_that(
      isTRUE(all(x[[cost_column]] > 0)),
      length(x) > 0,
      all(features %in% names(x)),
      all(colSums(!is.na(as.matrix(x[, features, drop = FALSE]))) > 0))
    # create rij matrix
    rij_mat <- t(as.matrix(x[, features, drop = FALSE]))
    rij_mat[is.na(rij_mat)] <- 0
    rij_mat <- methods::as(rij_mat, "sparseMatrix")
    pproto(NULL, ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(cost = x, features = features, cost_column = cost_column,
                  rij_matrix = rij_mat))
})

#' @name problem
#' @usage \S4method{problem}{Spatial,character}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "character"),
  function(x, features, cost_column, ...) {
    assertthat::assert_that(inherits(x, c("SpatialPolygonsDataFrame",
      "SpatialLinesDataFrame", "SpatialPointsDataFrame")))
    cost_column <- match.arg(cost_column, names(x))
    x <- x[is.finite(x[[cost_column]]), ]
    assertthat::assert_that(
      isTRUE(all(x[[cost_column]] > 0)),
      length(x) > 0,
      all(features %in% names(x)),
      all(colSums(!is.na(as.matrix(x@data[, features, drop = FALSE]))) > 0))
    # create rij matrix
    rij_mat <- t(as.matrix(x@data[, features, drop = FALSE]))
    rij_mat[is.na(rij_mat)] <- 0
    rij_mat <- methods::as(rij_mat, "sparseMatrix")
    pproto(NULL, ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(cost = x, features = features, cost_column = cost_column,
                  rij_matrix = rij_mat))
})
