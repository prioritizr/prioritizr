#' @include internal.R ConservationProblem-proto.R
NULL

#' Conservation planning problem
#'
#' Create a systematic conservation planning problem. This function is used to
#' specify the basic data used in a spatial prioritization problem: the
#' spatial distribution of the planning units and their costs, as well as
#' the features (eg. species, ecosystems) that need to be conserved. After
#' constructing this object, it can be customized to meet specific
#' objectives using targets (see \code{\link{targets}}) and constraints
#' (see \code{\link{constraints}}).
#'
#' @param x \code{\link[raster]{Raster-class}},
#'   \code{\link[sp]{SpatialPolygonsDataFrame-class}},
#'   \code{\link[sp]{SpatialLinesDataFrame-class}}, or 
#'   \code{\link{data.frame}} object, or \code{\link{numeric}} vector,
#'   specifying the planning units to use in the reserve design exercise and 
#'   their corresponding cost. It may be desirable to exclude some planning 
#'   units from the analysis, for example those outside the study area. To 
#'   exclude planning units, set the cost for those raster cells to \code{NA}.
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
#'     \item{\code{data.frame} or \code{numeric}}{\code{data.frame} object 
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
#'   column in the attribute table of a \code{\link[sp]{Spatial-class}} object
#'   with the cost data.
#'
#' @param rij \code{data.frame} containing information on the amount of
#'    each feature in each planning unit. This argument is only used argument to
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
#' @param ... not used.
#'
#' @return A \code{\link{ConservationProblem-class}} object containing the
#'   basic data used to build a prioritization problem.
#'
#' @seealso \code{\link{constraints}}, \code{\link{objectives}},
#'  \code{\link{targets}}.
#'
#' @examples
#' # create problem using raster planning unit data
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'  add_min_set_objective() %>%
#'  add_relative_targets(0.2) %>%
#'  add_binary_decisions()
#'
#' # create problem using polygon planning unit data
#' p2 <- problem(sim_pu_polygons, sim_features) %>%
#'  add_min_set_objective() %>%
#'  add_relative_targets(0.2) %>%
#'  add_binary_decisions()
#'
#' # create problem using line planning unit data
#' p3 <- problem(sim_pu_lines, sim_features) %>%
#'  add_min_set_objective() %>%
#'  add_relative_targets(0.2) %>%
#'  add_binary_decisions()
#'
#' # create problem using point planning unit data
#' p4 <- problem(sim_pu_points, sim_features) %>%
#'  add_min_set_objective() %>%
#'  add_relative_targets(0.2) %>%
#'  add_binary_decisions()
#'  
#' # alternatively one can supply pre-processed, aspatial data
#' costs <- sim_pu_polygons$cost
#' features <- data.frame(id = 1:nlayers(sim_features),
#'                        name = names(sim_features))
#' rij_mat <- rij_matrix(sim_pu_polygons, sim_features)
#' p5 <- problem(costs, features, rij_matrix = rij_mat) %>% 
#'  add_min_set_objective() %>%
#'  add_relative_targets(0.2) %>%
#'  add_binary_decisions()
#' \donttest{
#' # solve problems
#' s <- list(solve(p1), solve(p2), solve(p3), solve(p4), solve(p5))
#'
#' # plot solutions
#' par(mfrow=c(2,2))
#' plot(s[[1]], main = "raster data")
#'
#' plot(s[[2]], main = "polygon data")
#' plot(s[[2]][s[[2]]$solution == 1, ], col = "darkgreen", add = TRUE)
#'
#' plot(s[[3]], main = "line data")
#' lines(s[[3]][s[[3]]$solution == 1, ], col = "darkgreen", lwd = 2)
#'
#' plot(s[[4]], main = "point data", pch = 19)
#' points(s[[4]][s[[4]]$solution == 1, ], col = "darkgreen", cex = 2, pch = 19)
#' }
#'
#' @export
problem <- function(x, features, ...) UseMethod("problem")

#' @rdname problem
#' @method problem Raster
#' @export
problem.Raster <- function(x, features, ...) {
  assertthat::assert_that(inherits(x, "Raster"), inherits(features, "Raster"))
  assertthat::assert_that(isTRUE(raster::cellStats(x, "min") > 0),
    isTRUE(all(raster::cellStats(features, "max") > 0)),
    raster::nlayers(x) == 1, raster::nlayers(features) >= 1,
    raster::compareRaster(x, features, res = TRUE, tolerance = 1e-5,
      stopiffalse = FALSE))
  if (inherits(x, c("RasterStack", "RasterBrick")))
    x <- x[[1]]
  pproto(NULL, ConservationProblem,
   constraints = pproto(NULL, Collection), penalties = pproto(NULL, Collection),
                        data = list(cost = x, features = features,
                                    rij_matrix = rij_matrix(x, features)))
}

#' @rdname problem
#' @method problem Spatial
#' @export
problem.Spatial <- function(x, features, cost_column = names(x)[1], ...) {
  assertthat::assert_that(inherits(x, c("SpatialPolygonsDataFrame",
    "SpatialLinesDataFrame", "SpatialPointsDataFrame")))
  cost_column <- match.arg(cost_column, names(x))
  x <- x[is.finite(x[[cost_column]]), ]
  assertthat::assert_that(
    isTRUE(all(x[[1]] > 0)),
    isTRUE(all(raster::cellStats(features, "max", na.rm = TRUE) > 0)),
    raster::nlayers(features) >= 1,
    raster::compareCRS(x@proj4string, features@crs),
    isTRUE(rgeos::gIntersects(methods::as(raster::extent(x), "SpatialPolygons"),
      methods::as(raster::extent(features), "SpatialPolygons"))))
  pproto(NULL, ConservationProblem,
    constraints = pproto(NULL, Collection),
    penalties = pproto(NULL, Collection),
    data = list(cost = x, features = features, cost_column = cost_column,
                rij_matrix = rij_matrix(x[, cost_column], features)))
}

#' @rdname problem
#' @method problem data.frame
#' @export
problem.data.frame <- function(x, features, rij, ...) {
  # assert that arguments are valid
  assertthat::assert_that(
    # inputs are data.frames
    inherits(x, "data.frame"), inherits(features, "data.frame"),
    inherits(rij, "data.frame"),
    # x$cost
    assertthat::has_name(x, "cost"), is.numeric(x$cost), all(is.finite(x$cost)),
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
    all(is.finite(rij$x)), all(rij$pu %in% x$id),
    # rij$amount
    assertthat::has_name(rij, "amount"), is.numeric(rij$amount),
    all(is.finite(rij$amount)))
  # standardize ids
  rij$pu <- match(rij$pu, x$id)
  rij$species <- match(rij$species, features$id)
  # create rij matrix
  rij_mat <- Matrix::sparseMatrix(i = rij$species, j = rij$pu,
                                  x = rij$amount, giveCsparse = TRUE,
                                  index1 = TRUE, use.last.ij = FALSE)
  # create new problem object
  p <- pproto(NULL, ConservationProblem,
    constraints = pproto(NULL, Collection),
    penalties = pproto(NULL, Collection),
    data = list(cost = x, features = features, cost_column = "cost",
                rij_matrix = rij_mat))
  # return problem
  return(p)
}

#' @rdname problem
#' @method problem numeric
#' @export
problem.numeric <- function(x, features, rij_matrix, ...) {
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
}
