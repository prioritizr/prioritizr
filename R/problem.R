#' @include internal.R ConservationProblem-proto.R zones.R
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
#' \code{\link{penalties}}. After building the problem, the
#'  \code{\link{solve}} function can be used to identify solutions.
#'
#' @param x \code{\link[raster]{Raster-class}},
#'   \code{\link[sp]{SpatialPolygonsDataFrame-class}},
#'   \code{\link[sp]{SpatialLinesDataFrame-class}}, or
#'   \code{\link{data.frame}} object, \code{\link{numeric}} vector, or
#'   \code{\link{matrix}} specifying the planning units to use in the reserve
#'   design exercise and their corresponding cost. It may be desirable to
#'   exclude some planning units from the analysis, for example those outside
#'   the study area. To exclude planning units, set the cost for those raster
#'   cells to \code{NA}, or use the \code{add_locked_out_constraint}.
#'
#' @param features The correct argument for \code{features} depends on the
#'   input to \code{x}:
#'
#'   \describe{
#'
#'   \item{\code{\link[raster]{RasterLayer-class}},
#'     \code{\link[sp]{Spatial-class}}}{\code{\link[raster]{Raster-class}}
#'     object showing the distribution of conservation features. Missing
#'     values (i.e. \code{NA} values) can be used to indicate the absence of
#'     a feature in a particular cell instead of explicitly setting these
#'     cells to zero. Note that this argument type for \code{features} can
#'     only be used to specify data for problems involving a single zone.}
#'
#'   \item{\code{\link[raster]{RasterStack-class}},
#'     \code{\link[raster]{RasterBrick-class}}
#'     \code{\link[sp]{Spatial-class}}}{\code{ZonesRaster}
#'     object showing the distribution of conservation features in multiple
#'     zones. As above, missing values (i.e. \code{NA} values) can be used to
#'     indicate the absence of a feature in a particular cell instead of
#'     explicitly setting these cells to zero. Note that this argument type
#'     is explicitly designed for creating problems with spatial data that
#'     contain multiple zones.}
#'
#'   \item{\code{\link{Spatial}}, \code{data.frame}}{\code{character} vector
#'     with column names that correspond to the abundance or occurrence of
#'     different features in each planning unit. Note that this argument
#'     type can only be used to create problems involving a single zone.}
#'
#'   \item{\code{\link{Spatial}},
#'     \code{data.frame}}{\code{\link{ZonesCharacter}}
#'     object with column names that correspond to the abundance or
#'     occurrence of different features in each planning unit in different
#'     zones. Note that this argument type is designed specifically for
#'     problems involving multiple zones.}
#'
#'   \item{\code{\link{Spatial}}, \code{data.frame}, \code{numeric} vector,
#'     \code{\link{matrix}}}{\code{data.frame} object
#'     containing the names of the features. Note that if this
#'     type of argument is supplied to \code{features} then the argument
#'     \code{rij} or \code{rij_matrix} must also be supplied. This type of
#'     argument should follow the conventions used by \emph{Marxan}, wherein
#'     each row corresponds to a different feature. It must also contain the
#'     following columns:
#'
#'     \describe{
#'
#'     \item{\code{"id"}}{\code{integer} unique identifier for each feature
#'       These identifiers are used in the argument to \code{rij}.}
#'     \item{\code{"name"}}{\code{character} name for each feature.}
#'     \item{\code{"prop"}}{\code{numeric} relative target for each feature
#'       (optional).}
#'     \item{\code{"amount"}}{\code{numeric} absolute target for each
#'       feature (optional).}
#'
#'     }}
#'
#'  }
#'
#' @param cost_column \code{character} name or \code{integer} indicating the
#'   column(s) with the cost data. This argument must be supplied when the
#'   argument to \code{x} is a \code{\link[sp]{Spatial}} or
#'   \code{data.frame} object. This argument should contain the name of each
#'   column containing cost data for each management zone when creating
#'   problems with multiple zones. To create a problem with a single zone, then
#'   set the argument to \code{cost_column} as a single column name.
#'
#' @param rij \code{data.frame} containing information on the amount of
#'    each feature in each planning unit assuming each management zone. Similar
#'    to \code{data.frame} arguments for \code{features}, the \code{data.frame}
#'    objects must follow the conventions used by \emph{Marxan}. Note that the
#'    \code{"zone"} column is not needed for problems involving a single
#'    management zone. Specifically, the argument should contain the following
#'    columns:
#'
#'    \describe{
#'
#'    \item{\code{"pu"}}{\code{integer} planning unit identifier.}
#'
#'    \item{\code{"species"}}{\code{integer} feature identifier.}
#'
#'    \item{\code{"zone"}}{\code{integer} zone identifier (optional for
#'      problems involving a single zone).}
#'
#'    \item{\code{"amount"}}{\code{numeric} amount of the feature in the
#'      planning unit.}
#'
#'    }
#'
#' @param rij_matrix \code{list} of \code{matrix} or
#'    \code{\link[Matrix]{dgCMatrix-class}}
#'    objects specifying the amount of each feature (rows) within each planning
#'    unit (columns) for each zone. The \code{list} elements denote
#'    different zones, matrix rows denote features, and matrix columns denote
#'    planning units. For convenience, the argument to
#'    \code{rij_matrix} can be a single \code{matrix} or
#'    \code{\link[Matrix]{dgCMatrix-class}} when specifying a problem with a
#'    single management zone. This argument is only used when the argument
#'    to \code{x} is a \code{numeric} or \code{matrix} object.
#'
#' @param zones \code{data.frame} containing information on the zones. This
#'   argument is only used when argument to \code{x} and \code{y} are
#'   both \code{data.frame} objects and the problem being built contains
#'   multiple zones. Following conventions used in \code{MarZone}, this
#'   argument should contain the following columns:
#'   columns:
#'
#'   \describe{
#'
#'   \item{\code{"id"}}{\code{integer} zone identifier.}
#'
#'   \item{\code{"name"}}{\code{character} zone name.}
#'
#'   }
#'
#' @param run_checks \code{logical} flag indicating whether checks should be
#'   run to ensure the integrity of the input data. These checks are run by
#'   default; however, for large data sets they may substantially increase run
#'   time. If it is taking a prohibitively long time to create the
#'   prioritization problem, it is suggested to try setting \code{run_checks}
#'   to \code{FALSE}.
#'
#' @param ... not used.
#'
#' @details A reserve design exercise starts by dividing the study region
#'   into planning units (typically square or hexagonal cells) and, for
#'   each planning unit, assigning values that quantify socioeconomic
#'   cost and conservation benefit for a set of conservation features. The
#'   cost can be the acquisition cost of the land, the cost of management,
#'   the opportunity cost of foregone commercial activities (e.g. from logging
#'   or agriculture), or simply the area. The conservation features are
#'   typically species (e.g. Clouded Leopard) or habitats (e.g. mangroves or
#'   cloud forest). The benefit that each feature derives from a planning unit
#'   can take a variety of forms, but is typically either occupancy (i.e.
#'   presence or absence) or area of occurrence within each planning unit.
#'   Finally, in some types of reserve design models, representation targets
#'   must be set for each conservation feature, such as 20 % of the current
#'   extent of cloud forest or 10,000 km^2 of Clouded Leopard habitat
#'   (see \code{\link{targets}}).
#'
#'   The goal of the reserve design exercise is then to optimize the trade-off
#'   between conservation benefit and socioeconomic cost, i.e. to get the most
#'   benefit for your limited conservation funds. In general, the goal of an
#'   optimization problem is to minimize an objective function over a set of
#'   decision variables, subject to a series of constraints. The decision
#'   variables are what we control, usually there is one binary variable for
#'   each planning unit specifying whether or not to protect that unit (but
#'   other approaches are available, see \code{\link{decisions}}). The
#'   constraints can be thought of as rules that need to be followed, for
#'   example, that the reserve must stay within a certain budget or meet the
#'   representation targets.
#'
#'   Integer linear programming (ILP) is the subset of optimization algorithms
#'   used in this package to solve reserve design problems. The general form of
#'   an integer programming problem can be expressed in matrix notation using
#'   the following equation.
#'
#'   \deqn{\mathit{Minimize} \space \mathbf{c}^{\mathbf{T}}\mathbf{x} \space
#'   \mathit{subject \space to} \space
#'   \mathbf{Ax}\geq= or\leq \mathbf{b}}{Minimize (c^T)*x subject to Ax \ge, =,
#'   or \le b}
#'
#'   Here, \emph{x} is a vector of decision variables, \emph{c} and \emph{b} are
#'   vectors of known coefficients, and \emph{A} is the constraint
#'   matrix. The final term specifies a series of structural
#'   constraints where relational operators for the constraint can be either
#'   \eqn{\ge, =, or \le} the coefficients. For example, in the minimum set
#'   cover problem, \emph{c} would be a vector of costs for each planning unit,
#'   \emph{b} a vector of targets for each conservation feature, the relational
#'   operator would be \eqn{\ge} for all features, and \emph{A} would be the
#'   representation matrix with \eqn{A_{ij}=r_{ij}}{Aij = rij}, the
#'   representation level of feature \emph{i} in planning unit \emph{j}.
#'
#'   Please note that this function internally computes the amount of each
#'   feature in each planning unit when this data is not supplied (using the
#'   \code{rij_matrix} parameter). As a consequence, it can take a while to
#'   initialize large-scale conservation planning problems that involve
#'   millions of planning units.
#'
#' @return A \code{\link{ConservationProblem-class}} object containing the
#'   basic data used to build a prioritization problem.
#'
#' @seealso \code{\link{constraints}},  \code{\link{decisions}},
#'  \code{\link{objectives}} \code{\link{penalties}},
#'  \code{\link{portfolios}}, \code{\link{problem}},
#'  \code{\link{solvers}}, \code{\link{targets}}.
#'
#' @aliases problem,Raster,Raster-method problem,Spatial,Raster-method problem,data.frame,data.frame-method problem,numeric,data.frame-method problem,data.frame,character-method problem,Spatial,character-method problem,Raster,ZonesRaster-method problem,Spatial,ZonesRaster-method problem,Spatial,ZonesCharacter-method problem,data.frame,ZonesCharacter-method problem,matrix,data.frame-method
#'
#' @exportMethod problem
#'
#' @name problem
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_polygons, sim_pu_lines, sim_pu_points,
#'      sim_features)
#'
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
#' # create some problems with multiple zones
#'
#' # first, create a matrix containing the targets for multi-zone problems
#' # here each row corresponds to a different feature, each
#' # column corresponds to a different zone, and values correspond
#' # to the total (absolute) amount of a given feature that needs to be secured
#' # in a given zone
#' targets <- matrix(rpois(15, 1),
#'                   nrow = number_of_features(sim_features_zones),
#'                   ncol = number_of_zones(sim_features_zones),
#'                   dimnames = list(feature_names(sim_features_zones),
#'                                   zone_names(sim_features_zones)))
#'
#' # print targets
#' print(targets)
#'
#' # create a multi-zone problem with raster data
#' p6 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(targets) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s6 <- solve(p6)
#'
#' # plot solution
#' # here, each layer/panel corresponds to a different zone and pixel values
#' # indicate if a given planning unit has been allocated to a given zone
#' par(mfrow = c(1, 1))
#' plot(s6, main = c("zone 1", "zone 2", "zone 3"), axes = FALSE, box = FALSE)
#'
#' # alternatively, the category_layer function can be used to create
#' # a new raster object containing the zone ids for each planning unit
#' # in the solution (note this only works for problems with binary decisions)
#' par(mfrow = c(1, 1))
#' plot(category_layer(s6), axes = FALSE, box = FALSE)
#' }
#' # create a multi-zone problem with polygon data
#' p7 <- problem(sim_pu_zones_polygons, sim_features_zones,
#'               cost_column = c("cost_1", "cost_2", "cost_3")) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(targets) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s7 <- solve(p7)
#'
#' # create column containing the zone id for which each planning unit was
#' # allocated to in the solution
#' s7$solution <- category_vector(s7@data[, c("solution_1_zone_1",
#'                                            "solution_1_zone_2",
#'                                            "solution_1_zone_3")])
#' s7$solution <- factor(s7$solution)
#'
#' # plot solution
#' spplot(s7, zcol = "solution", main = "solution", axes = FALSE, box = FALSE)
#' }
#' # create a multi-zone problem with polygon planning unit data
#' # and where fields (columns) in the attribute table correspond
#' # to feature abundances
#'
#' # first fields need to be added to the planning unit data
#' # which indicate the amount of each feature in each zone
#' # to do this, the fields will be populated with random counts
#' sim_pu_zones_polygons$spp1_z1 <- rpois(nrow(sim_pu_zones_polygons), 1)
#' sim_pu_zones_polygons$spp2_z1 <- rpois(nrow(sim_pu_zones_polygons), 1)
#' sim_pu_zones_polygons$spp3_z1 <- rpois(nrow(sim_pu_zones_polygons), 1)
#' sim_pu_zones_polygons$spp1_z2 <- rpois(nrow(sim_pu_zones_polygons), 1)
#' sim_pu_zones_polygons$spp2_z2 <- rpois(nrow(sim_pu_zones_polygons), 1)
#' sim_pu_zones_polygons$spp3_z2 <- rpois(nrow(sim_pu_zones_polygons), 1)
#'
#' # create problem with polygon planning unit data and use field names
#' # to indicate feature data
#' # additionally, to make this example slightly more interesting,
#' # the problem will have proportion-type decisions such that
#' # a proportion of each planning unit can be allocated to each of the
#' # two management zones
#' p8 <- problem(sim_pu_zones_polygons,
#'               zones(c("spp1_z1", "spp2_z1", "spp3_z1"),
#'                     c("spp1_z2", "spp2_z2", "spp3_z2"),
#"                     feature_names = c("spp1", "spp2", "spp3"),
#'                     zone_names = c("z1", "z2")),
#'               cost_column = c("cost_1", "cost_2")) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(targets[1:3, 1:2]) %>%
#'       add_proportion_decisions()
#' \donttest{
#' # solve problem
#' s8 <- solve(p8)
#'
#' # plot solution
#' spplot(s8, zcol = c("solution_1_z1", "solution_1_z2"), main = "solution",
#'        axes = FALSE, box = FALSE)
#' }
#' @export
methods::setGeneric("problem",
                    signature = methods::signature("x", "features"),
                    function(x, features, ...) standardGeneric("problem"))

#' @name problem
#' @usage \S4method{problem}{Raster,Raster}(x, features, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Raster", features = "Raster"),
  function(x, features, run_checks = TRUE, ...) {
    assertthat::assert_that(inherits(x, "Raster"), raster::nlayers(x) == 1,
                            no_extra_arguments(...))
    problem(x, zones(features, zone_names = names(x),
                     feature_names = names(features)),
            run_checks = run_checks, ...)
})

#' @name problem
#' @usage \S4method{problem}{Raster,ZonesRaster}(x, features, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Raster", features = "ZonesRaster"),
  function(x, features, run_checks = TRUE, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "Raster"),
      inherits(features, "ZonesRaster"),
      assertthat::is.flag(run_checks),
      no_extra_arguments(...),
      raster::nlayers(x) > 0,
      number_of_features(features) > 0,
      raster::nlayers(x) == number_of_zones(features),
      raster::compareRaster(x, features[[1]], res = TRUE, tolerance = 1e-5,
                            stopiffalse = FALSE))
    if (run_checks)
      assertthat::assert_that(
        all(raster::cellStats(!is.na(x), "sum") > 0),
        all(raster::cellStats(x, "min") >= 0),
        all(raster::cellStats(x, "max") >= 0),
        all(raster::cellStats(raster::stack(as.list(features)), "min") >= 0))
    # convert x to RasterLayer if has only one layer
    if (inherits(x, c("RasterStack", "RasterBrick")) &&
        raster::nlayers(x) == 1)
      x <- x[[1]]
    # create rij matrix
    rij <- lapply(as.list(features), function(f) rij_matrix(x, f))
    names(rij) <- zone_names(features)
    # calculate feature abundances in total units
    fatu <- vapply(features, raster::cellStats,
                   numeric(number_of_features(features)), "sum")
    if (!is.matrix(fatu))
      fatu <- matrix(fatu, ncol = number_of_zones(features),
                     nrow = number_of_features(features))
    colnames(fatu) <- zone_names(features)
    rownames(fatu) <- feature_names(features)
    # create ConservationProblem object
    pproto(NULL, ConservationProblem,
           constraints = pproto(NULL, Collection),
           penalties = pproto(NULL, Collection),
           data = list(cost = x, features = features, rij_matrix = rij,
                       feature_abundances_in_total_units = fatu))
})

#' @name problem
#' @usage \S4method{problem}{Spatial,Raster}(x, features, cost_column, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "Raster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    assertthat::assert_that(assertthat::is.string(cost_column))
    problem(x, zones(features, zone_names = cost_column,
                     feature_names = names(features)),
            cost_column = cost_column, run_checks = run_checks, ...)
})

#' @name problem
#' @usage \S4method{problem}{Spatial,ZonesRaster}(x, features, cost_column, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "ZonesRaster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, c("SpatialPolygonsDataFrame", "SpatialLinesDataFrame",
                    "SpatialPointsDataFrame")),
      no_extra_arguments(...),
      length(x) > 0, is.character(cost_column), !anyNA(cost_column),
      all(cost_column %in% names(x)),
      length(cost_column) == number_of_zones(features),
      all(vapply(x@data[, cost_column, drop = FALSE], is.numeric, logical(1))),
      assertthat::is.flag(run_checks))
    # further validation checks
    assertthat::assert_that(
      length(x) > 0,
      all(colSums(!is.na(as.matrix(x@data[, cost_column, drop = FALSE])),
                  na.rm = TRUE) > 0),
      all(colSums(as.matrix(x@data[, cost_column, drop = FALSE]) < 0,
                  na.rm = TRUE) == 0),
      raster::compareCRS(x@proj4string, features[[1]]@crs),
      isTRUE(rgeos::gIntersects(methods::as(raster::extent(x),
                                            "SpatialPolygons"),
                                methods::as(raster::extent(features[[1]]),
                                            "SpatialPolygons"))))
    if (run_checks)
      assertthat::assert_that(
        all(raster::cellStats(raster::stack(as.list(features)), "min") >= 0))
    # compute rij matrix including non-planning unit cells
    rij <- rij_matrix(x, raster::stack(as.list(features)))
    rij <- lapply(seq_len(number_of_zones(features)), function(i) {
      m <- rij[((i - 1) * number_of_features(features)) +
               seq_len(number_of_features(features)), ,
          drop = FALSE]
      rownames(m) <- feature_names(features)
      return(m)
    })
    # calculate feature abundances in total units
    fatu <- vapply(rij, rowSums, numeric(number_of_features(features)),
                   na.rm = TRUE)
    if (!is.matrix(fatu))
      fatu <- matrix(fatu, nrow = number_of_features(features),
                     ncol = number_of_zones(features))
    rownames(fatu) <- feature_names(features)
    colnames(fatu) <- zone_names(features)
    # create rij matrix
    pos <- which(rowSums(!is.na(as.matrix(
             x@data[, cost_column, drop = FALSE]))) > 0)
    rij <- lapply(rij, function(x) x[, pos, drop = FALSE])
    names(rij) <- zone_names(features)
    # create ConservationProblem object
    pproto(NULL, ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(cost = x, features = features, cost_column = cost_column,
                  rij_matrix = rij, feature_abundances_in_total_units = fatu))
})

#' @name problem
#' @usage \S4method{problem}{Spatial,character}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "character"),
  function(x, features, cost_column, ...) {
    assertthat::assert_that(assertthat::is.string(cost_column))
    problem(x, zones(features, feature_names = features,
                     zone_names = cost_column),
           cost_column = cost_column, ...)
})

#' @name problem
#' @usage \S4method{problem}{Spatial,ZonesCharacter}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "Spatial", features = "ZonesCharacter"),
  function(x, features, cost_column, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, c("SpatialPolygonsDataFrame", "SpatialLinesDataFrame",
                    "SpatialPointsDataFrame")),
      inherits(features, "ZonesCharacter"),
      no_extra_arguments(...),
      length(x) > 0, is.character(cost_column), !anyNA(cost_column),
      all(cost_column %in% names(x)),
      number_of_zones(features) == length(cost_column),
      all(unlist(as.list(features), recursive = TRUE, use.names = FALSE) %in%
                 names(x)),
      all(vapply(x@data[, cost_column, drop = FALSE], is.numeric, logical(1))),
      all(colSums(!is.na(as.matrix(x@data[, cost_column, drop = FALSE])),
                  na.rm = TRUE) > 0),
      all(colSums(as.matrix(x@data[, cost_column, drop = FALSE]) < 0,
                  na.rm = TRUE) == 0))
    # create rij matrix
    pos <- which(rowSums(!is.na(as.matrix(
             x@data[, cost_column, drop = FALSE]))) > 0)
    rij <- lapply(features, function(z) {
      r <- t(as.matrix(x@data[pos, z, drop = FALSE]))
      r[is.na(r)] <- 0
      rownames(r) <- feature_names(features)
      methods::as(r, "sparseMatrix")
    })
    names(rij) <- zone_names(features)
    # calculate feature abundances in total units
    fatu <- colSums(x@data[, unlist(as.list(features)), drop = FALSE],
                    na.rm = TRUE)
    fatu <- matrix(fatu, ncol = number_of_zones(features),
                   nrow = number_of_features(features),
                   dimnames = list(feature_names(features),
                                   zone_names(features)))
    # create ConservationProblem object
    pproto(NULL, ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(cost = x, features = features, cost_column = cost_column,
                  rij_matrix = rij, feature_abundances_in_total_units = fatu))
})

#' @name problem
#' @usage \S4method{problem}{data.frame,character}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "data.frame", features = "character"),
  function(x, features, cost_column, ...) {
    assertthat::assert_that(assertthat::is.string(cost_column))
    problem(x, zones(features, zone_names = cost_column,
                     feature_names = features),
            cost_column = cost_column, ...)
})

#' @name problem
#' @usage \S4method{problem}{data.frame,ZonesCharacter}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "data.frame", features = "ZonesCharacter"),
  function(x, features, cost_column, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "data.frame"),
      inherits(features, "ZonesCharacter"),
      no_extra_arguments(...),
      nrow(x) > 0, is.character(cost_column), !anyNA(cost_column),
      all(cost_column %in% names(x)),
      number_of_zones(features) == length(cost_column),
      all(unlist(as.list(features), recursive = TRUE, use.names = FALSE) %in%
                 names(x)),
      all(vapply(x[, cost_column, drop = FALSE], is.numeric, logical(1))),
      all(colSums(!is.na(as.matrix(x[, cost_column, drop = FALSE])),
                  na.rm = TRUE) > 0),
      all(colSums(as.matrix(x[, cost_column, drop = FALSE]) < 0,
                  na.rm = TRUE) == 0))
    # create rij matrix
    pos <- which(rowSums(!is.na(as.matrix(x[, cost_column, drop = FALSE]))) > 0)
    rij <- lapply(as.list(features), function(z) {
      r <- t(as.matrix(x[pos, z, drop = FALSE]))
      r[is.na(r)] <- 0
      rownames(r) <- feature_names(features)
      methods::as(r, "sparseMatrix")
    })
    names(rij) <- zone_names(features)
    # calculate feature abundances in total units
    fatu <- colSums(x[, unlist(as.list(features)), drop = FALSE],
                    na.rm = TRUE)
    fatu <- matrix(fatu, ncol = number_of_zones(features),
                   nrow = number_of_features(features),
                   dimnames = list(feature_names(features),
                                   zone_names(features)))
    # create ConservationProblem object
    pproto(NULL, ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(cost = x, features = features, cost_column = cost_column,
                  rij_matrix = rij, feature_abundances_in_total_units = fatu))
})

#' @name problem
#' @usage \S4method{problem}{data.frame,data.frame}(x, features, rij, cost_column, zones, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "data.frame", features = "data.frame"),
  function(x, features, rij, cost_column, zones = NULL, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(x, "data.frame"), inherits(features, "data.frame"),
      is.character(cost_column), !anyNA(cost_column),
      inherits(rij, "data.frame"),
      nrow(x) > 0, nrow(features) > 0, nrow(rij) > 0,
      no_extra_arguments(...),
      # x
      assertthat::has_name(x, "id"), is.numeric(x$id), all(is.finite(x$id)),
      anyDuplicated(x$id) == 0, all(cost_column %in% names(x)),
      all(vapply(x[, cost_column, drop = FALSE], is.numeric, logical(1))),
      all(colSums(!is.na(as.matrix(x[, cost_column, drop = FALSE])),
                  na.rm = TRUE) > 0),
      all(colSums(as.matrix(x[, cost_column, drop = FALSE]) < 0,
                  na.rm = TRUE) == 0),
      # features
      assertthat::has_name(features, "id"),
      assertthat::has_name(features, "name"),
      anyDuplicated(features$id) == 0,
      anyDuplicated(features$name) == 0,
      !anyNA(features$id), !anyNA(features$name),
      is.numeric(features$id),
      is.character(features$name) || is.factor(features$name),
      # rij
      assertthat::has_name(rij, "pu"),
      assertthat::has_name(rij, "species"),
      assertthat::has_name(rij, "amount"),
      !anyNA(rij$pu), !anyNA(rij$species), !anyNA(rij$amount),
      is.numeric(rij$pu), is.numeric(rij$species), is.numeric(rij$amount),
      all(rij$pu %in% x$id),
      all(rij$species %in% features$id))
    # validate zone data
    if (!"zone" %in% names(rij))
      rij$zone <- 1
    if (length(unique(rij$zone)) > 1 && is.null(zones))
      stop("argument to zone must be specified for problems with multiple ",
           "zones")
    if (is.null(zones))
      zones <- data.frame(id = 1, name = cost_column)
    assertthat::assert_that(
      is.numeric(rij$zone),
      is.numeric(zones$id), is.character(zones$name) || is.factor(zones$name),
      !anyNA(rij$zone), !anyNA(zones$id), !anyNA(zones$name),
      anyDuplicated(zones$id) == 0, anyDuplicated(zones$name) == 0,
      nrow(zones) > 0, all(rij$zone %in% zones$id),
      nrow(zones) == length(cost_column))
    # standardize zone and feature ids
    rij$species <- match(rij$species, features$id)
    rij$zone <- match(rij$zone, zones$id)
    # calculate feature abundances in total units
    fatu <- Matrix::sparseMatrix(x = rij$amount, i = rij$species, j = rij$zone,
                                 use.last.ij = FALSE,
                                 dims = c(nrow(features), nrow(zones)),
                                 dimnames = list(as.character(features$name),
                                                 as.character(zones$name)))
    fatu <- as.matrix(fatu)
    # standardize planning unit ids
    pos <- which(rowSums(!is.na(as.matrix(x[, cost_column, drop = FALSE]))) > 0)
    rij$pu <- match(rij$pu, x$id[pos])
    rij <- rij[!is.na(rij$pu), ]
    # create rij matrix
    rij <- lapply(seq_along(zones$id), function(z) {
      r <- rij[rij$zone == z, ]
      Matrix::sparseMatrix(i = r$species, j = r$pu,
                           x = r$amount, giveCsparse = TRUE,
                           index1 = TRUE, use.last.ij = FALSE,
                           dims = c(nrow(features), length(pos)),
                           dimnames = list(features$name, NULL))
    })
    names(rij) <- as.character(zones$name)
    # create ConservationProblem object
    pproto(NULL, ConservationProblem,
      constraints = pproto(NULL, Collection),
      penalties = pproto(NULL, Collection),
      data = list(cost = x, features = features, cost_column = cost_column,
                  rij_matrix = rij, feature_abundances_in_total_units = fatu))
})

#' @name problem
#' @usage \S4method{problem}{numeric,data.frame}(x, features, rij_matrix, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "numeric", features = "data.frame"),
  function(x, features, rij_matrix, ...) {
    if (!is.list(rij_matrix))
      rij_matrix <- list("1" = rij_matrix)
    problem(matrix(x, ncol = 1), features, rij_matrix = rij_matrix)
})

#' @name problem
#' @usage \S4method{problem}{matrix,data.frame}(x, features, rij_matrix, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "matrix", features = "data.frame"),
  function(x, features, rij_matrix, ...) {
    # assert that arguments are valid
    if (!inherits(rij_matrix, "list"))
      rij_matrix <- list(rij_matrix)
    assertthat::assert_that(
      inherits(x, "matrix"), inherits(features, "data.frame"),
      inherits(rij_matrix, "list"),
      nrow(x) > 0, ncol(x) > 0, nrow(features) > 0, length(rij_matrix) > 0,
      no_extra_arguments(...),
      # x
      all(colSums(is.finite(x)) > 0),
      all(x > 0, na.rm = TRUE),
      all(colSums(!is.na(x)) > 0), all(colSums(x < 0, na.rm = TRUE) == 0),
      # features
      assertthat::has_name(features, "id"),
      assertthat::has_name(features, "name"),
      anyDuplicated(features$id) == 0,
      anyDuplicated(features$name) == 0,
      !anyNA(features$id), !anyNA(features$name),
      is.numeric(features$id),
      is.character(features$name) || is.factor(features$name),
      # rij_matrix
      all(vapply(rij_matrix, inherits, logical(1), c("matrix", "dgCMatrix"))),
      all(vapply(rij_matrix, sum, numeric(1), na.rm = TRUE) > 0),
      # multiple arguments
      ncol(x) == length(rij_matrix),
      all(vapply(rij_matrix, ncol, numeric(1)) == nrow(x)),
      all(vapply(rij_matrix, nrow, numeric(1)) == nrow(features)))
    # add names to rij_matrix if missing
    if (is.null(names(rij_matrix)))
      names(rij_matrix) <- as.character(seq_along(rij_matrix))
    # calculate feature abundances in total units
    fatu <- vapply(rij_matrix, rowSums, numeric(nrow(rij_matrix[[1]])),
                   na.rm = TRUE)
    if (!is.matrix(fatu))
      fatu <- matrix(fatu, nrow = nrow(features), ncol = length(rij_matrix))
    rownames(fatu) <- as.character(features$name)
    colnames(fatu) <- names(rij_matrix)
    # convert rij matrices to sparse format if needed
    pos <- which(rowSums(!is.na(x)) > 0)
    rij <- lapply(rij_matrix, function(z) {
      if (!inherits(z, "dgCMatrix")) {
        z[which(is.na(z))] <- 0
        rownames(z) <- as.character(features$name)
        z <- methods::as(z[, pos, drop = FALSE], "dgCMatrix")
      }
      return(z)
    })
    names(rij) <- names(rij_matrix)
    # create new problem object
    pproto(NULL, ConservationProblem,
           constraints = pproto(NULL, Collection),
           penalties = pproto(NULL, Collection),
           data = list(cost = x, features = features, rij_matrix = rij,
                       feature_abundances_in_total_units = fatu))
})
