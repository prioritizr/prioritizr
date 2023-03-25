#' @include internal.R ConservationProblem-class.R zones.R is_spatial_extents_overlap.R assertions.R cli.R
NULL

#' Conservation planning problem
#'
#' Create a systematic conservation planning problem. This function is used to
#' specify the basic data used in a spatial prioritization problem: the
#' spatial distribution of the planning units and their costs, as well as
#' the features (e.g., species, ecosystems) that need to be conserved. After
#' constructing this object, it can be
#' customized to meet specific goals using [objectives],
#' [targets], [constraints], and
#' [penalties]. After building the problem, the
#' [solve()] function can be used to identify solutions.
#'
#' @param x [terra::rast()], [sf::st_sf()], `data.frame`, `matrix`, or
#'   `numeric` vector specifying the planning units to use in the reserve
#'   design exercise and their corresponding cost. It may be desirable to
#'   exclude some planning units from the analysis, for example those outside
#'   the study area. To exclude planning units, set the cost for those raster
#'   cells to `NA`, or use the [add_locked_out_constraints()] function.
#'
#' @param features The feature data can be specified in a variety of ways.
#'   The specific formats that can be used depend on the cost data format (i.e.,
#'   argument to `x`) and whether the problem should have a single zone or
#'   multiple zones. If the problem should have a single zone, then the feature
#'   data can be specified following:
#'
#'   \describe{
#'
#'   \item{**`x` has [terra::rast()] or [sf::st_sf()] planning units**}{
#'     The argument to `features` can be a [terra::rast()]
#'     object showing the distribution of conservation features. Missing
#'     values (i.e., `NA` values) can be used to indicate the absence of
#'     a feature in a particular cell instead of explicitly setting these
#'     cells to zero. Note that this argument type for `features` can
#'     only be used to specify data for problems involving a single zone.}
#'
#'   \item{**`x` has [sf::st_sf()] or `data.frame` planning units**}{
#'     The argument to `features` can be a `character` vector
#'     with column names (from `x`) that correspond to the abundance or
#'     occurrence of different features in each planning unit. Note that
#'     this argument type can only be used to create problems involving a
#'     single zone.}
#'
#'   \item{**`x` has `data.frame`, `matrix`, or `numeric` vector planning
#'      units**}{
#'     The argument to `features` can be a `data.frame` object
#'     containing the names of the features. Note that if this
#'     type of argument is supplied to `features` then the argument
#'     `rij` or `rij_matrix` must also be supplied. This type of
#'     argument should follow the conventions used by *Marxan*, wherein
#'     each row corresponds to a different feature. It must also contain the
#'     following columns:
#'     \describe{
#'     \item{id}{`integer` unique identifier for each feature
#'       These identifiers are used in the argument to `rij`.}
#'     \item{name}{`character` name for each feature.}
#'     \item{prop}{`numeric` relative target for each feature
#'       (optional).}
#'     \item{amount}{`numeric` absolute target for each
#'       feature (optional).}
#'     }
#'   }
#'
#' }
#'
#'   If the problem should have multiple zones, then the feature
#'   data can be specified following:
#'
#'   \describe{
#'
#'   \item{**`x` has [terra::rast()] or [sf::st_sf()] planning units**}{
#'   The argument to `features` can be a [`ZonesRaster`][zones()]
#'   object showing the distribution of conservation features in multiple
#'   zones. As above, missing values (i.e., `NA` values) can be used to
#'   indicate the absence of a feature in a particular cell instead of
#'    explicitly setting these cells to zero.}
#'
#'   \item{**`x` has [sf::st_sf()] or `data.frame` planning units**}{
#'    The argument to `features` can be a [`ZonesCharacter`][zones()]
#'    object with column names (from `x`) that correspond to the abundance or
#'    occurrence of different features in each planning unit in different
#'    zones.}
#'
#' }
#'
#' @param cost_column `character` name or `integer` indicating the
#'   column(s) with the cost data. This argument must be supplied when the
#'   argument to `x` is a [sf::st_sf()] or
#'   `data.frame` object. This argument should contain the name of each
#'   column containing cost data for each management zone when creating
#'   problems with multiple zones. To create a problem with a single zone, then
#'   set the argument to `cost_column` as a single column name.
#'
#' @param rij `data.frame` containing information on the amount of
#'    each feature in each planning unit assuming each management zone. Similar
#'    to `data.frame` arguments for `features`, the `data.frame`
#'    objects must follow the conventions used by *Marxan*. Note that the
#'    `"zone"` column is not needed for problems involving a single
#'    management zone. Specifically, the argument should contain the following
#'    columns:
#'    \describe{
#'    \item{pu}{`integer` planning unit identifier.}
#'    \item{species}{`integer` feature identifier.}
#'    \item{zone}{`integer` zone identifier (optional for
#'      problems involving a single zone).}
#'    \item{amount}{`numeric` amount of the feature in the
#'      planning unit.}
#'    }
#'
#' @param rij_matrix `list` of `matrix` or [`dgCMatrix-class`]
#'    objects specifying the amount of each feature (rows) within each planning
#'    unit (columns) for each zone. The `list` elements denote
#'    different zones, matrix rows denote features, and matrix columns denote
#'    planning units. For convenience, the argument to
#'    `rij_matrix` can be a single `matrix` or
#'    [`dgCMatrix-class`] when specifying a problem with a
#'    single management zone. This argument is only used when the argument
#'    to `x` is a `numeric` or `matrix` object.
#'
#' @param zones `data.frame` containing information on the zones. This
#'   argument is only used when argument to `x` and `features` are
#'   both `data.frame` objects and the problem being built contains
#'   multiple zones. Following conventions used in `MarZone`, this
#'   argument should contain the following columns:
#'   columns:
#'   \describe{
#'   \item{id}{`integer` zone identifier.}
#'   \item{name}{`character` zone name.}
#'   }
#'
#' @param run_checks `logical` flag indicating whether checks should be
#'   run to ensure the integrity of the input data. These checks are run by
#'   default; however, for large datasets they may increase run time. If it is
#'   taking a prohibitively long time to create the prioritization problem,
#'   try setting `run_checks` to `FALSE`.
#'
#' @param ... not used.
#'
#' @details
#' A systematic conservation planning exercise leverages data to help inform
#' conservation decision making. To help ensure that the
#' data -- and resulting prioritizations -- are relevant to the over-arching
#' goals of the exercise, you should decide on the management action
#' (or set of actions) that need be considered in the exercise.
#' For example, these actions could include establishing protected areas,
#' selecting land for conservation easements, restoring habitat,
#' planting trees for carbon sequestration, eradicating invasive
#' species, or some combination of the previous actions.
#' If the exercise involves multiple different actions, they can
#' be incorporated by using multiple zones
#' (see the Management Zones vignette for details). After deciding
#' on the management action(s), you can compile the following data.
#'
#' First, you will need to create a set of planning units
#' (i.e., discrete spatial areas) to inform decision making.
#' Planning units are often created by subdividing a study region
#' into a set of square or hexagonal cells. They can also be created using
#' administrative boundaries (e.g., provinces), land management boundaries
#' (e.g., property boundaries derived from cadastral data), or
#' ecological boundaries (e.g., based on ecosystem classification data).
#' The size (i.e., spatial grain) of the planning units is often determined
#' based on a compromise between the scale needed to inform decision making, the
#' spatial accuracy (resolution) of available datasets, and
#' the computational resources available for generating prioritizations
#' (e.g., RAM and number of CPU cores on your computer).
#'
#' Second, you will need data to quantify the cost of implementing
#' each management action within each planning unit.
#' Critically, the cost data should reflect the management action(s)
#' considered in the exercise.
#' For example, costs are often specified using data that reflect economic
#' expenditure (e.g., land acquisition cost),
#' socioeconomic conditions (e.g., human population density),
#' opportunity costs of foregone commercial activities
#' (e.g., logging or agriculture), or
#' opportunity costs of foregone recreational activities
#' (e.g., recreational fishing).
#' In some cases -- depending on the management action(s) considered --
#' it can make sense to use a constant cost value
#' (e.g., all planning units are assigned a cost value equal to one)
#' or use a cost value based on spatial extent
#' (e.g., each planning unit is assigned a cost value based on its total area).
#' Also, in most cases, you want to avoid negative cost values.
#' This is because a negative value means that a place is *desirable*
#' for implementing a management action, and such places will almost
#' always be selected for prioritization even if they provide no benefit.
#'
#' Third, you will need data to quantify the benefits of implementing
#' management actions within planning units.
#' To achieve this, you will need to select a set of conservation features
#' that relate to the over-arching goals of the exercise.
#' For example, conservation features often include
#' species (e.g., Clouded Leopard), habitats (e.g., mangroves or
#' cloud forest), or ecosystems.
#' The benefit that each feature derives from a planning unit
#' can take a variety of forms, but is typically occupancy (i.e.,
#' presence or absence), area of occurrence within each planning unit
#' (e.g., based on species' geographic range data), or
#' a measure of habitat suitability (e.g., estimated using a statistical model).
#' After compiling these data, you have the minimal data needed to generate
#' a prioritization.
#'
#' A systematic conservation planning exercise involves prioritizing a set of
#' management actions to be implemented within certain planning units.
#' Critically, this prioritization should ideally optimize the trade-off
#' between benefits and costs.
#' To accomplish this, the \pkg{prioritizr} package uses input data
#' to formulate optimization problems (see Optimization section for details).
#' Broadly speaking, the goal of an optimization problem is to minimize
#' (or maximize) an objective function over a set of
#' decision variables, subject to a series of constraints.
#' Here, an objective function specifies the metric for evaluating
#' conservation plans. The decision variables are what we control, and usually
#' there is one binary variable for each planning unit to specify whether that
#' unit is selected or not (but other approaches are available, see
#' [decisions]). The constraints can be thought of as rules that must be
#' followed. For example, constraints can be used to ensure a prioritization
#' must stay within a certain budget. These constraints can also leverage
#' additional data to help ensure that prioritizations meet the over-arching
#' goals of the exercise. For example, to account for existing conservation
#' efforts, you could obtain data delineating the extent of existing protected
#' areas and use constraints to lock in planning units that are covered by them
#' (see [add_locked_in_constraints]).
#'
#' @section Optimization:
#' The \pkg{prioritizr} package uses exact algorithms to solve reserve design
#' problems (see [solvers] for details).
#' To achieve this, it internally formulates mathematical optimization problems
#' using mixed integer linear programming (MILP). The general form of
#' such problems can be expressed in matrix notation using
#' the following equation.
#'
#' \deqn{\mathit{Minimize} \space \mathbf{c}^{\mathbf{T}}\mathbf{x} \space
#' \mathit{subject \space to} \space
#' \mathbf{Ax}\geq= or\leq \mathbf{b}}{Minimize (c^T)*x subject to Ax \ge, =,
#' or \le b}
#'
#' Here, \eqn{x} is a vector of decision variables, \eqn{c} and \eqn{b} are
#' vectors of known coefficients, and \eqn{A} is the constraint
#' matrix. The final term specifies a series of structural
#' constraints where relational operators for the constraint can be either
#' \eqn{\ge}, \eqn{=}, or \eqn{\le} the coefficients. For example, in the
#' minimum set cover problem, \eqn{c} would be a vector of costs for each
#' planning unit, \eqn{b} a vector of targets for each conservation feature,
#' the relational operator would be \eqn{\ge} for all features, and \eqn{A}
#' would be the representation matrix with \eqn{A_{ij}=r_{ij}}{Aij = rij}, the
#' representation level of feature \eqn{i} in planning unit \eqn{j}.
#' If you wish to see exactly how a conservation planning problem is
#' formulated as mixed integer linear programming problem, you can use
#' the [write_problem()] function to save the optimization problem
#' to a plain-text file on your computer and then view it using a standard
#' text editor (e.g., Notepad).
#'
#' Please note that this function internally computes the amount of each
#' feature in each planning unit when this data is not supplied (using the
#' [rij_matrix()] function). As a consequence, it can take a while to
#' initialize large-scale conservation planning problems that involve
#' millions of planning units.
#'
#' @return A new `problem()` ([`ConservationProblem-class`]) object.
#'
#' @seealso
#' See [solve()] for details on solving a problem to generate solutions.
#' Also, see [objectives], [penalties], [targets], [constraints],
#' [decisions], [portfolios], [solvers] for information on customizing problems.
#' Additionally, see [summaries] and [importance] for information on
#' evaluating solutions.
#'
#' @aliases problem,Raster,Raster-method problem,SpatRaster,SpatRaster-method problem,Spatial,Raster-method problem,data.frame,data.frame-method problem,numeric,data.frame-method problem,data.frame,character-method problem,Spatial,character-method problem,Raster,ZonesRaster-method problem,SpatRaster,ZonesRaster-method problem,Spatial,ZonesRaster-method problem,Spatial,ZonesCharacter-method problem,data.frame,ZonesCharacter-method problem,matrix,data.frame-method problem,sf,Raster-method problem,sf,SpatRaster-method problem,SpatRaster,ZonesSpatRaster-method problem,sf,ZonesCharacter-method problem,sf,character-method problem,sf,ZonesRaster-method problem,sf,ZonesSpatRaster-method
#'
#' @exportMethod problem
#'
#' @name problem
#'
#' @examples
#' \dontrun{
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_pu_points <- get_sim_pu_points()
#' sim_pu_lines <- get_sim_pu_lines()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create problem using raster planning unit data
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem using polygon planning unit data
#' p2 <-
#'   problem(sim_pu_polygons, sim_features, "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem using line planning unit data
#' p3 <-
#'   problem(sim_pu_lines, sim_features, "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem using point planning unit data
#' p4 <-
#'   problem(sim_pu_points, sim_features, "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # since geo-processing can be slow for large spatial vector datasets
#' # (e.g., polygons, lines, points), it can be worthwhile to pre-process the
#' # planning unit data so that it contains columns indicating the amount of
#' # each feature inside each planning unit
#' # (i.e., each column corresponds to a different feature)
#'
#' # calculate the amount of each species within each planning unit
#' pre_proc_data <- rij_matrix(sim_pu_polygons, sim_features)
#'
#' # add extra columns to the polygon planning unit data
#' # to indicate the amount of each species within each planning unit
#' pre_proc_data <- as.data.frame(t(as.matrix(pre_proc_data)))
#' names(pre_proc_data) <- names(sim_features)
#' sim_pu_polygons <- cbind(sim_pu_polygons, pre_proc_data)
#'
#' # create problem using the polygon planning unit data
#' # with the pre-processed columns
#' p5 <-
#'   problem(sim_pu_polygons, features = names(pre_proc_data), "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # in addition to spatially explicit data, pre-processed aspatial data
#' # can also be used to create a problem
#' # (e.g., data created using external spreadsheet software)
#' costs <- sim_pu_polygons$cost
#' features <- data.frame(
#'   id = seq_len(terra::nlyr(sim_features)),
#'   name = names(sim_features)
#' )
#' rij_mat <- rij_matrix(sim_pu_polygons, sim_features)
#' p6 <-
#'   problem(costs, features, rij_matrix = rij_mat) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#' s4 <- solve(p4)
#' s5 <- solve(p5)
#' s6 <- solve(p6)
#'
#' # plot solutions for problems associated with spatial data
#' plot(s1, main = "raster data", axes = FALSE)
#' plot(s2[, "solution_1"], main = "polygon data")
#' plot(s3[, "solution_1"], main = "line data")
#' plot(s4[, "solution_1"], main = "point data")
#' plot(s5[, "solution_1"], main = "preprocessed data (polygon data)")
#'
#' # show solutions for problems associated with aspatial data
#' str(s6)
#'
#' # create some problems with multiple zones
#'
#' # first, create a matrix containing the targets for multi-zone problems
#' # here each row corresponds to a different feature, each
#' # column corresponds to a different zone, and values correspond
#' # to the total (absolute) amount of a given feature that needs to be secured
#' # in a given zone
#' targets <- matrix(
#'   rpois(15, 1),
#'   nrow = number_of_features(sim_zones_features),
#'   ncol = number_of_zones(sim_zones_features),
#'   dimnames = list(
#'     feature_names(sim_zones_features), zone_names(sim_zones_features)
#'   )
#' )
#'
#' # print targets
#' print(targets)
#'
#' # create a multi-zone problem with raster data
#' p7 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets(targets) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s7 <- solve(p7)
#'
#' # plot solution
#' # here, each layer/panel corresponds to a different zone and pixel values
#' # indicate if a given planning unit has been allocated to a given zone
#' par(mfrow = c(1, 1))
#' plot(s7, main = c("zone 1", "zone 2", "zone 3"), axes = FALSE)
#'
#' # alternatively, the category_layer function can be used to create
#' # a new raster object containing the zone ids for each planning unit
#' # in the solution (note this only works for problems with binary decisions)
#' par(mfrow = c(1, 1))
#' plot(category_layer(s7), axes = FALSE)
#'
#' # create a multi-zone problem with polygon data
#' p8 <-
#'   problem(
#'     sim_zones_pu_polygons, sim_zones_features,
#'     cost_column = c("cost_1", "cost_2", "cost_3")
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets(targets) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s8 <- solve(p8)
#'
#' # create column containing the zone id for which each planning unit was
#' # allocated to in the solution
#' s8$solution <- category_vector(sf::st_drop_geometry(
#'  s8[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' ))
#' s8$solution <- factor(s8$solution)
#'
#' # plot solution
#' plot(s8[, "solution"], axes = FALSE)
#'
#' # create a multi-zone problem with polygon planning unit data
#' # and where columns correspond to feature abundances
#'
#' # to begin with, we will add columns to the planning unit data
#' # that indicate the amount of each feature in each zone
#' sim_zones_pu_polygons$spp1_z1 <- rpois(nrow(sim_zones_pu_polygons), 1)
#' sim_zones_pu_polygons$spp2_z1 <- rpois(nrow(sim_zones_pu_polygons), 1)
#' sim_zones_pu_polygons$spp3_z1 <- rpois(nrow(sim_zones_pu_polygons), 1)
#' sim_zones_pu_polygons$spp1_z2 <- rpois(nrow(sim_zones_pu_polygons), 1)
#' sim_zones_pu_polygons$spp2_z2 <- rpois(nrow(sim_zones_pu_polygons), 1)
#' sim_zones_pu_polygons$spp3_z2 <- rpois(nrow(sim_zones_pu_polygons), 1)
#'
#' # create problem with polygon planning unit data and use column names
#' # to indicate feature data
#' # additionally, to make this example slightly more interesting,
#' # the problem will have proportion-type decisions such that
#' # a proportion of each planning unit can be allocated to each of the
#' # two management zones
#' p9 <-
#'   problem(
#'     sim_zones_pu_polygons,
#'     zones(
#'       c("spp1_z1", "spp2_z1", "spp3_z1"),
#'       c("spp1_z2", "spp2_z2", "spp3_z2"),
#        feature_names = c("spp1", "spp2", "spp3"),
#'       zone_names = c("z1", "z2")
#'     ),
#'     cost_column = c("cost_1", "cost_2")
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets(targets[1:3, 1:2]) %>%
#'   add_proportion_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s9 <- solve(p9)
#'
#' # plot solution
#' plot(s9[, c("solution_1_z1", "solution_1_z2")], axes = FALSE)
#' }
#' @export
methods::setGeneric(
  "problem",
  signature = methods::signature("x", "features"),
  function(x, features, ...) {
    assert_required(x)
    assert_required(features)
    assert(
      is_inherits(
        x,
        c(
          "sf", "SpatRaster", "data.frame", "numeric", "matrix", "Raster",
          "Spatial"
        )
      ),
      is_inherits(
        features,
        c(
          "character", "data.frame", "SpatRaster", "ZonesCharacter",
          "ZonesSpatRaster", "Raster", "ZonesRaster"
        )
      )
    )
    standardGeneric("problem")
  }
)

#' @name problem
#' @usage \S4method{problem}{SpatRaster,SpatRaster}(x, features, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "SpatRaster", features = "SpatRaster"),
  function(x, features, run_checks = TRUE, ...) {
    assert(
      inherits(x, "SpatRaster"),
      terra::nlyr(x) == 1,
      no_duplicates(names(features))
    )
    assert_dots_empty()
    problem(
      x,
      zones(
        features, zone_names = names(x), feature_names = names(features)
      ),
      run_checks = run_checks,
      ...
    )
})

#' @name problem
#' @usage \S4method{problem}{SpatRaster,ZonesSpatRaster}(x, features, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "SpatRaster", features = "ZonesSpatRaster"),
  function(x, features, run_checks = TRUE, ...) {
    # assert that arguments are valid
    assert(
      inherits(x, "SpatRaster"),
      inherits(features, "ZonesSpatRaster"),
      assertthat::is.flag(run_checks),
      number_of_features(features) > 0,
      terra::nlyr(x) == number_of_zones(features),
      is_comparable_raster(x, features)
    )
    assert_dots_empty()
    if (run_checks) {
      assert(any_nonNA(x))
      verify(
        all_positive(x),
        any_nonzero(x),
        all_positive(features),
        any_nonzero(features)
      )
    }
    # create rij matrix
    rij <- lapply(
      terra::as.list(features),
      function(f) `rownames<-`(rij_matrix(x, f), feature_names(features))
    )
    names(rij) <- zone_names(features)
    # calculate feature abundances in total units
    fatu <- vapply(
      features,
      function(x) terra::global(x, "sum", na.rm = TRUE)[[1]],
      numeric(number_of_features(features))
    )
    if (!is.matrix(fatu)) {
      fatu <- matrix(
        fatu,
        ncol = number_of_zones(features),
        nrow = number_of_features(features)
      )
    }
    colnames(fatu) <- zone_names(features)
    rownames(fatu) <- feature_names(features)
    # create ConservationProblem object
    conservation_problem(
      data = list(
        cost = x,
        features = features,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
      )
    )
  }
)

#' @name problem
#' @usage \S4method{problem}{data.frame,character}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "data.frame", features = "character"),
  function(x, features, cost_column, ...) {
    assert_required(cost_column)
    assert(assertthat::is.string(cost_column))
    problem(
      x,
      zones(features, zone_names = cost_column, feature_names = features),
      cost_column = cost_column,
      ...
    )
})

#' @name problem
#' @usage \S4method{problem}{data.frame,ZonesCharacter}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "data.frame", features = "ZonesCharacter"),
  function(x, features, cost_column, ...) {
    # assert that arguments are valid
    assert(
      is.data.frame(x),
      inherits(features, "ZonesCharacter"),
      nrow(x) > 0,
      is.character(cost_column),
      assertthat::noNA(cost_column),
      all_match_of(cost_column, names(x)),
      number_of_zones(features) == length(cost_column),
      all_columns_inherit(x[, cost_column, drop = FALSE], "numeric"),
      all_columns_any_finite(x[, cost_column, drop = FALSE])
    )
    assert_dots_empty()
    assert(
      all_match_of(unlist(as.list(features)), names(x)),
      msg = c(
        paste(
          "{.arg features} must contain character values that are column names",
          "for {.arg x}."
        ),
        "x" = paste(
          "The following values are not columns names of {.arg x}:",
          "{.val {setdiff(unlist(as.list(features)), names(x))}}."
        )
      )
    )
    verify(
      all_positive(x[, cost_column, drop = FALSE]),
      any_nonzero(x[, cost_column, drop = FALSE]),
      all_positive(x[, unlist(features), drop = FALSE]),
      any_nonzero(x[, unlist(features), drop = FALSE])
    )
    # create rij matrix
    pos <- which(rowSums(!is.na(as.matrix(x[, cost_column, drop = FALSE]))) > 0)
    rij <- lapply(as.list(features), function(z) {
      r <- t(as.matrix(x[pos, z, drop = FALSE]))
      r[is.na(r)] <- 0
      rownames(r) <- feature_names(features)
      colnames(r) <- NULL
      methods::as(r, "sparseMatrix")
    })
    names(rij) <- zone_names(features)
    # calculate feature abundances in total units
    fatu <- colSums(
      x[, unlist(as.list(features)), drop = FALSE],
      na.rm = TRUE
    )
    fatu <- matrix(
      fatu,
      ncol = number_of_zones(features),
      nrow = number_of_features(features),
      dimnames = list(feature_names(features), zone_names(features))
    )
    # create ConservationProblem object
    conservation_problem(
      data = list(
        cost = x,
        features = features,
        cost_column = cost_column,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
      )
    )
  }
)

#' @name problem
#' @usage \S4method{problem}{data.frame,data.frame}(x, features, rij, cost_column, zones, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "data.frame", features = "data.frame"),
  function(x, features, rij, cost_column, zones = NULL, ...) {
    # assert that arguments are valid
    assert_required(rij)
    assert_required(cost_column)
    assert(
      is.data.frame(x),
      is.data.frame(features),
      is.character(cost_column),
      assertthat::noNA(cost_column),
      is.data.frame(rij),
      nrow(x) > 0,
      nrow(features) > 0,
      nrow(rij) > 0,
      # x
      assertthat::has_name(x, "id"),
      is.numeric(x$id),
      is_count_vector(x$id),
      all_finite(x$id),
      no_duplicates(x$id),
      all_match_of(cost_column, names(x)),
      all_columns_inherit(x[, cost_column, drop = FALSE], "numeric"),
      all_columns_any_finite(x[, cost_column, drop = FALSE]),
      # features
      assertthat::has_name(features, "id"),
      assertthat::has_name(features, "name"),
      is.numeric(features$id),
      is_count_vector(features$id),
      no_duplicates(features$id),
      no_duplicates(features$name),
      all_finite(features$id),
      is_inherits(features$name, c("character", "factor")),
      assertthat::noNA(features$name),
      # rij
      assertthat::has_name(rij, "pu"),
      assertthat::has_name(rij, "species"),
      assertthat::has_name(rij, "amount"),
      is.numeric(rij$pu),
      is_count_vector(rij$pu),
      assertthat::noNA(rij$pu),
      is.numeric(rij$species),
      is_count_vector(rij$species),
      assertthat::noNA(rij$species),
      is.numeric(rij$amount),
      assertthat::noNA(rij$amount),
      all_match_of(rij$pu, x$id),
      all_match_of(rij$species, features$id)
    )
    assert_dots_empty()
    # verifications
    verify(
      all_positive(x[, cost_column]),
      any_nonzero(x[, cost_column]),
      all_positive(rij$amount),
      any_nonzero(rij$amount)
    )
    # validate zone data
    if (!"zone" %in% names(rij))
      rij$zone <- 1
    assert(
      !(length(unique(rij$zone)) > 1 && is.null(zones)),
      msg = "{.arg zone} must be specified when using multiple zones."
    )
    if (is.null(zones))
      zones <- data.frame(id = 1, name = cost_column)
    assert(
      is.numeric(rij$zone),
      is_count_vector(rij$zone),
      assertthat::noNA(rij$zone),
      is.numeric(zones$id),
      no_duplicates(zones$id),
      is_inherits(zones$name, c("character", "factor")),
      no_duplicates(zones$name),
      nrow(zones) > 0,
      all_match_of(rij$zone, zones$id),
      nrow(zones) == length(cost_column)
    )
    # standardize zone and feature ids
    rij$species <- match(rij$species, features$id)
    rij$zone <- match(rij$zone, zones$id)
    # calculate feature abundances in total units
    fatu <- Matrix::sparseMatrix(
      x = rij$amount,
      i = rij$species,
      j = rij$zone,
      use.last.ij = FALSE,
      dims = c(nrow(features), nrow(zones)),
      dimnames = list(as.character(features$name), as.character(zones$name))
    )
    fatu <- as.matrix(fatu)
    # standardize planning unit ids
    pos <- which(rowSums(!is.na(as.matrix(x[, cost_column, drop = FALSE]))) > 0)
    rij$pu <- match(rij$pu, x$id[pos])
    rij <- rij[!is.na(rij$pu), ]
    # create rij matrix
    rij <- lapply(seq_along(zones$id), function(z) {
      r <- rij[rij$zone == z, ]
      Matrix::sparseMatrix(
        i = r$species,
        j = r$pu,
        x = r$amount,
        index1 = TRUE,
        use.last.ij = FALSE,
        dims = c(nrow(features), length(pos)),
        dimnames = list(features$name, NULL)
      )
    })
    names(rij) <- as.character(zones$name)
    # create ConservationProblem object
    conservation_problem(
      data = list(
        cost = x,
        features = features,
        cost_column = cost_column,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
      )
    )
  }
)

#' @name problem
#' @usage \S4method{problem}{numeric,data.frame}(x, features, rij_matrix, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "numeric", features = "data.frame"),
  function(x, features, rij_matrix, ...) {
    assert_required(rij_matrix)
    if (!is.list(rij_matrix))
      rij_matrix <- list("1" = rij_matrix)
    problem(matrix(x, ncol = 1), features, rij_matrix = rij_matrix, ...)
  }
)

#' @name problem
#' @usage \S4method{problem}{matrix,data.frame}(x, features, rij_matrix, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "matrix", features = "data.frame"),
  function(x, features, rij_matrix, ...) {
    # assert that arguments are valid
    assert_required(rij_matrix)
    if (!inherits(rij_matrix, "list"))
      rij_matrix <- list(rij_matrix)
    assert(
      is.matrix(x),
      is.data.frame(features),
      is.list(rij_matrix),
      nrow(x) > 0,
      ncol(x) > 0,
      nrow(features) > 0,
      length(rij_matrix) > 0,
      # x
      is.numeric(x),
      all_columns_any_finite(x),
      # features
      assertthat::has_name(features, "id"),
      is_count_vector(features$id),
      is.numeric(features$id),
      all_finite(features$id),
      no_duplicates(features$id),
      assertthat::has_name(features, "name"),
      is_inherits(features$name, c("character", "factor")),
      no_duplicates(features$name),
      assertthat::noNA(features$name),
      # rij_matrix
      all_elements_inherit(rij_matrix, c("matrix", "dgCMatrix")),
      # multiple arguments
      ncol(x) == length(rij_matrix)
    )
    assert_dots_empty()
    rij_matrix_ncol <- vapply(rij_matrix, ncol, numeric(1))
    assert(
      all(rij_matrix_ncol == nrow(x)),
      msg = c(
        paste0(
          "{.arg rij_matrix} must contain matrices that have",
          "the same number of columns as the number of rows in {.arg x}."
        ),
        "x" = "{.arg rij_matrix} has {.val {rij_matrix_ncol}} columns.",
        "x" = "{.arg x} has {.val {nrow(x)}} rows."
      )
    )
    rij_matrix_nrow <- vapply(rij_matrix, nrow, numeric(1))
    assert(
      all(vapply(rij_matrix, nrow, numeric(1)) == nrow(features)),
      msg = c(
        paste0(
          "{.arg rij_matrix} must contain matrices that have",
          "the same number of rows as {.arg features}."
        ),
        "x" = "{.arg rij_matrix} has {.val {rij_matrix_nrow}} rows.",
        "x" = "{.arg x} has {.val {nrow(features)}} rows."
      )
    )
    assert(
      all(
        vapply(rij_matrix, FUN.VALUE = logical(1), function(x) {
          any(is.finite(x))
        })
      ),
      msg = paste(
        "{.arg rij_matrix} must not contain only missing or",
        "non-finite values (e.g., {.val {NaN}}, {.val {NA}}, {.val {Inf})."
      )
    )
    # verifications
    verify(
      all_positive(x),
      any_nonzero(x)
    )
    verify(
      all(vapply(rij_matrix, all_positive, logical(1))),
      msg = "{.arg rij_matrix} has negative values."
    )
    verify(
      all(vapply(rij_matrix, any_nonzero, logical(1))),
      msg = "{.arg rij_matrix} has only zero values."
    )
    # add names to rij_matrix if missing
    if (is.null(names(rij_matrix)))
      names(rij_matrix) <- as.character(seq_along(rij_matrix))
    # calculate feature abundances in total units
    fatu <- vapply(
      rij_matrix, Matrix::rowSums, numeric(nrow(rij_matrix[[1]])), na.rm = TRUE
    )
    if (!is.matrix(fatu)) {
      fatu <- matrix(fatu, nrow = nrow(features), ncol = length(rij_matrix))
    }
    rownames(fatu) <- as.character(features$name)
    colnames(fatu) <- names(rij_matrix)
    # convert rij matrices to sparse format if needed
    pos <- which(Matrix::rowSums(!is.na(x)) > 0)
    rij <- lapply(rij_matrix, function(z) {
      if (inherits(z, "dgCMatrix")) {
        z@x[which(is.na(z@x))] <- 0
      } else {
        z[which(is.na(z))] <- 0
      }
      rownames(z) <- as.character(features$name)
      Matrix::drop0(methods::as(z[, pos, drop = FALSE], "dgCMatrix"))
    })
    names(rij) <- names(rij_matrix)
    # create new problem object
    conservation_problem(
      data = list(
        cost = x,
        features = features,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
      )
    )
  }
)

#' @name problem
#' @usage \S4method{problem}{sf,SpatRaster}(x, features, cost_column, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "sf", features = "SpatRaster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    assert_required(cost_column)
    assert(assertthat::is.string(cost_column))
    problem(
      x,
      zones(
        features,
        zone_names = cost_column,
        feature_names = names(features)
      ),
      cost_column = cost_column,
      run_checks = run_checks,
      ...
    )
})

#' @name problem
#' @usage \S4method{problem}{sf,ZonesSpatRaster}(x, features, cost_column, run_checks, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "sf", features = "ZonesSpatRaster"),
  function(x, features, cost_column, run_checks = TRUE, ...) {
    # assert that arguments are valid
    assert_required(cost_column)
    assert(
      inherits(x, "sf"),
      nrow(x) > 0,
      all_match_of(cost_column, names(x)),
      all_columns_inherit(x[, cost_column], "numeric"),
      all_columns_any_finite(x[, cost_column]),
      is_same_crs(x, features),
      is_spatial_extents_overlap(x, features),
      is.character(cost_column),
      assertthat::noNA(cost_column),
      all_match_of(cost_column, names(x)),
      length(cost_column) == number_of_zones(features),
      assertthat::is.flag(run_checks),
      assertthat::noNA(run_checks)
    )
    assert_dots_empty()
    assert(
      all(!st_geometry_classes(x) %in% c("GEOMETRYCOLLECTION", "MULTIPOINT")),
      msg = paste(
        "{.arg x} must not contain {.cls GEOMETRYCOLLECTION} or",
        "{.cls MULTIPOINT} geometries."
      )
    )
    # further validation checks
    verify(
      all_positive(x[, cost_column]),
      any_nonzero(x[, cost_column])
    )
    if (run_checks) {
      verify(
        all_positive(features),
        any_nonzero(features)
      )
    }
    # compute rij matrix including non-planning unit cells
    rij <- rij_matrix(x, terra::rast(as.list(features)))
    rij <- lapply(seq_len(number_of_zones(features)), function(i) {
      idx <- ((i - 1) * number_of_features(features)) +
        seq_len(number_of_features(features))
      m <- rij[idx, , drop = FALSE]
      rownames(m) <- feature_names(features)
      m
    })
    # calculate feature abundances in total units
    fatu <- vapply(
      rij, Matrix::rowSums, numeric(number_of_features(features)), na.rm = TRUE
    )
    if (!is.matrix(fatu)) {
      fatu <- matrix(
        fatu,
        nrow = number_of_features(features),
        ncol = number_of_zones(features)
      )
    }
    rownames(fatu) <- feature_names(features)
    colnames(fatu) <- zone_names(features)
    # create rij matrix
    pos <- which(
      rowSums(
        !is.na(as.matrix(sf::st_drop_geometry(x)[, cost_column, drop = FALSE]))
      ) > 0
    )
    rij <- lapply(rij, function(x) x[, pos, drop = FALSE])
    names(rij) <- zone_names(features)
    # create ConservationProblem object
    conservation_problem(
      data = list(
        cost = x,
        features = features,
        cost_column = cost_column,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
      )
    )
  }
)

#' @name problem
#' @usage \S4method{problem}{sf,character}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "sf", features = "character"),
  function(x, features, cost_column, ...) {
    assert_required(cost_column)
    assert(assertthat::is.string(cost_column))
    problem(
      x,
      zones(features, feature_names = features, zone_names = cost_column),
      cost_column = cost_column,
      ...
    )
})

#' @name problem
#' @usage \S4method{problem}{sf,ZonesCharacter}(x, features, cost_column, ...)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(x = "sf", features = "ZonesCharacter"),
  function(x, features, cost_column, ...) {
    # assert that arguments are valid
    assert_required(cost_column)
    assert(
      inherits(x, "sf"),
      inherits(features, "ZonesCharacter"),
      nrow(x) > 0,
      is.character(cost_column),
      assertthat::noNA(cost_column),
      all_match_of(cost_column, names(x)),
      all_columns_inherit(x[, cost_column], "numeric"),
      all_columns_any_finite(x[, cost_column]),
      number_of_zones(features) == length(cost_column)
    )
    assert_dots_empty()
    assert(
      all(!st_geometry_classes(x) %in% c("GEOMETRYCOLLECTION", "MULTIPOINT")),
      msg = paste(
        "{.arg x} must not contain {.cls GEOMETRYCOLLECTION} or",
        "{.cls MULTIPOINT} geometries."
      )
    )
    assert(
      all_match_of(unlist(as.list(features)), names(x)),
      msg = c(
        paste(
          "{.arg features} must contain character values that are column names",
          "of {.arg x}."
        ),
        "x" = paste(
          "The following values are not columns names of {.arg x}:",
          "{.val {setdiff(unlist(as.list(features)), names(x))}}."
        )
      )
    )
    verify(
      all_positive(x[, cost_column]),
      any_nonzero(x[, cost_column]),
      all_positive(sf::st_drop_geometry(x)[, unlist(features)]),
      any_nonzero(sf::st_drop_geometry(x)[, unlist(features)])
    )
    # create rij matrix
    pos <- which(
      rowSums(
        !is.na(
          as.matrix(
            sf::st_drop_geometry(x)[, cost_column, drop = FALSE]
          )
        )
      ) > 0
    )
    rij <- lapply(features, function(z) {
      r <- t(as.matrix(sf::st_drop_geometry(x)[pos, z, drop = FALSE]))
      r[is.na(r)] <- 0
      rownames(r) <- feature_names(features)
      methods::as(r, "sparseMatrix")
    })
    names(rij) <- zone_names(features)
    # calculate feature abundances in total units
    fatu <- colSums(
      sf::st_drop_geometry(x)[, unlist(as.list(features)), drop = FALSE],
      na.rm = TRUE
    )
    fatu <- matrix(
      fatu,
      ncol = number_of_zones(features),
      nrow = number_of_features(features),
      dimnames = list(feature_names(features), zone_names(features))
    )
    # create ConservationProblem object
    conservation_problem(
      data = list(
        cost = x,
        features = features,
        cost_column = cost_column,
        rij_matrix = rij,
        feature_abundances_in_total_units = fatu
      )
    )
  }
)
