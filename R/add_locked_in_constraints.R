#' @include internal.R Constraint-proto.R intersecting_units.R
NULL

#' Add locked in constraints
#'
#' Add constraints to a conservation planning [problem()] to ensure
#' that specific planning units are selected (or allocated
#' to a specific zone) in the solution. For example, it may be desirable to
#' lock in planning units that are inside existing protected areas so that the
#' solution fills in the gaps in the existing reserve network. If specific
#' planning units should be locked out of a solution, use
#' [add_locked_out_constraints()]. For problems with non-binary
#' planning unit allocations (e.g., proportions), the
#' [add_manual_locked_constraints()] function can be used to lock
#' planning unit allocations to a specific value.
#'
#' @usage add_locked_in_constraints(x, locked_in)
#'
#' @param x [problem()] object.
#'
#' @param locked_in Object that determines which planning units that should be
#'   locked in. See the Data format section for more information.
#'
#' @section Data format:
#'
#' The locked planning units can be specified using the following formats.
#' Generally, the locked data should correspond to the planning units
#' in the argument to `x.` To help make working with
#' [terra::rast()] planning unit data easier,
#' the locked data should correspond to cell indices in the
#' [terra::rast()] data. For example, `integer` arguments
#' should correspond to cell indices and `logical` arguments should have
#' a value for each cell---regardless of which planning unit cells contain
#' `NA` values.
#'
#' \describe{
#'
#' \item{`data` as an `integer` vector}{containing indices that indicate which
#'   planning units should be locked for the solution. This argument is only
#'   compatible with problems that contain a single zone.}
#'
#' \item{`data` as a `logical` vector}{containing `TRUE` and/or
#'   `FALSE` values that indicate which planning units should be locked
#'   in the solution. This argument is only compatible with problems that
#'   contain a single zone.}
#'
#' \item{`data` as a `matrix` object}{containing `logical` `TRUE` and/or
#'   `FALSE` values which indicate if certain planning units are
#'   should be locked to a specific zone in the solution. Each row
#'   corresponds to a planning unit, each column corresponds to a zone, and
#'   each cell indicates if the planning unit should be locked to a given
#'   zone. Thus each row should only contain at most a single `TRUE`
#'   value.}
#'
#' \item{`data` as a `character` vector}{containing field (column) name(s)
#'   that indicate if planning units should be locked for the solution.
#'   This format is only
#'   compatible if the planning units in the argument to `x` are a
#'   [sf::sf()] or `data.frame` object. The fields
#'   (columns) must have `logical` (i.e., `TRUE` or `FALSE`)
#'   values indicating if the planning unit is to be locked for the solution.
#'   For problems that contain a single zone, the argument to `data` must
#'   contain a single field name. Otherwise, for problems that
#'   contain multiple zones, the argument to `data` must
#'   contain a field name for each zone.}
#'
#' \item{`data` as a [sf::sf()] object}{
#'   containing geometries that will be used to lock planning units for
#'   the solution. Specifically, planning units in `x` that spatially
#'   intersect with `y` will be locked (per [intersecting_units()]).
#'   Note that this option is only available
#'   for problems that contain a single management zone.}
#'
#' \item{`data` as a [terra::rast()] object}{
#'   containing cells used to lock planning units for the solution.
#'   Specifically, planning units in `x`
#'   that intersect with cells that have non-zero and non-`NA` values are
#'   locked.
#'   For problems that contain multiple zones, the
#'   `data` object must contain a layer
#'   for each zone. Note that for multi-band arguments, each pixel must
#'   only contain a non-zero value in a single band. Additionally, if the
#'   cost data in `x` is a [terra::rast()] object, we
#'   recommend standardizing `NA` values in this dataset with the cost
#'   data. In other words, the pixels in `x` that have `NA` values
#'   should also have `NA` values in the locked data.}
#' }
#'
#' @inherit add_contiguity_constraints return
#'
#' @seealso
#' See [constraints] for an overview of all functions for adding constraints.
#'
#' @family constraints
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_polygons <- get_sim_pu_polygons()
#' sim_features <- get_sim_features()
#' sim_locked_in_raster <- get_sim_locked_in_raster()
#' sim_pu_zones_raster <- get_sim_pu_zones_raster()
#' sim_pu_zones_polygons <- get_sim_pu_zones_polygons()
#' sim_features_zones <- get_sim_features_zones()
#'
#' # create minimal problem
#' p1 <-
#'   problem(sim_pu_polygons, sim_features, "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with added locked in constraints using integers
#' p2 <- p1 %>% add_locked_in_constraints(which(sim_pu_polygons$locked_in))
#'
#' # create problem with added locked in constraints using a field name
#' p3 <- p1 %>% add_locked_in_constraints("locked_in")
#'
#' # create problem with added locked in constraints using raster data
#' p4 <- p1 %>% add_locked_in_constraints(sim_locked_in_raster)
#'
#' # create problem with added locked in constraints using spatial polygon data
#' locked_in <- sim_pu_polygons[sim_pu_polygons$locked_in == 1, ]
#' p5 <- p1 %>% add_locked_in_constraints(locked_in)
#'
#' # solve problems
#' s1 <- solve(p1)
#' s2 <- solve(p2)
#' s3 <- solve(p3)
#' s4 <- solve(p4)
#' s5 <- solve(p5)
#'
#' # create single object with all solutions
#' s6 <- sf::st_sf(
#'   tibble::tibble(
#'     s1 = s1$solution_1,
#'     s2 = s2$solution_1,
#'     s3 = s3$solution_1,
#'     s4 = s4$solution_1,
#'     s5 = s5$solution_1
#'   ),
#'   geometry = sf::st_geometry(s1)
#' )
#'
#' # plot solutions
#' plot(
#'   s6,
#'   main = c(
#'     "none locked in", "locked in (integer input)",
#'     "locked in (character input)", "locked in (raster input)",
#'     "locked in (polygon input)"
#'   )
#' )
#'
#' # create minimal multi-zone problem with spatial data
#' p7 <-
#'   problem(
#'     sim_pu_zones_polygons, sim_features_zones,
#'     cost_column = c("cost_1", "cost_2", "cost_3")
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets(matrix(rpois(15, 1), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create multi-zone problem with locked in constraints using matrix data
#' locked_matrix <- as.matrix(sf::st_drop_geometry(
#'   sim_pu_zones_polygons[, c("locked_1", "locked_2", "locked_3")]
#' ))
#'
#' p8 <- p7 %>% add_locked_in_constraints(locked_matrix)
#'
#' # solve problem
#' s7 <- solve(p7)
#'
#' # create new column representing the zone id that each planning unit
#' # was allocated to in the solution
#' s7$solution <- category_vector(sf::st_drop_geometry(
#'   s7[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' ))
#' s7$solution <- factor(s7$solution)
#'
#' # plot solution
#' plot(s7[ "solution"], axes = FALSE)
#'
#' # create multi-zone problem with locked in constraints using field names
#' p9 <- p7 %>% add_locked_in_constraints(c("locked_1", "locked_2", "locked_3"))
#'
#' # solve problem
#' s9 <- solve(p9)
#'
#' # create new column representing the zone id that each planning unit
#' # was allocated to in the solution
#' s9$solution <- category_vector(sf::st_drop_geometry(
#'   s9[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' ))
#' s9$solution[s9$solution == 1 & s9$solution_1_zone_1 == 0] <- 0
#' s9$solution <- factor(s9$solution)
#'
#' # plot solution
#' plot(s9[, "solution"], axes = FALSE)
#'
#' # create multi-zone problem with raster planning units
#' p10 <-
#'   problem(sim_pu_zones_raster, sim_features_zones) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets(matrix(rpois(15, 1), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create multi-layer raster with locked in units
#' locked_in_raster <- sim_pu_zones_raster[[1]]
#' locked_in_raster[!is.na(locked_in_raster)] <- 0
#' locked_in_raster <- locked_in_raster[[c(1, 1, 1)]]
#' locked_in_raster[[1]][1] <- 1
#' locked_in_raster[[2]][2] <- 1
#' locked_in_raster[[3]][3] <- 1
#'
#' # plot locked in raster
#' plot(locked_in_raster)
#'
#' # add locked in raster units to problem
#' p10 <- p10 %>% add_locked_in_constraints(locked_in_raster)
#'
#' # solve problem
#' s10 <- solve(p10)
#'
#' # plot solution
#' plot(category_layer(s10), main = "solution", axes = FALSE)
#' }
#'
#' @name add_locked_in_constraints
#'
#' @exportMethod add_locked_in_constraints
#'
#' @aliases add_locked_in_constraints,ConservationProblem,numeric-method add_locked_in_constraints,ConservationProblem,logical-method add_locked_in_constraints,ConservationProblem,matrix-method add_locked_in_constraints,ConservationProblem,character-method  add_locked_in_constraints,ConservationProblem,Raster-method add_locked_in_constraints,ConservationProblem,SpatRaster-method add_locked_in_constraints,ConservationProblem,Spatial-method add_locked_in_constraints,ConservationProblem,sf-method

#'
#' @export
methods::setGeneric("add_locked_in_constraints",
                    signature = methods::signature("x", "locked_in"),
                    function(x, locked_in)
                      standardGeneric("add_locked_in_constraints"))

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,numeric}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "numeric"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(
      is_conservation_problem(x),
      x$number_of_zones() == 1,
      is.numeric(locked_in),
      all_finite(locked_in),
      is_count_vector(locked_in),
      max(locked_in) <= number_of_total_units(x),
      min(locked_in) >= 1
    )
    # create matrix with locked in constraints
    m <- matrix(FALSE, ncol = 1, nrow = x$number_of_total_units())
    m[locked_in, 1] <- TRUE
    # add constraints
    add_locked_in_constraints(x, m)
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,logical}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "logical"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(
      is_conservation_problem(x),
      is.logical(locked_in),
      all_finite(locked_in),
      x$number_of_zones() == 1,
      x$number_of_total_units() == length(locked_in)
    )
    # add constraints
    add_locked_in_constraints(x, matrix(locked_in, ncol = 1))
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,matrix}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "matrix"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(
      is_conservation_problem(x),
      is.matrix(locked_in),
      is.logical(locked_in),
      all_finite(locked_in),
      x$number_of_zones() == ncol(locked_in),
      x$number_of_total_units() == nrow(locked_in),
      all(rowSums(locked_in) <= 1)
    )
    assertthat::assert_that(
      sum(locked_in, na.rm = TRUE) > 0,
      msg = "at least one planning unit must be locked in"
    )
    # create data.frame with statuses
    ind <- which(locked_in, arr.ind = TRUE)
    y <- data.frame(
      pu = ind[, 1],
      zone = x$zone_names()[ind[, 2]],
      status = 1,
      stringsAsFactors = FALSE
    )
    # add constraints
    add_manual_locked_constraints(x, y)
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,character}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "character"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(
      is_conservation_problem(x),
      is.character(locked_in),
      assertthat::noNA(locked_in),
      x$number_of_zones() == length(locked_in)
    )
    assertthat::assert_that(
      inherits(x$data$cost, c("data.frame", "Spatial", "sf")),
      msg = paste(
        "argument to locked_in contains character values, and the",
        "planning unit data are not in a data.frame, Spatial, or sf format"
      )
    )
    assertthat::assert_that(
      all_match_of(locked_in, names(x$data$cost)),
      msg = paste(
        "argument to locked_in contains values that are not column names",
        "in the planning unit data"
      )
    )
    assertthat::assert_that(
      all_columns_inherit(
        as.data.frame(x$data$cost)[, locked_in, drop = FALSE],
        "logical"
      ),
      msg = paste(
        "argument to locked_in refers to a column that does not",
        "have logical (TRUE / FALSE) values"
      )
    )
    # add constraints
    add_locked_in_constraints(
      x,
      as.matrix(as.data.frame(x$data$cost)[, locked_in, drop = FALSE])
    )
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,Spatial}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, locked_in) {
    .Deprecated(msg = sp_pkg_deprecation_notice)
    add_locked_in_constraints(
      x,
      intersecting_units(x$data$cost, sf::st_as_sf(locked_in))
    )
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,sf}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "sf"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(
      is_conservation_problem(x),
      inherits(locked_in, "sf"),
      x$number_of_zones() == 1
    )
    # add constraints
    add_locked_in_constraints(x, intersecting_units(x$data$cost, locked_in))
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,Raster}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "Raster"),
  function(x, locked_in) {
    # add constraints
    .Deprecated(msg = raster_pkg_deprecation_notice)
    add_locked_in_constraints(x, terra::rast(locked_in))
})

#' @name add_locked_in_constraints
#' @usage \S4method{add_locked_in_constraints}{ConservationProblem,SpatRaster}(x, locked_in)
#' @rdname add_locked_in_constraints
methods::setMethod("add_locked_in_constraints",
  methods::signature("ConservationProblem", "SpatRaster"),
  function(x, locked_in) {
    # assert valid arguments
    assertthat::assert_that(
      inherits(x, "ConservationProblem"),
      inherits(locked_in, "SpatRaster"),
      x$number_of_zones() == terra::nlyr(locked_in)
    )
    # create matrix with statuses
    if (
      inherits(x$data$cost, c("SpatRaster", "Raster")) &&
      isTRUE(x$number_of_zones() > 1)
    ) {
      status <- vapply(
        seq_len(x$number_of_zones()),
        FUN.VALUE = logical(x$number_of_total_units()),
        function(i) {
          replace(
            rep(FALSE, x$number_of_total_units()),
            intersecting_units(x$data$cost[[i]], locked_in[[i]]),
            TRUE
          )
        }
      )
    } else {
      status <- vapply(
        seq_len(x$number_of_zones()),
        FUN.VALUE = logical(x$number_of_total_units()),
        function(i) {
          replace(
            rep(FALSE, x$number_of_total_units()),
            intersecting_units(x$data$cost, locked_in[[i]]),
            TRUE
          )
        }
      )
    }
    # additional checks
    assertthat::assert_that(
      all(rowSums(status) <= 1),
      msg = "some planning units are locked in to multiple zones"
    )
    assertthat::assert_that(
      sum(status) >= 1,
      msg = "at least one planning unit must be locked in"
    )
    # add constraints
    add_locked_in_constraints(x, status)
})
