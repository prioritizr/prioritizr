#' @include internal.R Constraint-class.R intersecting_units.R
NULL

#' Add locked out constraints
#'
#' Add constraints to a conservation planning problem to ensure
#' that specific planning units are not selected
#' (or allocated to a specific zone) in the solution. For example, it may be
#' useful to lock out planning units that have been degraded and are not
#' suitable for conserving species. If specific planning units should be locked
#' in to the solution, use [add_locked_in_constraints()]. For
#' problems with non-binary planning unit allocations (e.g., proportions), the
#' [add_manual_locked_constraints()] function can be used to lock
#' planning unit allocations to a specific value.
#'
#' @usage add_locked_out_constraints(x, locked_out)
#'
#' @param x [problem()] object.
#'
#' @param locked_out Object that determines which planning units that should be
#'   locked out. See the Data format section for more information.
#'
#' @inherit add_contiguity_constraints return
#' @inherit add_locked_in_constraints details
#' @inheritSection add_locked_in_constraints Data format
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
#' sim_locked_out_raster <- get_sim_locked_out_raster()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create minimal problem
#' p1 <-
#'   problem(sim_pu_polygons, sim_features, "cost") %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with added locked out constraints using integers
#' p2 <- p1 %>% add_locked_out_constraints(which(sim_pu_polygons$locked_out))
#'
#' # create problem with added locked out constraints using a column name
#' p3 <- p1 %>% add_locked_out_constraints("locked_out")
#'
#' # create problem with added locked out constraints using raster data
#' p4 <- p1 %>% add_locked_out_constraints(sim_locked_out_raster)
#'
#' # create problem with added locked out constraints using spatial polygon data
#' locked_out <- sim_pu_polygons[sim_pu_polygons$locked_out == 1, ]
#' p5 <- p1 %>% add_locked_out_constraints(locked_out)
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
#'     "none locked out", "locked out (integer input)",
#'     "locked out (character input)", "locked out (raster input)",
#'     "locked out (polygon input)"
#'   )
#' )
#'
#' # reset plot
#' par(mfrow = c(1, 1))
#'
#' # create minimal multi-zone problem with spatial data
#' p7 <-
#'   problem(
#'     sim_zones_pu_polygons, sim_zones_features,
#'     cost_column = c("cost_1", "cost_2", "cost_3")
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets(matrix(rpois(15, 1), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create multi-zone problem with locked out constraints using matrix data
#' locked_matrix <- as.matrix(sf::st_drop_geometry(
#'   sim_zones_pu_polygons[, c("locked_1", "locked_2", "locked_3")]
#' ))
#'
#' p8 <- p7 %>% add_locked_out_constraints(locked_matrix)
#'
#' # solve problem
#' s8 <- solve(p8)
#'
#' # create new column representing the zone id that each planning unit
#' # was allocated to in the solution
#' s8$solution <- category_vector(sf::st_drop_geometry(
#'   s8[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' ))
#' s8$solution <- factor(s8$solution)
#'
#' # plot solution
#' plot(s8[, "solution"], main = "solution", axes = FALSE)
#'
#' # create multi-zone problem with locked out constraints using column names
#' p9 <-
#'   p7 %>%
#'   add_locked_out_constraints(c("locked_1", "locked_2", "locked_3"))
#'
#' # solve problem
#' s9 <- solve(p9)
#'
#' # create new column in s8 representing the zone id that each planning unit
#' # was allocated to in the solution
#' s9$solution <- category_vector(sf::st_drop_geometry(
#'   s9[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
#' ))
#' s9$solution[s9$solution == 1 & s9$solution_1_zone_1 == 0] <- 0
#' s9$solution <- factor(s9$solution)
#'
#' # plot solution
#' plot(s9[, "solution"], main = "solution", axes = FALSE)
#'
#' # create multi-zone problem with raster planning units
#' p10 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets(matrix(rpois(15, 1), nrow = 5, ncol = 3)) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create multi-layer raster with locked out units
#' locked_out_raster <- sim_zones_pu_raster[[1]]
#' locked_out_raster[!is.na(locked_out_raster)] <- 0
#' locked_out_raster <- locked_out_raster[[c(1, 1, 1)]]
#' names(locked_out_raster) <- c("zones_1", "zones_2", "zones_3")
#' locked_out_raster[[1]][1] <- 1
#' locked_out_raster[[2]][2] <- 1
#' locked_out_raster[[3]][3] <- 1
#'
#' # plot locked out raster
#' plot(locked_out_raster)
#'
#' # add locked out raster units to problem
#' p10 <- p10 %>% add_locked_out_constraints(locked_out_raster)
#'
#' # solve problem
#' s10 <- solve(p10)
#'
#' # plot solution
#' plot(category_layer(s10), main = "solution", axes = FALSE)
#' }
#' @name add_locked_out_constraints
#'
#' @exportMethod add_locked_out_constraints
#'
#' @aliases add_locked_out_constraints,ConservationProblem,numeric-method add_locked_out_constraints,ConservationProblem,logical-method add_locked_out_constraints,ConservationProblem,matrix-method add_locked_out_constraints,ConservationProblem,character-method add_locked_out_constraints,ConservationProblem,Raster-method add_locked_out_constraints,ConservationProblem,SpatRaster-method add_locked_out_constraints,ConservationProblem,Spatial-method add_locked_out_constraints,ConservationProblem,sf-method
#'
#' @export
methods::setGeneric(
  "add_locked_out_constraints",
  signature = methods::signature("x", "locked_out"),
  function(x, locked_out) {
    assert_required(x)
    assert_required(locked_out)
    assert(
      is_conservation_problem(x),
      is_inherits(
        locked_out,
        c(
          "character", "numeric", "logical",
          "matrix", "sf", "SpatRaster", "Spatial", "Raster"
        )
      )
    )
    standardGeneric("add_locked_out_constraints")
  }
)


#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,numeric}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "numeric"),
  function(x, locked_out) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is.numeric(locked_out),
      x$number_of_zones() == 1,
      all_finite(locked_out),
      is_count_vector(locked_out),
      max(locked_out) <= number_of_total_units(x),
      min(locked_out) >= 1
    )
    # create matrix with locked out constraints
    m <- matrix(FALSE, ncol = 1, nrow = x$number_of_total_units())
    m[locked_out, 1] <- TRUE
    # add constraints
    add_locked_out_constraints(x, m)
  }
)

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,logical}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "logical"),
  function(x, locked_out) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is.logical(locked_out),
      all_finite(locked_out),
      x$number_of_zones() == 1,
      x$number_of_total_units() == length(locked_out)
    )
    # add constraints
    add_locked_out_constraints(x, matrix(locked_out, ncol = 1))
  }
)

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,matrix}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "matrix"),
  function(x, locked_out) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is.matrix(locked_out),
      is.logical(locked_out),
      all_finite(locked_out),
      x$number_of_zones() == ncol(locked_out),
      x$number_of_total_units() == nrow(locked_out),
      all(rowSums(locked_out) <= ncol(locked_out))
    )
    assert(
      sum(locked_out, na.rm = TRUE) > 0,
      msg = "{.arg locked_out} must lock out at least one planning unit."
    )
    # create data.frame with statuses
    ind <- which(locked_out, arr.ind = TRUE)
    y <- data.frame(
      pu = ind[, 1],
      zone = x$zone_names()[ind[, 2]],
      status = 0,
      stringsAsFactors = FALSE
    )
    # add constraints
    add_manual_locked_constraints(x, y)
  }
)

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,character}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "character"),
  function(x, locked_out) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is.character(locked_out),
      assertthat::noNA(locked_out),
      x$number_of_zones() == length(locked_out)
    )
    assert(
      inherits(x$data$cost, c("data.frame", "Spatial", "sf")),
      msg = paste(
        "{.arg locked_out} can only be a character vector, if the",
        "planning unit data for {.arg x} is a",
        "{.cls sf}, {.cls Spatial}, or data frame."
      )
    )
    assert(
      all_match_of(locked_out, names(x$data$cost)),
      msg = paste(
        "{.arg locked_out} must contain character values that refer to",
        "column locked_out of the planning unit data for {.arg x}."
      )
    )
    assert(
      all_columns_inherit(
        as.data.frame(x$data$cost)[, locked_out, drop = FALSE],
        "logical"
      ),
      msg = paste(
        "{.arg locked_out} must refer to columns of the",
        "planning unit data for {.arg x} that contain",
        "logical ({.code TRUE}/{.code FALSE}) values."
      )
    )

    # add constraints
    add_locked_out_constraints(
      x, as.matrix(as.data.frame(x$data$cost)[, locked_out, drop = FALSE])
    )
  }
)

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,Spatial}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "Spatial"),
  function(x, locked_out) {
    cli_warning(sp_pkg_deprecation_notice)
    add_locked_out_constraints(
      x,
      suppressWarnings(
        intersecting_units(x$data$cost, sf::st_as_sf(locked_out))
      )
    )
  }
)

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,sf}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "sf"),
  function(x, locked_out) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      inherits(locked_out, "sf"),
      x$number_of_zones() == 1
    )
    assert(
      is_same_crs(x$data$cost, locked_out),
      msg = paste(
        "{.arg locked_out} and planning units for {.arg x}",
        "must have the same coordinate reference system."
      )
    )
    assert(
      is_spatial_extents_overlap(x$data$cost, locked_out),
      msg = paste(
        "{.arg locked_out} and planning units for {.arg x}",
        "must have overlapping spatial extents."
      )
    )
    # add constraints
    add_locked_out_constraints(x, intersecting_units(x$data$cost, locked_out))
  }
)

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,Raster}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "Raster"),
  function(x, locked_out) {
    cli_warning(raster_pkg_deprecation_notice)
    add_locked_out_constraints(x, terra::rast(locked_out))
  }
)

#' @name add_locked_out_constraints
#' @usage \S4method{add_locked_out_constraints}{ConservationProblem,SpatRaster}(x, locked_out)
#' @rdname add_locked_out_constraints
methods::setMethod("add_locked_out_constraints",
  methods::signature("ConservationProblem", "SpatRaster"),
  function(x, locked_out) {
    assert(
      is_conservation_problem(x),
      inherits(locked_out, "SpatRaster"),
      is_numeric_values(locked_out),
      x$number_of_zones() == terra::nlyr(locked_out)
    )
    assert(
      is_same_crs(x$data$cost, locked_out),
      msg = paste(
        "{.arg locked_out} and planning units for {.arg x}",
        "must have the same coordinate reference system."
      )
    )
    assert(
      is_spatial_extents_overlap(x$data$cost, locked_out),
      msg = paste(
        "{.arg locked_out} and planning units for {.arg x}",
        "must have overlapping spatial extents."
      )
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
            intersecting_units(x$data$cost[[i]], locked_out[[i]]),
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
            intersecting_units(x$data$cost, locked_out[[i]]),
            TRUE
          )
        }
      )
    }
    # additional checks
    assert(
      sum(status) >= 1,
      msg = "{.arg locked_out} must lock out at least one planning unit."
    )
    # add constraints
    add_locked_out_constraints(x, status)
  }
)
