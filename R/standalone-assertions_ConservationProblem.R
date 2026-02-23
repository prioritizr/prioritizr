# ---
# repo: prioritizr/prioritizr
# file: standalone-assertions_ConservationProblem.R
# dependencies: [standalone-assertions_handlers.R, standalone-assertions_class.R, standalone-assertions_functions.R, standalone-get_crs.R, standalone-assertions_raster.R]
# imports: [assertthat (>= 0.2.0), cli (>= 3.6.0), units (>= 0.8.7), sf (>= 1.0-12)]
# ---

#' All is valid total unit identifier?
#'
#' Check if a value contains valid total unit identifiers.
#'
#' @param x [problem()] object.
#'
#' @param y `numeric` vector of values.
#'
#' @return A `logical` value.
#'
#' @noRd
all_is_valid_total_unit_ids <- function(x, y) {
  assert(
    is_conservation_problem(x),
    is.numeric(y),
    .internal = TRUE
  )
  # return FALSE if any values are NA
  if (anyNA(y)) return(FALSE)
  # if x has sequential total unit ids, then we can check
  # if the identifiers are valid using simple bounds check
  if (x$is_ids_equivalent_to_indices()) {
    return(
      (max(y) <= x$number_of_total_units()) &&
      (min(y) >= 0)
    )
  }
  # otherwise, we actually need to compare the identifiers
  all(y %in% x$total_unit_ids())
}

assertthat::on_failure(all_is_valid_total_unit_ids) <- function(call, env) {
  # get objects
  x <- eval(call$x, envir = env)
  y <- eval(call$y, envir = env)
  # error handling if ids are equivalent to indices...
  if (x$is_ids_equivalent_to_indices()) {
    ## find invalid ids
    invalid_ids <- y[(y > x$number_of_total_units()) | (y < 0)]
  } else {
    # otherwise, error handling if ids are equivalent to indices...
    ## find invalid ids
    invalid_ids <- unique(y[!y %in% x$total_unit_ids()])
  }
  # prepare information on invalid ids
  w <- cli::cli_vec(invalid_ids, list("vec-last" = " and ", "vec-trunc" = 3))
  w <- cli::format_inline("{.val {w}}")
  # prepare information on planning unit identifier information
  pu_identifier_info <- switch(
    x$planning_unit_class(),
    data.frame = paste0(
      "For planning units in {.cls ", x$planning_unit_class(),
      "} format, identifiers must be values in the {.field id} column."
    ),
    tbl_df = paste0(
      "For planning units in {.cls ", x$planning_unit_class(),
      "} format, identifiers must be values in the {.field id} column."
    ),
    sf = paste0(
      "For planning units in {.cls ", x$planning_unit_class(),
      "} format, identifiers must be row numbers."
    ),
    SpatialPolygonsDataFrame = paste0(
      "For planning units in {.cls ", x$planning_unit_class(),
      "} format, identifiers must be row numbers."
    ),
    SpatialPointsDataFrame = paste0(
      "For planning units in {.cls ", x$planning_unit_class(),
      "} format, identifiers must be row numbers."
    ),
    SpatialLinesDataFrame = paste0(
      "For planning units in {.cls ", x$planning_unit_class(),
      "} format, identifiers must be row numbers."
    ),
    matrix = paste0(
      "For planning units in {.cls ", x$planning_unit_class(),
      "} format, identifiers must be row numbers."
    ),
    numeric = paste0(
      "For planning units in {.cls ", x$planning_unit_class(),
      "} format, identifiers must be element indices."
    ),
    SpatRaster = paste0(
      "For planning units in {.cls ", x$planning_unit_class(),
      "} format, identifiers must be cell indices."
    ),
    RasterLayer = paste0(
      "For planning units in {.cls ", x$planning_unit_class(),
      "} format, identifiers must be cell indices."
    ),
    RasterStack = paste0(
      "For planning units in {.cls ", x$planning_unit_class(),
      "} format, identifiers must be cell indices."
    )
  )
  assert(
    !is.null(pu_identifier_info),
    msg = "Couldn't recognize planning units in {.arg x}.",
    .internal = TRUE
  )
  # return result
  c(
    "!" = paste0(
      "{.arg ", deparse(call$y),
      "} must contain values that are valid identifiers."
    ),
    "x" = paste0(
      "The following values are not valid: ", w, "."
    ),
    "i" = pu_identifier_info
  )
}

#' Assert can calculate area-based targets for features
#'
#' Assert that features in a [problem()] have area-based units so that
#' area-based targets can be calculated
#'
#' @param x [problem()] object.
#'
#' @param features `integer` vector with feature indices.
#'
#' @inheritParams assert
#'
#' @return An invisible `logical` value.
#'
#' @noRd
assert_can_calculate_area_based_targets <- function(x, features,
                                                    call = fn_caller_env()) {
  assert_required(x, .internal = TRUE)
  assert_required(features, .internal = TRUE)
  # process depending on feature data
  if (inherits(x$get_data("features"), c("ZonesRaster", "ZonesSpatRaster"))) {
    ## if has raster features
    ## get units
    ft_crs <- get_crs(x$get_data("features"))
    cell_unit <- units::deparse_unit(ft_crs$ud_unit)
    ### if cell unit is degree, then throw error
    assert(
      !identical(cell_unit, units::deparse_unit(sf::st_crs(4326)$ud_unit)),
      msg = c(
        "!" = paste(
          "{.arg x} must not have features in a",
          "geodetic coordinate reference system."
        ),
        "i" = paste0(
          "This is because the target calculations involve area-based units."
        ),
        "i" = ifelse(
          inherits(x$get_data("features"), "ZonesSpatRaster"),
          "Use {.fn terra::project} to reproject data.",
          "Use {.fn raster::projectRaster} to reproject data."
        )
      ),
      call = call
    )
    ### if cell unit is NA, then throw error
    assert(
      assertthat::noNA(cell_unit),
      ft_crs != sf::st_crs(NA),
      ft_crs != sf::st_crs(na_crs),
      msg = c(
        "!" = paste(
          "{.arg x} must have features in a",
          "coordinate reference system that has defined units."
        ),
        "i" = paste0(
          "This is because the target calculations involve area-based units."
        ),
        "i" = ifelse(
          inherits(x$get_data("features"), "ZonesSpatRaster"),
          "Use {.fn terra::project} to reproject data.",
          "Use {.fn raster::projectRaster} to reproject data."
        )
      ),
      call = call
    )
  } else {
    ## if does not have raster features
    ### extract data
    fu <- x$get_data("feature_units")[features]
    n <- x$feature_names()[features]
    n <- n[is.na(fu)]
    ## assertions
    assert(
      identical(length(n), 0L),
      msg = c(
        "!" = paste(
          "{.arg x} must have defined {.arg feature units}",
          "to calculate targets for features."
        ),
        "i" = paste0(
          "This is because the target calculations involve area-based units."
        ),
        "x" = paste(
          "{.arg x} is missing units for the following features:",
          "{repr.character(n)}."
        )
      ),
      call = call
    )
  }
  # return success
  invisible(TRUE)
}

#' Is planning units spatially explicit?
#'
#' Check if a [problem()] has spatially explicit planing units?
#'
#' @param x object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
is_pu_spatially_explicit <- function(x) {
  assert(inherits(x, "ConservationProblem"), .internal = TRUE)
  inherits(x$data$cost, c("Spatial", "Raster", "sf", "SpatRaster"))
}

assertthat::on_failure(is_pu_spatially_explicit) <- function(call, env) {
  c(
    paste(
      "{.arg data} must be supplied because planning unit",
      "data for {.arg x} are not spatially explicit."
    ),
    "i" = paste(
      "To calculate {.arg data} automatically,",
      "the planning unit data for {.arg x} must be a",
      "{.cls sf} or {.cls SpatRaster}."
    )
  )
}

#' Are planning unit rasters comparable?
#'
#' This function checks if the planning units in a [problem()] object
#' is comparable with a raster.
#'
#' @param x [problem()] object.
#'
#' @param y [terra::rast()] or [raster::raster()] object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value indicating if the
#'   objects have the same
#'   resolution, extent, dimensionality, and coordinate system.
#'
#' @noRd
is_pu_comparable_raster <- function(x, y) {
  assert(is_conservation_problem(x), .internal = TRUE)
  is_comparable_raster(x$data$cost, y)
}

assertthat::on_failure(is_pu_comparable_raster) <- function(call, env) {
  c(
    paste0(
      "{.arg ", deparse(call$x), "} must have planning units that are ",
      "comparable with {.arg ", deparse(call$y),  "}."
    ),
    "x" = paste(
      "They do not have the same spatial resolution, extent,",
      "coordinate reference system, and dimensionality (rows / columns)."
    )
  )
}

#' Has single zone?
#'
#' Check if a [problem()] has a single zone.
#'
#' @param x [problem()] object.
#'
#' @return A `logical` value.
#'
#' @noRd
has_single_zone <- function(x) {
  assert(is_conservation_problem(x), .internal = TRUE)
  isTRUE(x$number_of_zones() == 1)
}

assertthat::on_failure(has_single_zone) <- function(call, env) {
  x <- eval(call$x, envir = env)
  c(
    "!" = "Can't calculate targets.",
    "x" = "This function is only compatible with single zone problems.",
    "x" = paste0(
      "{.arg ", deparse(call$x), "} has {.val {",
      x$number_of_zones(), "}} zones."
    )
  )
}

#' All conservation planning problems comparable?
#'
#' @param x set of [problem()] objects.
#'
#' @return A `logical` value.
#'
#' @noRd
all_comparable_problem <- function(...) {
  x <- list(...)
  assert(
    is.list(x),
    all_elements_inherit(x, "ConservationProblem"),
    .internal = TRUE
  )
  
  isTRUE(
    ## all problems must have same number of zones
    all(vapply(
      lapply(x, number_of_zones), identical,
      logical(1), x[[1]]$number_of_zones()
    )) &&
    ## if these are multi-zone problems, then they must have the same zone
    ## names. note that this is not so important for the single-zone
    ## problems because the zone name is automatically derived from the
    ## name of the cost layer, and so it could be more reasonable for
    ## a user to consider single-zone problems that have different
    ## zone names
    (
      isTRUE(identical(x[[1]]$number_of_zones(), 1L)) ||
      all(vapply(
        lapply(x, zone_names), identical,
        logical(1), x[[1]]$zone_names()
      ))
    ) &&
    ## all problems have same types of planning units
    all(vapply(
      lapply(x, function(z) z$planning_unit_class()), identical,
      logical(1), x[[1]]$planning_unit_class()
    )) &&
    ## all problems have the same decision types
    all(vapply(
      lapply(x, function(z) z$decisions$name), identical,
      logical(1), x[[1]]$decisions$name
    )) &&
    ## all problems have same number of total units
    all(vapply(
      lapply(x, number_of_total_units), identical,
      logical(1), x[[1]]$number_of_total_units()
    )) &&
    ## all problems have same planning unit indices (in other words,
    ## the pixel or row number indices for planning units are the
    ## same across all problems)
    all(vapply(
      lapply(x, function(z) z$planning_unit_indices()), identical,
      logical(1), x[[1]]$planning_unit_indices()
    )) &&
    ## all problems have same number of planning units
    all(vapply(
      lapply(x, number_of_planning_units), identical,
      logical(1), x[[1]]$number_of_planning_units()
    )) &&
    ## all problems have same decision type
   all(vapply(lapply(x, function(z) z$decisions$name), identical,
                 logical(1), x[[1]]$decisions$name))
  )
}

assertthat::on_failure(all_comparable_problem) <- function(call, env) {
  # get the list of problems passed to all_comparable_problem()
  x <- as.list(eval(substitute(list(...)), env)) 
  
  # replicate the same checks as the main function, individually
  checks <- list(
    number_zones = isTRUE(all(vapply(
      lapply(x, number_of_zones), identical,
      logical(1), x[[1]]$number_of_zones()
    ))),
    zones_names = isTRUE(all(vapply(
      lapply(x, zone_names), identical,
      logical(1), x[[1]]$zone_names()
    ))),
    number_pu = isTRUE(all(vapply(
      lapply(x, number_of_planning_units), identical,
      logical(1), x[[1]]$number_of_planning_units()
    ))),
    number_total = isTRUE(all(vapply(
      lapply(x, number_of_total_units), identical,
      logical(1), x[[1]]$number_of_total_units()
    ))),
    pu_class = isTRUE(all(vapply(
      lapply(x, function(z) z$planning_unit_class()), identical,
      logical(1), x[[1]]$planning_unit_class()
    ))),
    pu_indices = isTRUE(all(vapply(
      lapply(x, function(z) z$planning_unit_indices()), identical,
      logical(1), x[[1]]$planning_unit_indices()
    ))),
    decisions = isTRUE(all(vapply(lapply(x, function(z) z$decisions$name), 
                                  identical,
                                  logical(1), x[[1]]$decisions$name
                                  )))
  )
  
  # cli-style messages for each type of failure
  messages <- list(
    number_zones = c("{.arg x} has mismatched number of zones.",
                        "i" = "All problems must have the same number of zones."),
    zones_names = c("{.arg x} has mismatched zone names.",
                    "i" = "All problems must have identical zone names."),
    number_pu = c("{.arg x} has mismatched number of planning units.",
                     "i" = "All problems must have the same number of planning units."),
    number_total = c("{.arg x} has mismatched total units.",
                        "i" = "All problems must have the same total units."),
    pu_class = c("{.arg x} has mismatched planning unit classes.",
                 "i" = "All problems must have identical planning unit classes."),
    pu_indices = c("{.arg x} has mismatched planning unit indices.",
                   "i" = "All problems must have identical planning unit indices."),
    decisions = c("{.arg x} has mismatched decision types.",
                   "i" = "All problems must have identical decision types.")
  )
  
  failed <- names(checks)[!unlist(checks)]
  
  if (length(failed) > 0) {
    
    detail_msgs <- unlist(
      lapply(failed, function(f) {
        msg <- messages[[f]]
        
        c(
          "x" = msg[[1]],
          "i" = msg[["i"]]
        )
      }),
      recursive = FALSE
    )
    
    if (length(failed) == 1) {
      cli::cli_abort(detail_msgs, call = call)
    }
    
    cli::cli_abort(
      c(
        "!" = "{.arg x} is not comparable.",
        "i" = "The error is caused by one or more of the following:",
        detail_msgs
      ),
      call = call
    )
  }
}
