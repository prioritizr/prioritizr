#' @include assertions.R
NULL

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
          "Use {.fun terra::project} to reproject data.",
          "Use {.fun raster::projectRaster} to reproject data."
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
          "Use {.fun terra::project} to reproject data.",
          "Use {.fun raster::projectRaster} to reproject data."
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

#' Assert that an object is a valid first argument for a method function.
#'
#' Throw an error for a function to indicate that it should not be
#' used with [problem()] directly.
#'
#' @param x Object.
#'
#' @param call Caller environment.
#'
#' @return An invisible `TRUE` indicating success.
#'
#' @noRd
assert_valid_method_arg <- function(x, call = fn_caller_env()) {
  # assemble error message
  m <- c(
    "!" = "This function can't add targets to a {.fun problem}.",
    "v" = "Use it with {.fun add_auto_targets} or {.fun add_group_targets}."
  )
  # run assertion
  assert(!is_conservation_problem(x), msg = m, call = call)
  # return success
  invisible(TRUE)
}
