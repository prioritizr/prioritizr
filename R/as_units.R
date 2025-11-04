#' Standardize unit to \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}
#'
#' Standardize number to
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#'
#' @param x `numeric` vector.
#'
#' @param unit `character` vector of spatial units
#' (e.g., `"km2"`, `"acres"`, `"hectares"`.
#' For convenience, a single `character` value can be specified if all
#' values in `x` are the same unit.
#'
#' @return A `numeric` vector.
#'
#' @examples
#' as_km2(5, "km2")
#' as_km2(5, "acres")
#' as_km2(c(5, 10), "ha")
#' as_km2(c(5, 10), c("ha", "acres"))
#'
#' @export
as_km2 <- function(x, unit) {
  # assert arguments are valid
  assert(
    is.numeric(x),
    is.character(unit),
    .internal = TRUE
  )
  # main calculations
  if (
    identical(length(unit), 1L) ||
    identical(length(unique(unit)), 1L)
  ) {
    out <- as.numeric(
      units::set_units(
        units::set_units(x, unit[[1]], mode = "standard"),
        "km^2"
      )
    )
  } else {
    out <- vapply(
      seq_along(x),
      FUN.VALUE = numeric(1),
      function(i) {
        as.numeric(
          units::set_units(
            units::set_units(x[[i]], unit[[i]], mode = "standard"),
            "km^2",
            mode = "standard"
          )
        )
      }
    )
  }
  # return result
  out
}

#' Standardize unit to density per \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}
#'
#' Standardize number to density per
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#'
#' @inheritParams as_km2
#'
#' @inherit as_km2 return
#'
#' @examples
#' as_per_km2(5, "km2")
#' as_per_km2(5, "acres")
#' as_per_km2(c(5, 10), "ha")
#' as_km2(c(5, 10), c("ha", "acres"))
#'
#' @export
as_per_km2 <- function(x, unit) {
  # assert arguments are valid
  assert(
    is.numeric(x),
    is.character(unit),
    .internal = TRUE
  )
  # prepare units
  unit <- paste0("1/", unit)
  # main calculations
  if (
    identical(length(unit), 1L) ||
    identical(length(unique(unit)), 1L)
  ) {
    out <- as.numeric(
      units::set_units(
        units::set_units(x, unit[[1]], mode = "standard"),
        "1/km^2",
        mode = "standard"
      )
    )
  } else {
    out <- vapply(
      seq_along(x),
      FUN.VALUE = numeric(1),
      function(i) {
        as.numeric(
          units::set_units(
            units::set_units(x[[i]], unit[[i]], mode = "standard"),
            "1/km^2",
            mode = "standard"
          )
        )
      }
    )
  }
  # return result
  out
}
