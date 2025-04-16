#' @include internal.R
NULL

#' Is a match of?
#'
#' Check if a value matches an element in a `vector.
#'
#' @param x value.
#'
#' @param choices vector.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
is_match_of <- function(x, choices) {
  assert(
    assertthat::is.scalar(x),
    is.atomic(choices),
    .internal = TRUE
  )
  isTRUE(x %in% choices)
}

assertthat::on_failure(is_match_of) <- function(call, env) {
  w <- eval(call$choices, envir = env)
  if (length(w) <= 5) {
    if (is.numeric(w)) {
      w <- paste0("{", w, "}")
    }
    w <- list_text(paste0("{.val ", w, "}"), last_sep = "or")
  } else {
    w <- paste0("one of the values in {.code ", deparse(call$choices), "}")
  }
  paste0("{.arg ", deparse(call$x), "} must be ", w, ".")
}

#' All a match of?
#'
#' Check if all elements in a vector match elements in a vector.
#'
#' @param x value.
#'
#' @param choices vector.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
all_match_of <- function(x, choices) {
  assert(
    is.atomic(x) || is.factor(x),
    is.atomic(choices),
    .internal = TRUE
  )
  all(x %in% choices)
}

assertthat::on_failure(all_match_of) <- function(call, env) {
  w <- eval(call$choices, envir = env)
  if (length(w) <= 5) {
    if (is.numeric(w)) {
      w <- paste0("{", w, "}")
    }
    w <- list_text(paste0("{.val ", w, "}"), last_sep = "or")
  } else {
    w <- paste0("one of the values in {.code ", deparse(call$choices), "}")
  }
  paste0(
    "All values in {.arg ", deparse(call$x), "} must be ", w, "."
  )
}

#' No duplicates
#'
#' Check if all elements in a vector are unique.
#'
#' @param x vector object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
no_duplicates <- function(x) {
  assert(is_inherits(x, c("numeric", "character", "factor")), .internal = TRUE)
  anyDuplicated(x) == 0L
}

assertthat::on_failure(no_duplicates) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x), "} must not contain duplicate values."
  )
}

#' Is count vector?
#'
#' Check if a vector contains count values.
#'
#' @param x object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
is_count_vector <- function(x) {
  is.numeric(x) &&
  all(x >= 1, na.rm = TRUE) &&
  all(x == round(x), na.rm = TRUE)
}

assertthat::on_failure(is_count_vector) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x),
    "} must not contain zeros, negative, or non-integer values."
  )
}

#' All is valid total unit identifier?
#'
#' Check if a value contains valid total unit identifiers.
#'
#' @param x [problem()] object.
#'
#' @param y `numeric` vector of vales.
#'
#' @return A `logical` value.
#'
#' @noRd
all_is_valid_total_unit_ids <- function(x, y) {
  assert(
    inherits(x, "ConservationProblem"),
    is.atomic(y),
    .internal = TRUE
  )
  # return FALSE if any values are NA
  if (anyNA(y)) return(FALSE)
  # if x has sequential total unit ids, then we can check
  # if the identifiers are valid using simple bounds check
  if (x$is_ids_equivalent_to_indices()) {
    return(
      (max(y) <= number_of_total_units(x)) &&
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
    invalid_ids <- y[(y > number_of_total_units(x)) | (y < 0)]
  } else {
    # otherwise, error handling if ids are equivalent to indices...
    ## find invalid ids
    invalid_ids <- unique(y[!y %in% x$total_unit_ids()])
  }
  # prepare information on invalid ids
  invalid_ids <- paste0("{", invalid_ids, "}")
  if (length(invalid_ids) > 5) {
    w <- paste0(
      paste(paste0("{.val ", invalid_ids[seq_len(4)], "}"), collapse = ", "),
      ", ", cli::symbol$ellipsis, ", and {.val ", invalid_ids[6], "}"
    )
  } else {
    w <- list_text(paste0("{.val ", invalid_ids, "}"), last_sep = "and")
  }
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
