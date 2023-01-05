#' @include internal.R all_binary.R all_columns_any_finite.R all_columns_any_finite.R all_columns_inherit.R all_finite.R all_positive.R all_proportion.R all_rows_any_finite.R any_nonNA.R
NULL

#' Verify if assertion is met
#'
#' Verify if an assertion is met and throw a [base::warning()] if it
#' is not. This function is equivalent to [assertthat::assert_that()]
#' except that it throws warnings and not errors.
#'
#' @param x `logical` condition.
#'
#' @details
#' The function will throw warnings if any of the conditions are not met.
#'
#' @return A `logical` value.
#'
#' @noRd
verify_that <- function(..., env = parent.frame()) {
  res <- assertthat::validate_that(..., env = env)
  if (isTRUE(res))
    return(TRUE)
  warning(res, immediate. = TRUE)
  FALSE
}

#' All list elements inherit
#'
#' Check if all elements in a list inherit from a particular class.
#'
#' @param x object.
#'
#' @param what `character` name of class.
#'
#' @return A `logical` value.
#'
#' @noRd
all_elements_inherit <- function(x, what) {
  assertthat::assert_that(inherits(x, "list"), is.character(what))
  all(vapply(x, inherits, logical(1), what))
}

assertthat::on_failure(all_elements_inherit) <- function(call, env) {
  w <- call$what
  paste(deparse(call$x), "has some that elements that are not a", w)
}

#' Is a match of?
#'
#' Check if a value matches an element in a `vector.
#'
#' @param x value.
#'
#' @param choices vector.
#'
#' @return A `logical` value.
#'
#' @noRd
is_match_of <- function(x, choices) {
  assertthat::assert_that(
    assertthat::is.scalar(x),
    is.vector(choices)
  )
  isTRUE(x %in% choices)
}

assertthat::on_failure(is_match_of) <- function(call, env) {
  w <- eval(call$choices, envir = env)
  if (length(w) < 5) {
    w <- list_text(w, last_sep = "or")
  } else {
    w <- deparse(call$choices)
  }
  paste(deparse(call$x), "is not one of", w)
}

#' All a match of?
#'
#' Check if all elements in a vector match elements in a vector.
#'
#' @param x value.
#'
#' @param choices vector.
#'
#' @return A `logical` value.
#'
#' @noRd
all_match_of <- function(x, choices) {
  assertthat::assert_that(
    is.vector(x) || is.factor(x),
    is.vector(choices)
  )
  all(x %in% choices)
}

assertthat::on_failure(all_match_of) <- function(call, env) {
  w <- eval(call$choices, envir = env)
  if (length(w) < 5) {
    w <- list_text(w, last_sep = "or")
  } else {
    w <- deparse(call$choices)
  }
  paste(
    deparse(call$x),
    "contains a value is not one of",
    w
  )
}

#' No duplicates
#'
#' Check if all elements in a vector are unique.
#'
#' @param x vector object.
#'
#' @return A `logical` value.
#'
#' @noRd
no_duplicates <- function(x) {
  assertthat::assert_that(is.vector(x))
  anyDuplicated(x) == 0L
}

assertthat::on_failure(no_duplicates) <- function(call, env) {
  paste(deparse(call$x), "contains duplicate values")
}

#' Is thread count?
#'
#' Check if a value is a valid threat count or not.
#'
#' @param x `numeric` value.
#'
#' @return A `logical` value.
#'
#' @noRd
is_thread_count <- function(x) {
  assertthat::is.count(x) &&
    assertthat::noNA(x) &&
    isTRUE(x <= parallel::detectCores(TRUE))
}

assertthat::on_failure(is_thread_count) <- function(call, env) {
  paste(
    deparse(call$x),
    "must be an integer between 1 and",
    parallel::detectCores(TRUE),
    "(i.e., number of available cores)"
  )
}

#' Is package installed?
#'
#' Check if a package is installed.
#'
#' @param x `character` value.
#'
#' @return A `logical` value.
#'
#' @noRd
is_installed <- function(x, name = NULL) {
  assertthat::is.string(x) &&
    assertthat::noNA(x) &&
    requireNamespace(x, quietly = TRUE)
}

assertthat::on_failure(is_installed) <- function(call, env) {
  pkg <- deparse(call$x)
  name <- deparse(call$name)
  if (identical(pkg, "rcbc")) {
    code <- ":\nremotes::install_github(\"dirkschumacher/rcbc\")"
  } else if (identical(pkg, "cplexAPI")) {
    code <- ":\nremotes::install_github(\"cran/cplexAPI\")"
  } else if  (identical(pkg, "lpsymphony")) {
    code <- ":\nremotes::install_bioc(\"lpsymphony\")"
  } else if  (identical(pkg, "gurobi")) {
    code <- paste(
      "instructions at:\n",
      "vignette(\"gurobi_installation_guide\", package = \"prioritizr\")"
    )
  } else {
    code <- paste0(":\ninstall.packages(\"", pkg, "\")")
  }
  if (is.character(name)) {
    paste(
      name, "requires the \"", pkg, "\" package, install it using", code
    )
  } else {
    paste(
      "the \"", pkg, "\" package is not installed, install it using", code
    )
  }
}

#' Are rasters comparable?
#'
#' This function checks if two [terra::rast()] objects are comparable.
#'
#' @param x [terra::rast()] or [raster::raster()] object.
#'
#' @param y [terra::rast()] or [raster::raster()] object.
#'
#' @return A `logical` value indicating if the
#'   objects have the same
#'   resolution, extent, dimensionality, and coordinate system.
#'
#' @noRd
is_comparable_raster <- function(x, y) {
  # wrapper for Raster objects
  if (inherits(x, "Raster")) x <- terra::rast(x)
  if (inherits(y, "Raster")) y <- terra::rast(y)
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    inherits(y, "SpatRaster")
  )
  # run checks
  is_same_crs(x, y) &&
    terra::compareGeom(
      x[[1]], y[[1]],
      crs = FALSE, res = TRUE, stopOnError = FALSE
    )
}

assertthat::on_failure(is_comparable_raster) <- function(call, env) {
  paste0(
    deparse(call$x), " and ", deparse(call$y),  " are not comparable; ",
    "they have different spatial resolutions, extents, ",
    "coordinate reference systems, or dimensionality (rows / columns)"
  )
}

#' No extra arguments
#'
#' Check that no additional unused arguments have been supplied to a function
#' through the `...`.
#'
#' @param ... arguments that are not used.
#'
#' @return A `logical` value.
#'
#' @noRd
no_extra_arguments <- function(...) {
  length(list(...)) == 0
}

assertthat::on_failure(no_extra_arguments) <- function(call, env) {
  "unused arguments"
}

#' Is count vector?
#'
#' Check if a vector contains count values.
#'
#' @param x object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_count_vector <- function(x) {
  assertthat::assert_that(is.numeric(x))
  all(x >= 1, na.rm = TRUE) && all(x == round(x), na.rm = TRUE)
}

assertthat::on_failure(is_count_vector) <- function(call, env) {
  paste(
    deparse(call$x),
    "contains zeros, negative, or non-integer values"
  )
}

#' Is integer?
#'
#' Check if a vector contains integer values.
#'
#' @param x object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_integer <- function(x) {
  assertthat::assert_that(is.numeric(x))
  all(x == round(x), na.rm = TRUE)
}

assertthat::on_failure(is_count_vector) <- function(call, env) {
  paste(
    deparse(call$x),
    "contains non-integer values"
  )
}

#' Is matrix?
#'
#' Check if an object is `matrix` or `Matrix?
#'
#' @param x object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_a_matrix <- function(x) {
  inherits(x, c("Matrix", "matrix"))
}

assertthat::on_failure(is_a_matrix) <- function(call, env) {
  paste(deparse(call$x), "is not a matrix or Matrix")
}

#' Is problem?
#'
#' Check if an object is a `ConservationProblem` object.
#'
#' @param x object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_conservation_problem <- function(x) {
  inherits(x, c("ConservationProblem"))
}

assertthat::on_failure(is_conservation_problem) <- function(call, env) {
  paste(deparse(call$x), "is not a problem()")
}

#' Is inherits?
#'
#' Check if an object inherits from a set of classes.
#'
#' @param x object.
#'
#' @param what `character` name of class.
#'
#' @return A `logical` value.
#'
#' @noRd
is_inherits <- function(x, what) {
  inherits(x, what)
}

assertthat::on_failure(is_inherits) <- function(call, env) {
  w <- eval(call$what, envir = env)
  paste(
    deparse(call$x),
    "is not a",
    list_text(w, last_sep = "or", quote = FALSE)
  )
}
