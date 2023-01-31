#' @include internal.R
NULL

#' Is like a matrix?
#'
#' Check if an object is `matrix` or `Matrix?
#'
#' @param x object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_matrix_ish <- function(x) {
  inherits(x, c("Matrix", "matrix"))
}

assertthat::on_failure(is_matrix_ish) <- function(call, env) {
  x <- eval(call$x, envir = env)
  c(
    paste0(
      "{.arg ", deparse(call$x),
      "} is not a {.cls matrix} or {.cls Matrix}."
    ),
    "x" = paste0(
      "{.arg ", deparse(call$x), "} is a {.cls ", class(x)[[1]], "}."
    )
  )
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
  inherits(x, "ConservationProblem")
}

assertthat::on_failure(is_conservation_problem) <- function(call, env) {
  c(
    paste0(
      "{.arg ", deparse(call$x),
      "} is not a {.cls ConservationProblem}."
    ),
    "i" = "see {.fun problem} to create a new conservation problem."
  )
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
  what2 <- what
  if ("numeric" %in% what2) what2 <- c(what2, "integer")
  inherits(x, what2)
}

assertthat::on_failure(is_inherits) <- function(call, env) {
  x <- eval(call$x, envir = env)
  w <- eval(call$what, envir = env)
  c(
    paste0(
      paste0("{.arg ", deparse(call$x), "}"),
      " is not a ",
      list_text(paste0("{.cls ", w, "}"), last_sep = "or", quote = FALSE),
      "."
    ),
    "x" = paste0(
      "{.arg ", deparse(call$x), "} is a {.cls ", class(x)[[1]], "}."
    )
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
  if (!is.numeric(x)) return(FALSE)
  all(x == round(x), na.rm = TRUE)
}

assertthat::on_failure(is_integer) <- function(call, env) {
  paste(
    "{.arg ", deparse(call$x), "} must contain integer values."
  )
}

#' Is spatially explicit?
#'
#' Check if an object is a spatially explicit format.
#'
#' @param x object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_spatially_explicit <- function(x) {
  inherits(x, c("Spatial", "Raster", "sf", "SpatRaster"))
}

assertthat::on_failure(is_spatially_explicit) <- function(call, env) {
  c(
    paste(
      "{.arg ", deparse(call$x), "} must be a",
      "{.cls sf} or {.cls SpatRaster}."
    ),
    "i" = "This is because spatially explicit data are needed."
  )
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

#' All list elements inherit
#'
#' Check if all elements in a list inherit from a particular class.
#'
#' @param x object.
#'
#' @param what `character` name of class.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @noRd
all_elements_inherit <- function(x, what) {
  assert(inherits(x, "list"), is.character(what), .internal = TRUE)
  all(vapply(x, inherits, logical(1), what))
}

assertthat::on_failure(all_elements_inherit) <- function(call, env) {
  w <- call$what
  paste0(
    "All elements of {.arg ", deparse(call$x),
    "} must be a {.cls ", w, "}."
  )
}
