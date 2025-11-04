# ---
# repo: prioritizr/prioritizr
# file: standalone-assertions_class.R
# dependencies: [standalone-assertions_handlers.R]
# imports: [assertthat (>= 0.2.0), cli (>= 3.6.0)]
# ---

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
      "} must be a {.cls matrix} or {.cls Matrix}."
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
  !inherits(x, "pproto") &&
  inherits(x, "ConservationProblem")
}

assertthat::on_failure(is_conservation_problem) <- function(call, env) {
  x <- eval(call$x, envir = env)
  if (inherits(x, "pproto")) {
    return(
      c(
        paste0(
          "{.arg ", deparse(call$x),
          "} was created using an earlier version of the {.pkg prioritizr}",
          " package."
        ),
        "i" = "Use {.fn problem} to create a new conservation problem."
      )
    )
  } else {
    return(
      c(
        paste0(
          "{.arg ", deparse(call$x),
          "} must be a {.cls ConservationProblem}."
        ),
        "i" = "See {.fn problem} to create a new conservation problem."
      )
    )
  }
}

#' Is target setting method?
#'
#' Check if an object is a `Method` object.
#'
#' @param x object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_method <- function(x) {
  inherits(x, "TargetMethod")
}

assertthat::on_failure(is_method) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x),
    "}  must be a target setting method object."
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
  w <- cli::cli_vec(w, list("vec-last" = " or ", before = "<", after = ">"))
  w <- cli::format_inline("{.val {w}}")
  w <- gsub("\"", "", w, fixed = TRUE)
  c(
    paste0(
      paste0("{.arg ", deparse(call$x), "}"),
      " must be a ", w, "."
    ),
    "x" = paste0(
      "{.arg ", deparse(call$x), "} is a {.cls ", class(x)[[1]], "}."
    )
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
