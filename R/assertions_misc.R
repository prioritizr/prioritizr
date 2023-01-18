#' @include internal.R
NULL

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

#' Is single patch?
#'
#' Check if all geometries form a single patch?
#'
#' @param x [sf::st_sf()] object.
#'
#' @return A `logical` value.
#'
#' @noRd
is_single_patch_sf <- function(x) {
  assertthat::assert_that(is(x, "sf"))
  m <- igraph::graph_from_adjacency_matrix(adjacency_matrix(x))
  isTRUE(igraph::components(m)$no == 1)
}
