#' @include internal.R
NULL

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
  paste0(
    "{.arg ", deparse(call$x),
    "} must be an integer value between 1 and ",
    parallel::detectCores(TRUE),
    " (i.e., number of available cores)."
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
is_installed <- function(x) {
  assertthat::is.string(x) &&
    assertthat::noNA(x) &&
    requireNamespace(x, quietly = TRUE)
}

assertthat::on_failure(is_installed) <- function(call, env) {
  pkg <- deparse(call$x)
  if (identical(pkg, "rcbc")) {
    code <- ":\f{.code remotes::install_github(\"dirkschumacher/rcbc\")}"
  } else if (identical(pkg, "cplexAPI")) {
    code <- ":\f{.code remotes::install_github(\"cran/cplexAPI\")}"
  } else if  (identical(pkg, "lpsymphony")) {
    code <- ":\f{.code remotes::install_bioc(\"lpsymphony\")}"
  } else if  (identical(pkg, "gurobi")) {
    code <- c(
      "instructions in\f{.vignette prioritizr::gurobi_installation_guide}."
    )
  } else {
    code <- paste0(":\f{.code install.packages(\"", pkg, "\")}")
  }
  paste(
    "The {.pkg ", pkg, "} package is required, install it using", code
  )
}
