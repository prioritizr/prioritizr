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

#' Is budget length?
#'
#' Check if a value is a valid budget length for a [problem].
#'
#' @param x [problem] object.
#'
#' @param budget `numeric` vector.
#'
#' @return A `logical` value.
#'
#' @noRd
is_budget_length <- function(x, budget) {
  (length(budget) == 1) ||
  (length(budget) == number_of_zones(x))
}

assertthat::on_failure(is_budget_length) <- function(call, env) {
  p <- eval(call$x, envir = env)
  nz <- number_of_zones(p)
  budget_msg <- ifelse(
    nz == 1,
    "{.arg budget} must be a single numeric value.",
    paste(
      "{.arg budget} must have a single numeric value,",
      "or a value for each zone in {.arg x}."
    )
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
  pkg <- eval(call$x, envir = env)
  if (identical(pkg, "rcbc")) {
    code <- ": {.code remotes::install_github(\"dirkschumacher/rcbc\")}"
  } else if (identical(pkg, "cplexAPI")) {
    code <- ": {.code remotes::install_github(\"cran/cplexAPI\")}"
  } else if  (identical(pkg, "lpsymphony")) {
    code <- ": {.code remotes::install_bioc(\"lpsymphony\")}"
  } else if (identical(pkg, "gurobi")) {
    code <- c(
      "instructions in {.vignette \"gurobi_installation_guide\"}."
    )
  } else {
    code <- paste0(": {.code install.packages(\"", pkg, "\")}")
  }
  c(
    paste0("The {.pkg ", pkg, "} package must be installed."),
    "i" = paste0("Install it using", code)
  )
}

#' Is valid raw solution?
#'
#' Check if an object contains a valid raw solution.
#'
#' @param x object.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @return A `logical` value.
#'
#' @noRd
is_valid_raw_solution <- function(x) {
  !is.null(x) && !is.null(x[[1]]$x)
}

assertthat::on_failure(is_valid_raw_solution) <- function(call, env) {
  c(
    "Can't find a solution!",
    "i" = paste(
      "This is because it is impossible to meet the",
      "targets, budgets, or constraints."
    )
  )
}
