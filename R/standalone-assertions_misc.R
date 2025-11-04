# ---
# repo: prioritizr/prioritizr
# file: standalone-assertions_class.R
# dependencies: standalone-assertions_handlers.R
# imports: [parallel, assertthat (>= 0.2.0), cli (>= 3.6.0), units (>= 0.8.7), prioritizr (>= 8.0.6)]
# ---

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
#' @param time_limit `numeric` time limit for generating solution.
#' Defaults to `NULL`.
#'
#' @param call Caller environment.
#'
#' @return A `logical` value.
#'
#' @return A `logical` value.
#'
#' @noRd
is_valid_raw_solution <- function(x, time_limit = NULL) {
  !is.null(x) && !is.null(x[[1]]$x)
}

assertthat::on_failure(is_valid_raw_solution) <- function(call, env) {
  # get time limit
  time_limit <- eval(call$time_limit, envir = env)
  # prepare message
  msg <- c(
    "Can't find a solution!",
    "i" = paste(
      "This is because it is impossible to meet the",
      "targets, budgets, or constraints."
    )
  )
  if (
    !is.null(time_limit) &&
    assertthat::is.number(time_limit) &&
    isTRUE(time_limit < 1e5)
  ) {
    msg <- c(
      msg,
      "i" = "It could also be because the {.arg time_limit} is too low."
    )
  }
  # return message
  msg
}

#' Any solvers installed?
#'
#' Test if any solvers are installed.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{Rsymphony}, \pkg{lpsymphony}, \pkg{gurobi}.
#'
#' @return `logical` value indicating if any solvers are installed.
#'
#' @noRd
any_solvers_installed <- function() {
  !is.null(default_solver_name())
}

assertthat::on_failure(any_solvers_installed) <- function(call, env) {
  c(
    "No optimization solvers are installed.",
    "x" = "You must install a solver to generate prioritizations.",
    "i" = "See {.topic solvers} for options."
  )
}

#' Default solver name
#'
#' This function returns the name of the default solver. If no solvers are
#' detected on the system, then a `NULL` object is returned.
#'
#' @details This function tests if any of the following packages are installed:
#'   \pkg{gurobi}, \pkg{cplexAPI}, \pkg{rcbc}, \pkg{highs},
#'   \pkg{lpsymphony}, \pkg{Rsymphony}.
#'
#' @return `character` indicating the name of the default solver.
#'
#' @noRd
default_solver_name <- function() {
  if (requireNamespace("gurobi", quietly = TRUE)) {
    return("gurobi")
  } else if (requireNamespace("cplexAPI", quietly = TRUE)) {
    return("cplexAPI")
  } else if (requireNamespace("rcbc", quietly = TRUE)) {
    return("rcbc")
  } else if (requireNamespace("highs", quietly = TRUE)) {
    return("highs")
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    return("lpsymphony")
  } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
    return("Rsymphony")
  } else {
    return(NULL)
  }
}

#' Is area unit?
#'
#' Assert that a value is a valid unit of measuring area.
#'
#' @param `character` value.
#'
#' @return A `logical` value indicating if it is a valid unit of measurement.
#'
#' @noRd
is_area_units <- function(x) {
  if (!is.character(x)) return(FALSE)
  suppressMessages(
    inherits(
      try(
        units::set_units(
          units::set_units(1, x, mode = "standard"),
          "km^2"
        ),
        silent = TRUE
      ),
      "units"
    )
  )
}

assertthat::on_failure(is_area_units) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x),
    "} must be a {.cls character} value denoting a unit for measuring area."
  )
}

#' All area units?
#'
#' Assert that a vector has valid units of measuring area.
#'
#' @param `character` value.
#'
#' @param na.rm `logical` value. Defaults to `FALSE`.
#'
#' @return A `logical` value indicating if it is a valid unit of measurement.
#'
#' @noRd
all_area_units <- function(x, na.rm = FALSE) {
  if (!is.character(x)) return(FALSE)
  if (isTRUE(na.rm)) {
    x <- x[!is.na(x)]
  }
  all(vapply(unique(x), is_area_units, logical(1)))
}

assertthat::on_failure(all_area_units) <- function(call, env) {
  paste0(
    "{.arg ", deparse(call$x),
    "} must contain {.cls character} values denoting units for measuring area."
  )
}
