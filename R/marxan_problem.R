#' @include internal.R
NULL

#' *Marxan* conservation problem
#'
#' Create a conservation planning [problem()] following the
#' mathematical formulations used in *Marxan* (detailed in Beyer
#' *et al.* 2016). Note that these problems are solved using
#' exact algorithms and not simulated annealing (i.e., the *Marxan* software).
#'
#' @param x `character` file path for a *Marxan* input file (typically
#'   called `"input.dat"`), or `data.frame` containing planning unit
#'   data (typically called `"pu.dat"`). If the argument to `x` is a
#'   `data.frame`, then each row corresponds to a different planning unit,
#'   and it must have the following columns:
#'
#'   \describe{
#'
#'   \item{id}{`integer` unique identifier for each planning unit.
#'     These identifiers are used in the argument to `puvspr`.}
#'
#'   \item{cost}{`numeric` cost of each planning unit.}
#'
#'   \item{status}{`integer` indicating if each planning unit
#'     should not be locked in the solution (0) or if it should be locked in
#'     (2) or locked out (3) of the solution. Although *Marxan* allows
#'     planning units to be selected in the initial solution (using values of
#'     1), these values have no effect here. This column is optional.}
#'
#'   }
#'
#' @param spec `data.frame` containing information on the features.
#'   The argument to `spec` must follow the conventions used by
#'   *Marxan* for the species data file (conventionally called
#'   `"spec.dat"`). Each row corresponds to a different feature and
#'   each column corresponds to different information about the features. It
#'   must contain the columns listed below. Note that the argument to
#'   `spec` must contain at least one column named `"prop"` or
#'   `"amount"`---**but not both columns with both of these
#'   names**---to specify the target for each feature.
#'
#'   \describe{
#'
#'   \item{id}{`integer` unique identifier for each feature
#'     These identifiers are used in the argument to `puvspr`.}
#'
#'   \item{name}{`character` name for each feature.}
#'
#'   \item{prop}{`numeric` relative target for each feature
#'     (optional).}'
#'
#'   \item{amount}{`numeric` absolute target for each
#'     feature (optional).}
#'
#'   }
#'
#' @param puvspr `data.frame` containing information on the amount of
#'   each feature in each planning unit. The argument to
#'   `puvspr` must follow the conventions used in the *Marxan* input
#'   data file (conventionally called `"puvspr.dat"`). It must contain the
#'   following columns:
#'
#'   \describe{
#'
#'   \item{pu}{`integer` planning unit identifier.}
#'
#'   \item{species}{`integer` feature identifier.}
#'
#'   \item{amount}{`numeric` amount of the feature in the
#'        planning unit.}
#'
#'   }
#'
#' @param bound `NULL` object indicating that no boundary data
#'   is required for the conservation planning problem, or a `data.frame`
#'   containing information on the planning units' boundaries. The argument to
#'   `bound` must follow the conventions used in the *Marxan* input
#'   data file (conventionally called `"bound.dat"`). It must contain the
#'   following columns:
#'
#'   \describe{
#'
#'   \item{id1}{`integer` planning unit identifier.}
#'
#'   \item{id2}{`integer` planning unit identifier.}
#'
#'   \item{boundary}{`numeric` length of shared boundary
#'     between the planning units identified in the previous two columns.}
#'
#'   }
#'
#' @param blm `numeric` boundary length modifier. This argument only
#'   has an effect when argument to `x` is a `data.frame`. The
#'   default argument is zero.
#'
#' @param symmetric `logical` does the boundary data (i.e., argument to
#'   `bound`) describe symmetric relationships between planning units?
#'    If the boundary data contain asymmetric connectivity data,
#'    this parameter should be set to `FALSE`.
#'    Defaults to `TRUE`.
#'
#' @param ... not used.
#'
#' @details
#' This function is provided as a convenient wrapper for solving
#' *Marxan* problems using the \pkg{prioritizr} package.
#' Please note that it requires installation of the \pkg{data.table} package
#' to import *Marxan* data files.
#'
#' @section Notes:
#' In previous versions, this function could not accommodate asymmetric
#' connectivity data. It has now been updated to handle asymmetric connectivity
#' data.
#'
#' @seealso For more information on the correct format for
#'   for *Marxan* input data, see the
#'   [official *Marxan* website](https://marxansolutions.org) and Ball
#'   *et al.* (2009).
#'
#' @return A [problem()] object.
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) *Marxan and relatives:
#' Software for spatial conservation prioritisation* in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
#' conservation planning problems with integer linear programming.
#' *Ecological Modelling*, 228: 14--22.
#'
#' @examples
#' # create Marxan problem using Marxan input file
#' # (note this example requires the data.table package to be installed)
#' \dontrun{
#' input_file <- system.file("extdata/marxan/input.dat", package = "prioritizr")
#' p1 <-
#'   marxan_problem(input_file) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' head(s1)
#'
#' # create Marxan problem using data.frames that have been loaded into R
#' # (note this example also requires the data.table package to be installed)
#' ## load in planning unit data
#' pu_path <- system.file("extdata/marxan/input/pu.dat", package = "prioritizr")
#' pu_dat <- data.table::fread(pu_path, data.table = FALSE)
#' head(pu_dat)
#'
#' ## load in feature data
#' spec_path <- system.file(
#'   "extdata/marxan/input/spec.dat", package = "prioritizr"
#' )
#' spec_dat <- data.table::fread(spec_path, data.table = FALSE)
#' head(spec_dat)
#'
#' ## load in planning unit vs feature data
#' puvspr_path <- system.file(
#'   "extdata/marxan/input/puvspr.dat", package = "prioritizr"
#' )
#' puvspr_dat <- data.table::fread(puvspr_path, data.table = FALSE)
#' head(puvspr_dat)
#'
#' ## load in the boundary data
#' bound_path <- system.file(
#'   "extdata/marxan/input/bound.dat", package = "prioritizr"
#' )
#' bound_dat <- data.table::fread(bound_path, data.table = FALSE)
#' head(bound_dat)
#'
#' # create problem without the boundary data
#' p2 <-
#'   marxan_problem(pu_dat, spec_dat, puvspr_dat) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # print solution
#' head(s2)
#'
#' # create problem with the boundary data and a boundary length modifier
#' # set to 5
#' p3 <-
#'   marxan_problem(pu_dat, spec_dat, puvspr_dat, bound_dat, blm = 5) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # print solution
#' head(s3)
#' }
#' @export
marxan_problem <- function(x, ...) UseMethod("marxan_problem")

#' @rdname marxan_problem
#' @method marxan_problem default
#' @export
marxan_problem.default <- function(x, ...) {
  stop("argument to x is not valid, it should be a character file path or ",
       "a data.frame containing the planning unit data")
}

#' @rdname marxan_problem
#' @method marxan_problem data.frame
#' @export
marxan_problem.data.frame <- function(x, spec, puvspr, bound = NULL,
                                      blm = 0, symmetric = TRUE, ...) {
  # assert arguments are valid
  assertthat::assert_that(no_extra_arguments(...))
  ## x
  assertthat::assert_that(
    is.data.frame(x),
    assertthat::has_name(x, "id"),
    assertthat::has_name(x, "cost"),
    is_count_vector(x$id),
    no_duplicates(x$id),
    assertthat::noNA(x$id),
    assertthat::has_name(x, "cost"),
    is.numeric(x$cost),
    assertthat::noNA(x$cost)
  )
  if (assertthat::has_name(x, "status")) {
    assertthat::assert_that(
      is.numeric(x$status),
      assertthat::noNA(x$status),
      all_match_of(x$status, seq(0, 3))
    )
  }
  ## spec
  assertthat::assert_that(
    is.data.frame(spec),
    assertthat::has_name(spec, "id"),
    is_count_vector(spec$id),
    no_duplicates(spec$id),
    assertthat::noNA(spec$id)
  )
  if (assertthat::has_name(spec, "name")) {
    assertthat::assert_that(
      is_inherits(spec$name, c("character", "factor")),
      no_duplicates(as.character(spec$name)),
      assertthat::noNA(spec$name)
    )
  } else {
   spec$name <- paste0("feature.", seq_len(nrow(spec)))
  }
  assertthat::assert_that(
    do.call(xor, as.list(c("prop", "amount") %in% names(spec))),
    msg = paste(
      "argument to spec must have the column \"prop\" or \"amount\" and",
      "not both"
    )
  )
  if (assertthat::has_name(spec, "prop")) {
    assertthat::assert_that(
      is.numeric(spec$prop),
      all_finite(spec$prop),
      all_proportion(spec$prop)
    )
  }
  if (assertthat::has_name(spec, "amount")) {
    assertthat::assert_that(
      is.numeric(spec$amount),
      all_finite(spec$amount)
    )
  }
  ## puvspr
  assertthat::assert_that(
    is.data.frame(puvspr),
    assertthat::has_name(puvspr, "pu"),
    assertthat::has_name(puvspr, "species"),
    assertthat::has_name(puvspr, "amount"),
    is_count_vector(puvspr$pu),
    is_count_vector(puvspr$species),
    is.numeric(puvspr$amount),
    all_finite(puvspr$pu),
    all_finite(puvspr$species),
    all_finite(puvspr$amount),
    all_match_of(puvspr$pu, x$id),
    all_match_of(puvspr$species, spec$id)
  )
  ## bound
  assertthat::assert_that(is_inherits(bound, c("NULL", "data.frame")))
  if (is.data.frame(bound)) {
    assertthat::assert_that(
      assertthat::has_name(bound, "id1"),
      assertthat::has_name(bound, "id2"),
      assertthat::has_name(bound, "boundary"),
      is_count_vector(bound$id1),
      is_count_vector(bound$id2),
      is.numeric(bound$boundary),
      assertthat::noNA(bound$id1),
      assertthat::noNA(bound$id2),
      assertthat::noNA(bound$boundary),
      all(bound$id1 %in% x$id),
      all(bound$id2 %in% x$id)
    )
  }
  ## blm
  assertthat::assert_that(
    assertthat::is.number(blm),
    all_finite(blm)
  )
  if (abs(blm) > 1e-15 && is.null(bound)) {
    warning(
      "no boundary data supplied, so the blm parameter has no effect",
      call. = FALSE, immediate. = TRUE
    )
  }
  ## symmetric
  assertthat::assert_that(
    assertthat::is.flag(symmetric),
    assertthat::noNA(symmetric)
  )
  if (!isTRUE(symmetric) && is.null(bound)) {
    warning(
      "no boundary data supplied, so the symmetric parameter has no effect",
      call. = FALSE, immediate. = TRUE
    )
  }
  # create locked in data
  if (assertthat::has_name(x, "status")) {
    x$locked_in <- x$status == 2
    x$locked_out <- x$status == 3
  } else {
    x$locked_in <- FALSE
    x$locked_out <- FALSE
  }
  # create problem
  p <- problem(x = x, features = spec, rij = puvspr, cost_column = "cost")
  # add objective
  p <- add_min_set_objective(p)
  # add locked in constraints
  if (any(x$locked_in))
    p <- add_locked_in_constraints(p, "locked_in")
  # add locked in constraints
  if (any(x$locked_out))
    p <- add_locked_out_constraints(p, "locked_out")
  # add targets
  if ("prop" %in% names(spec)) {
    p <- add_relative_targets(p, "prop")
  } else {
    p <- add_absolute_targets(p, "amount")
  }
  # add boundary data
  if (!is.null(bound) && isTRUE(symmetric)) {
    p <- add_boundary_penalties(p, penalty = blm, edge_factor = 1, data = bound)
  } else if (!is.null(bound) && !isTRUE(symmetric)) {
    p <- add_asym_connectivity_penalties(p, penalty = blm, data = bound)
  }
  # return problem
  p
}

#' @rdname marxan_problem
#' @method marxan_problem character
#' @export
marxan_problem.character <- function(x, ...) {
  # assert that arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::is.readable(x),
    no_extra_arguments(...),
    is_installed("data.table", "marxan_problem()")
  )
  # declare local functions
  parse_field <- function(field, lines) {
    x <- grep(field, lines, value = TRUE)
    if (length(x) == 0)
      return(NA)
    x <- sub(paste0(field, " "), "", x)
    x
  }
  load_file <- function(field, lines, input_dir, force = TRUE) {
    x <- parse_field(field, lines)
    if (is.na(x)) {
      # nocov start
      if (force) {
        stop("input file does not contain ", field, " field")
      }
      return(NULL)
      # nocov end
    }
    if (file.exists(x)) {
      return(data.table::fread(x, data.table = FALSE))
    } else if (file.exists(file.path(input_dir, x))) {
      return(data.table::fread(file.path(input_dir, x), data.table = FALSE))
    } else if (force) { # nocov
      stop("file path in ", field, " field does not exist") # nocov
    } else {
      return(NULL) # nocov
    }
  }
  # read marxan file
  input_dir <- dirname(x)
  x <- readLines(x)
  # parse working directory
  base_input_dir <- parse_field("INPUTDIR", x)
  if (!is.na(base_input_dir)) {
    if (
      (substr(base_input_dir, 1, 1) == "/")  || # absolute path on unix
      (substr(base_input_dir, 2, 2) == ":")     # absolute path on Windows
    ) {
      input_dir <- base_input_dir
    } else {
      input_dir <- file.path(input_dir, base_input_dir)
    }
  }
  # parse data
  pu_data <- load_file("PUNAME", x, input_dir)
  spec_data <- load_file("SPECNAME", x, input_dir)
  puvspr_data <- load_file("PUVSPRNAME", x, input_dir)
  bound_data <- load_file("BOUNDNAME", x, input_dir, force = FALSE)
  blm <- as.numeric(parse_field("BLM", x))
  # check if connectivity data should be asymmetric
  sym <- !isTRUE(as.logical(parse_field("ASYMMETRICCONNECTIVITY", x)))
  if (is.null(bound_data)) {
    sym <- TRUE # nocov
  }
  # return problem
  marxan_problem(
    x = pu_data,
    spec = spec_data,
    puvspr = puvspr_data,
    bound = bound_data,
    blm = ifelse(is.na(blm), 0, blm),
    symmetric = sym
  )
}
