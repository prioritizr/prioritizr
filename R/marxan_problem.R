#' @include internal.R
NULL

#' \emph{Marxan} conservation problem
#'
#' Create a conservation planning \code{\link{problem}} following the
#' mathematical formulations used in \emph{Marxan} (detailed in Beyer
#' \emph{et al.} 2016).
#'
#' @param x \code{character} file path for a \emph{Marxan} input file (typically
#'   called \code{"input.dat"}), or \code{data.frame} containing planning unit
#'   data (typically called \code{"pu.dat"}). If the argument to \code{x} is a
#'   \code{data.frame}, then each row corresponds to a different planning unit,
#'   and it must have the following columns:
#'
#'   \describe{
#'
#'   \item{\code{"id"}}{\code{integer} unique identifier for each planning unit.
#'     These identifiers are used in the argument to \code{puvspr}.}
#'
#'   \item{\code{"cost"}}{\code{numeric} cost of each planning unit.}
#'
#'   \item{\code{"status"}}{\code{integer} indicating if each planning unit
#'     should not be locked in the solution (0) or if it should be locked in
#'     (2) or locked out (3) of the solution. Although \emph{Marxan} allows
#'     planning units to be selected in the initial solution (using values of
#'     1), these values have no effect here. This column is optional.}
#'
#'   }
#'
#' @param spec \code{data.frame} containing information on the features.
#'   The argument to \code{spec} must follow the conventions used by
#'   \emph{Marxan} for the species data file (conventionally called
#'   \code{"spec.dat"}). Each row corresponds to a different feature and
#'   each column corresponds to different information about the features. It
#'   must contain the columns listed below. Note that the argument to
#'   \code{spec} must contain at least one column named \code{"prop"} or
#'   \code{"amount"}---\strong{but not both columns with both of these
#'   names}---to specify the target for each feature.
#'
#'   \describe{
#'
#'   \item{\code{"id"}}{\code{integer} unique identifier for each feature
#'     These identifiers are used in the argument to \code{puvspr}.}
#'
#'   \item{\code{"name"}}{\code{character} name for each feature.}
#'
#'   \item{\code{"prop"}}{\code{numeric} relative target for each feature
#'     (optional).}'
#'
#'   \item{\code{"amount"}}{\code{numeric} absolute target for each
#'     feature (optional).}
#'
#'   }
#'
#' @param puvspr \code{data.frame} containing information on the amount of
#'   each feature in each planning unit. The argument to
#'   \code{puvspr} must follow the conventions used in the \emph{Marxan} input
#'   data file (conventionally called \code{"puvspr.dat"}). It must contain the
#'   following columns:
#'
#'   \describe{
#'
#'   \item{\code{"pu"}}{\code{integer} planning unit identifier.}
#'
#'   \item{\code{"species"}}{\code{integer} feature identifier.}
#'
#'   \item{\code{"amount"}}{\code{numeric} amount of the feature in the
#'        planning unit.}
#'
#'   }
#'
#' @param bound \code{NULL} object indicating that no boundary data
#'   is required for the conservation planning problem, or a \code{data.frame}
#'   containing information on the planning units' boundaries. The argument to
#'   \code{bound} must follow the conventions used in the \emph{Marxan} input
#'   data file (conventionally called \code{"bound.dat"}). It must contain the
#'   following columns:
#'
#'   \describe{
#'
#'   \item{\code{"id1"}}{\code{integer} planning unit identifier.}
#'
#'   \item{\code{"id2"}}{\code{integer} planning unit identifier.}
#'
#'   \item{\code{"boundary"}}{\code{numeric} length of shared boundary
#'     between the planning units identified in the previous two columns.}
#'
#'   }
#'
#' @param blm \code{numeric} boundary length modifier. This argument only
#'   has an effect when argument to \code{x} is a \code{data.frame}. The
#'   default argument is zero.
#'
#' @param ... not used.
#'
#' @details This function is provided as a convenient wrapper for solving
#'   \emph{Marxan} problems using \pkg{prioritizr}. Although this function
#'   could accommodate asymmetric connectivity in earlier versions of the
#'   \pkg{prioritizr} package, this functionality is no longer available.
#'   Please see the \code{\link{add_connectivity_penalties}} function for
#'   adding asymmetric connectivity penalties to a conservation planning
#'   problem. For more information on the correct formats
#'   for \emph{Marxan} input data, see the
#'   \href{http://marxan.net}{official \emph{Marxan} website} and Ball
#'   \emph{et al.} (2009).
#'
#' @return \code{\link{ConservationProblem-class}} object.
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) \emph{Marxan and relatives:
#' Software for spatial conservation prioritisation} in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
#' conservation planning problems with integer linear programming.
#' \emph{Ecological Modelling}, 228: 14--22.
#'
#' @examples
#' # create Marxan problem using Marxan input file
#' input_file <- system.file("extdata/input.dat", package = "prioritizr")
#' p1 <- marxan_problem(input_file)
#' \donttest{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # print solution
#' head(s1)
#' }
#' # create Marxan problem using data.frames that have been loaded into R
#' ## load in planning unit data
#' pu_path <- system.file("extdata/input/pu.dat", package = "prioritizr")
#' pu_dat <- data.table::fread(pu_path, data.table = FALSE)
#' head(pu_dat)
#'
#' ## load in feature data
#' spec_path <- system.file("extdata/input/spec.dat", package = "prioritizr")
#' spec_dat <- data.table::fread(spec_path, data.table = FALSE)
#' head(spec_dat)
#'
#' ## load in planning unit vs feature data
#' puvspr_path <- system.file("extdata/input/puvspr.dat",
#'                            package = "prioritizr")
#' puvspr_dat <- data.table::fread(puvspr_path, data.table = FALSE)
#' head(puvspr_dat)
#'
#' ## load in the boundary data
#' bound_path <- system.file("extdata/input/bound.dat", package = "prioritizr")
#' bound_dat <- data.table::fread(bound_path, data.table = FALSE)
#' head(bound_dat)
#'
#' # create problem without the boundary data
#' p2 <- marxan_problem(pu_dat, spec_dat, puvspr_dat)
#' \donttest{
#' # solve problem
#' s2 <- solve(p2)
#'
#' # print solution
#' head(s2)
#' }
#' # create problem with the boundary data and a boundary length modifier
#' # set to 5
#' p3 <- marxan_problem(pu_dat, spec_dat, puvspr_dat, bound_dat, 5)
#' \donttest{
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
                                      blm = 0, ...) {
  # assert arguments are valid
  assertthat::assert_that(no_extra_arguments(...))
  ## x
  assertthat::assert_that(
    inherits(x, "data.frame"),
    assertthat::has_name(x, "id"),
    assertthat::has_name(x, "cost"),
    is.numeric(x$id),
    anyDuplicated(x$id) == 0,
    assertthat::noNA(x$id),
    assertthat::has_name(x, "cost"),
    is.numeric(x$cost),
    assertthat::noNA(x$cost))
  if ("status" %in% names(x))
    assertthat::assert_that(
      is.numeric(x$status),
      assertthat::noNA(x$status),
      all(x$status %in% seq(0, 3)))
  ## spec
  assertthat::assert_that(
    inherits(spec, "data.frame"),
    assertthat::has_name(spec, "id"),
    is.numeric(spec$id),
    anyDuplicated(spec$id) == 0,
    assertthat::noNA(spec$id))
  if ("name" %in% names(spec)) {
   assertthat::assert_that(
     inherits(spec$name, c("character", "factor")),
     anyDuplicated(as.character(spec$name)) == 0,
     assertthat::noNA(as.character(spec$name)))
  } else {
   spec$name <- paste0("feature.", seq_len(nrow(spec)))
  }
  if (!do.call(xor, as.list(c("prop", "amount") %in% names(spec))))
    stop("argument to spec must have the column \"prop\" or \"amount\" and " ,
         "not both")
  if ("prop" %in% names(spec)) {
    assertthat::assert_that(
      is.numeric(spec$prop),
      assertthat::noNA(spec$prop),
      all(spec$prop >= 0),
      all(spec$prop <= 1))
  }
  if ("amount" %in% names(spec)) {
    assertthat::assert_that(
      is.numeric(spec$amount),
      assertthat::noNA(spec$amount))
  }
  ## puvspr
  assertthat::assert_that(
    inherits(puvspr, "data.frame"),
    assertthat::has_name(puvspr, "pu"),
    assertthat::has_name(puvspr, "species"),
    assertthat::has_name(puvspr, "amount"),
    is.numeric(puvspr$pu),
    is.numeric(puvspr$species),
    is.numeric(puvspr$amount),
    assertthat::noNA(puvspr$pu),
    assertthat::noNA(puvspr$species),
    assertthat::noNA(puvspr$amount),
    all(puvspr$pu %in% x$id),
    all(puvspr$species %in% spec$id))
  ## bound
  assertthat::assert_that(inherits(bound, c("NULL", "data.frame")))
  if (inherits(bound, c("data.frame"))) {
    assertthat::assert_that(
      assertthat::has_name(bound, "id1"),
      assertthat::has_name(bound, "id2"),
      assertthat::has_name(bound, "boundary"),
      is.numeric(bound$id1),
      is.numeric(bound$id2),
      is.numeric(bound$boundary),
      assertthat::noNA(bound$id1),
      assertthat::noNA(bound$id2),
      assertthat::noNA(bound$boundary),
      all(bound$id1 %in% x$id), all(bound$id2 %in% x$id))
  }
  ## blm
  assertthat::assert_that(assertthat::is.scalar(blm), is.finite(blm))
  if (abs(blm) > 1e-50 && is.null(bound))
    warning("no boundary data supplied so the blm argument has no effect")
  # create locked in data
  if (assertthat::has_name(x, "status")) {
    x$locked_in <- x$status == 2
    x$locked_out <- x$status == 3
  } else {
    x$locked_in <- FALSE
    x$locked_out <- FALSE
  }
  # create problem
  p <- problem(x, spec, puvspr, cost_column = "cost")
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
  if (!is.null(bound)) {
      p <- add_boundary_penalties(p, blm, 1, data = bound)
  }
  # return problem
  return(p)
}

#' @rdname marxan_problem
#' @method marxan_problem character
#' @export
marxan_problem.character <- function(x, ...) {
  # assert that arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::is.readable(x),
    no_extra_arguments(...))
  # declare local functions
  parse_field <- function(field, lines) {
      x <- grep(field, lines, value = TRUE)
      if (length(x) == 0)
        return(NA)
      x <- sub(paste0(field, " "), "", x)
      return(x)
  }
  load_file <- function(field, lines, input_dir, force = TRUE) {
    x <- parse_field(field, lines)
    if (is.na(x)) {
      if (force)
        stop("input file does not contain ", field, " field")
      return(NULL)
    }
    if (file.exists(x)) {
      return(data.table::fread(x, data.table = FALSE))
    } else if (file.exists(file.path(input_dir, x))) {
      return(data.table::fread(file.path(input_dir, x), data.table = FALSE))
    } else if (force) {
      stop("file path in ", field, " field does not exist")
    } else {
      return(NULL)
    }
  }
  # read marxan file
  input_dir <- dirname(x)
  x <- readLines(x)
  # parse working directory
  base_input_dir <- parse_field("INPUTDIR", x)
  if (!is.na(base_input_dir)) {
    if ((substr(base_input_dir, 1, 1) == "/")  || # absolute path on unix
        (substr(base_input_dir, 2, 2) == ":")) { # absolute path on Windows
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
  # check that asymmetric connectivity is not specified
  asym <- as.logical(parse_field("ASYMMETRICCONNECTIVITY", x))
  if (isTRUE(asym))
   stop("marxan_problem function cannot accommodate asymmetric connectivity")
  # return problem
  marxan_problem(x = pu_data, spec = spec_data, puvspr = puvspr_data,
                 bound = bound_data, blm = ifelse(is.na(blm), 0, blm))
}
