#' @include internal.R
NULL

#' Marxan conservation problem
#'
#' Create a Marxan conservation problem. Although users are encouraged to
#' build and tailor conservation planning problems to suit their own needs,
#' sometimes it is easier to use a more familiar "canned" approach. This
#' function provides a convenient wrapper for generating
#' and solving Marxan-style conservation problems. If users already have
#' their conservation planning data in the Marxan input format, this function
#' can also be used to read Marxan data files and solve the Marxan-style
#' problems using exact algorithm solvers.
#'
#' @param x \code{character} file path for Marxan input file (typically
#'   called \code{"input.dat"}), a \code{data.frame} object containing
#'   planning unit data following conventions (ie. \code{"pu.dat"}), or a
#'   \code{\link[sp]{Spatial-class}} or \code{\link[raster]{Raster-class}}
#'   object containing planning unit data.
#'
#' @param features \code{\link[raster]{Raster-class}} object containing
#'   data on the distribution of features across the study area.
#'
#' @param targets \code{numeric} amount of each feature to be represented
#'   in the solution.
#'
#' @param targets_type \code{character} name indicating if the \code{targets}
#'   are expressed as \code{"relative"} (eg. \code{0.2} meaning that 20 \% of a
#'   feature needs to be conserved), or \code{"absolute"} (eg. \code{200}
#'   meaning that 200 units of a feature need to be conserved) amounts.
#'
#' @param spec \code{data.frame} containing information on the features.
#'   The argument to \code{spec} must follow the conventions used by
#'   Marxan for the species data file (conventionally called \code{"spec.dat"}).
#'   Each row corresponds to a different feature. It must also contain
#'   the following columns:
#'   \describe{
#'     \item{\code{"id"}}{\code{integer} unique identifier for each feature
#'       These identifiers are used in the argument to \code{rij_data}.}
#'     \item{\code{"name"}}{\code{character} name for each feature.}
#'     \item{\code{"prop"}}{\code{numeric} relative target for each feature
#'        (optional).}'
#'     \item{\code{"amount"}}{\code{numeric} absolute target for each
#'         feature (optional).}
#'   }
#'
#' @param puvspr \code{data.frame} containing information on the amount of
#'    each feature in each planning unit. The argument to
#'    \code{puvspr} must follow the conventions used in the Marxan input data
#'    file (conventionally called \code{"puvspr.dat"}). It must contain the
#'    following columns:
#'    \describe{
#'      \item{\code{"pu"}}{\code{integer} planning unit identifier.}
#'      \item{\code{"species"}}{\code{integer} feature identifier.}
#'      \item{\code{"amount"}}{\code{numeric} amount of the feature in the
#'        planning unit.}
#'    }
#'
#' @param bound \code{data.frame} containing information on the
#'    planning units' boundaries or the connectivity between planning units.
#'    The argument to \code{bound} must follow the
#'    conventions used in the Marxan input data file (conventionally called
#'    \code{"bound.dat"}). It must contain the following columns:
#'    \describe{
#'      \item{\code{"id1"}}{\code{integer} planning unit identifier.}
#'      \item{\code{"id2"}}{\code{integer} planning unit identifier.}
#'      \item{\code{"boundary"}}{\code{numeric} length of shared boundary
#'        between planning units, or the strength of the connectivity between
#'        the two planning units.}
#'    }
#'
#' @param blm \code{numeric} boundary length modifier. This argument only
#'   has an effect when argument to \code{x} is a \code{data.frame}. Defaults
#'   to zero.
#'
#' @param asymmetric_connectivity \code{logical} does the argument
#'   to \code{bound} denote asymmetric connectivity between planning units?
#'   This argument only has an effect when argument to \code{bound} is a
#'   \code{data.frame}. Defaults to \code{FALSE}.
#'
#' @inheritParams add_boundary_penalties
#'
#' @inheritParams add_locked_in_constraints
#'
#' @inheritParams add_locked_out_constraints
#'
#' @param ... not used
#'
#' @return \code{\link{ConservationProblem-class}} object.
#'
#' @examples
#'
#' # create Marxan problem using spatial data
#' data(sim_pu_raster, sim_features)
#' p1 <- marxan_problem(sim_pu_raster, features=sim_features, targets=0.2,
#'                      targets_type="relative", penalty=1,
#'                      edge_factor=0.5)
#'
#' \donttest{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # show solution
#' plot(s1)
#' }
#'
#' # create Marxan problem using Marxan input files
#' input_file <- system.file("extdata/input.dat", package="prioritizr")
#' p2 <- marxan_problem(input_file)
#'
#' \donttest{
#' # solve problem
#' s2 <- solve(p2)
#'
#' # count number of selected planning units in solution
#' print(sum(s2))
#' }
#'
#' @name marxan_problem
#'
#' @export
marxan_problem <- function(x, ...) UseMethod("marxan_problem")

#' @rdname marxan_problem
#' @method marxan_problem default
#' @export
marxan_problem.default <- function(x, features, targets,
                                  targets_type = c("relative", "absolute"),
                                  locked_in = NULL, locked_out = NULL,
                                  penalty = 0, edge_factor = 0.5, ...) {
  # create problem
  p <- problem(x, features) %>%
    add_min_set_objective() %>%
    add_boundary_penalties(penalty, edge_factor)
  if (targets_type == "relative")
    p <- p %>% add_relative_targets(targets)
  if (targets_type == "absolute")
    p <- p %>% add_absolute_targets(targets)
  if (!is.null(locked_in))
    p <- p %>% add_locked_in_constraints(locked_in)
  if (!is.null(locked_out))
    p <- p %>% add_locked_out_constraints(locked_out)
  # return problem
  return(p)
}

#' @rdname marxan_problem
#' @method marxan_problem data.frame
#' @export
marxan_problem.data.frame <- function(x, spec, puvspr, bound = NULL,
                                      blm = 0, asymmetric_connectivity = FALSE,
                                      ...) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "data.frame"), inherits(spec, "data.frame"),
    inherits(puvspr, "data.frame"),
    is.null(bound) || inherits(bound, "data.frame"),
    assertthat::is.scalar(blm),
    assertthat::is.flag(asymmetric_connectivity))
  # create locked in data
  if (assertthat::has_name(x, "status")) {
    x$locked_in <- x$status == 2
    x$locked_out <- x$status == 3
  }
  # add in feature names if not present
  if (!assertthat::has_name(spec, "name"))
    spec$name <- paste0("feature.", seq_len(nrow(spec)))
  # create problem
  p <- problem(x, spec, puvspr) %>%
    add_min_set_objective()
  # add locked in/out constraints
  if (sum(x$locked_in) > 0)
    p <- p %>% add_locked_in_constraints("locked_in")
  if (sum(x$locked_out) > 0)
    p <- p %>% add_locked_out_constraints("locked_out")
  # add targets
  if (assertthat::has_name(spec, "prop")) {
    p <- add_relative_targets(p, "prop")
  } else {
  if (assertthat::has_name(spec, "amount"))
    p <- add_absolute_targets(p, "amount")
  }
  # add bounday data
  if (is.null(bound) & (blm > 1e-10))
    warning("no boundary data supplied to blm will have no effect on results")
  if (!is.null(bound)) {
    # sanity checks
    if (!"id1" %in% names(bound))
      stop("file path listed under BOUNDNAME is missing the \"id1\" column")
    if (!"id2" %in% names(bound))
      stop("file path listed under BOUNDNAME is missing the \"id2\" column")
    if (!"boundary" %in% names(bound))
      stop("file path listed under BOUNDNAME is missing the \"boundary\"",
           " column")
    # appply penalties
    if (!asymmetric_connectivity) {
      p <- add_boundary_penalties(p, blm, 1, bound)
    } else {
      p <- add_connectivity_penalties(p, blm, bound)
    }
  }
  # return problem
  return(p)
}

#' @rdname marxan_problem
#' @method marxan_problem character
#' @export
marxan_problem.character <- function(x, ...) {
 assertthat::assert_that(assertthat::is.string(x), assertthat::is.readable(x))
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
    x <- file.path(input_dir, x)
    if (file.exists(x)) {
      if (requireNamespace("data.table", quietly = TRUE)) {
        return(data.table::fread(x, data.table = FALSE))
      } else {
        stop("the \"data.table\" package needs to be installed to load Marxan",
             " data files.")
      }
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
  if (!is.na(base_input_dir))
      input_dir <- file.path(input_dir, base_input_dir)
  # parse data
  pu_data <- load_file("PUNAME", x, input_dir)
  spec_data <- load_file("SPECNAME", x, input_dir)
  puvspr_data <- load_file("PUVSPRNAME", x, input_dir)
  bound_data <- load_file("BOUNDNAME", x, input_dir, force = TRUE)
  blm <- as.numeric(parse_field("BLM", x))
  asym <- as.logical(parse_field("ASYMMETRICCONNECTIVITY", x))
  if (is.na(asym))
   asym <- FALSE
  # return problem
  marxan_problem(x = pu_data, spec = spec_data, puvspr = puvspr_data,
                 bound = bound_data, blm = blm,
                 asymmetric_connectivity = asym)
}
