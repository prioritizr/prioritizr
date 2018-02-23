#' @include internal.R pproto.R ConservationProblem-proto.R zones.R MiscParameter-proto.R
NULL

#' @export
methods::setOldClass("tibble")

#' Add Manual Targets
#'
#' Set targets for a for a conservation planning \code{\link{problem}}.
#' This function can be used to customize all aspects of a target. For most
#' cases, targets can be specified using the \code{link{add_absolute_targets}}
#' and \code{\link{add_relative_targets}} functions. However, this function
#' can be used to (i) mix absolute and relative targets for different
#' features and zones, (ii) set targets that pertain to the allocations of
#' planning units in multiple zones, and (iii) set targets that require
#' different senses (e.g. targets which specify the solution should not exceed
#' a certain quantity using \code{"<="} values).
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param targets \code{data.frame} or \code{\link[tibble]{tibble}} object.
#'   This argument should contain the following fields (columns):
#'   \describe{
#'
#'     \item{\code{"feature"}}{\code{character} name of features in argument
#'       to \code{x}.}
#'
#'     \item{\code{"zone"}}{\code{character} name of zones in argument to
#'       \code{x}. This field (column) is optional for arguments to \code{x}
#'       that do not contain multiple zones.}
#'
#'     \item{\code{"type"}}{\code{character} describing the type of target.
#'       Acceptable values include: \code{"absolute"} and \code{"relative"}.
#'       These values correspond to \code{\link{add_absolute_targets}},
#'       and \code{\link{add_relative_targets}} respectively.}
#'
#'     \item{\code{"sense"}}{\code{character} sense of the target. Acceptable
#'       values include: \code{">="}, \code{"<="}, and \code{"="}. This field
#'       (column) is optional and if it is missing then target senses will
#'       default to \code{">="} values.}
#'
#'     \item{\code{"target"}}{\code{numeric} target value.}
#'
#'  }
#'
#' @details
#' Targets are used to specify the minimum amount or proportion of a feature's
#' distribution that needs to be protected. Most conservation planning problems
#' require targets with the exception of the maximum cover
#' (see \code{\link{add_max_cover_objective}}) and maximum utility
#' (see \code{\link{add_max_utility_objective}}) problems. Attempting to solve
#' problems with objectives that require targets without specifying targets
#' will throw an error.
#'
#' @return \code{\link{ConservationProblem-class}} object with the targets added
#'   to it.
#'
#' @seealso \code{\link{targets}}.
#'
#' @aliases add_manual_targets-method add_manual_targets,ConservationProblem,data.frame-method add_manual_targets,ConservationProblem,tibble-method
#'
#' @name add_manual_targets
#'
#' @docType methods
NULL

#' @name add_manual_targets
#' @rdname add_manual_targets
#' @exportMethod add_manual_targets
#' @export
methods::setGeneric(
  "add_manual_targets",
  signature = methods::signature("x", "targets"),
  function(x, targets, ...) standardGeneric("add_manual_targets"))

#' @name add_manual_targets
#' @rdname add_manual_targets
#' @usage \S4method{add_manual_targets}{ConservationProblem,data.frame}(x, targets, ...)
methods::setMethod(
  "add_manual_targets",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, targets, ...) {
    add_manual_targets(x, tibble::as.tibble(x))
})

#' @name add_manual_targets
#' @rdname add_manual_targets
#' @usage \S4method{add_manual_targets}{ConservationProblem,tibble}(x, targets, ...)
methods::setMethod(
  "add_manual_targets",
  methods::signature("ConservationProblem", "tibble"),
  function(x, targets, ...) {
    # assert that arguments are valid
    validate_targets <- function(x) {
      assertthat::assert_that(
        inherits(x, "ConservationProblem"),
        inherits(targets, "tibble"),
        assert_that::has_name(targets, "feature"),
        assert_that::has_name(targets, "target"),
        assert_that::has_name(targets, "type"),
        is.character(targets$feature) || is.factor(targets$feature),
        all(as.character(targets$feature) %in% x$feature_names()),
        is.numeric(targets$target),
        is.character(targets$type) || is.factor(targets$type),
        all(targets$type %in% c("absolute", "relative")))
      if (x$number_of_zones() > 1 || assert_that::has_name(targets, "zone")) {
        assertthat::assert_that(
          assert_that::has_name(targets, "zone"),
          is.character(targets$zone) || is.factor(targets$zone) ||
          is.list(targets$zone))
        assertthat::assert_that(
          all(vapply(targets$zone, inherits, logical(1),
              c("character", "factor"))),
          msg = paste("argument to targets has list-type cells in the column",
                      "\"zone\" which contain invalid values"))
      }
      if (assert_that::has_name(targets, "sense"))
       assertthat::assert_that(
         is.character(targets$sense) || is.factor(targets$sense),
         all(as.character(targets$sense) %in% c(">=", "<=", "=")))
      return(TRUE)
    }
    validate_targets(targets)
    # define function to validate changes to the targets object
    vfun <- function(x) inherits(try(validate_targets(x), silent = TRUE),
                                class = "try-error")
    # define function to render targets object
    rfun <- function(x)
      getFromNamespace("rhandsontable", "rHandsontableOutput")(x)
    # add targets to problem
    x$add_targets(pproto(
    "ManualTargets",
    Target,
    name = "Targets",
    data = list(abundances = x$feature_abundances_in_planning_units()),
    parameters = parameters(misc_parameter("Targets", targets, vfun, rfun)),
    repr = function() {
      targets <- self$parameters$get("targets")
      if (all(as.character(targets$type) == "relative")) {
        out <- "Relative"
      } else if (all(as.character(targets$type) == "absolute")) {
        out <- "Absolute"
      } else {
        out <- "Mixed"
      }
      out <- paste0(" targets [targets (min: ", min(targets$target),
                    ", max: ", max(targets$target), ")]")
      return(out)
     },
     output = function(self) {
       # get data
       targets <- self$parameters$get("targets")
       abundances <- self$data$abundances
       # add zone column if missing
       if (!assert_that::has_name(targets, "zone"))
         targets$zone <- "zone"
       # convert zone column to list of characters if needed
       if (!inherits(target$zone, "list"))
        targets$zone <- as.list(targets$zone)
       # add sense column if missing
       if (!assert_that::has_name(targets, "sense"))
        targets$sense <- ">="
       # add compute relative targets as absolute targets
       targets$value <- targets$target
       relative_rows <- which(targets$type == "relative")
       feature_id <- match(targets$feature, rownames(abundances))
       for (i in seq_along(relative_rows)) {
          zone_names <- targets$zone[[relative_rows[[i]]]]
          zone_id <- match(zone_names, colnames(abundances))
          abund_mtx <- as.matrix(data.frame(feature_id[i], zone_id))
          targets$value[relative_rows[i]] <- sum(abundances[abund_mtx]) *
                                             targets$target[relative_rows[i]]
       }
       # return tibble
       return(targets)
     }))
})
