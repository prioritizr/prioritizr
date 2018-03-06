#' @include internal.R pproto.R ConservationProblem-proto.R zones.R MiscParameter-proto.R
NULL

#' @export
methods::setOldClass("tbl_df")

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
#' @param targets \code{data.frame} or \code{\link[tibble]{tibble}} object. See
#'   the Details section for more information.
#'
#' @param ... not used.
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
#' The \code{targets} argument should contain the following fields (columns):
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
#'       Acceptable values include \code{"absolute"} and \code{"relative"}.
#'       These values correspond to \code{\link{add_absolute_targets}},
#'       and \code{\link{add_relative_targets}} respectively.}
#'
#'     \item{\code{"sense"}}{\code{character} sense of the target. Acceptable
#'       values include: \code{">="}, \code{"<="}, and \code{"="}. This field
#'       (column) is optional and if it is missing then target senses will
#'       default to \code{">="} values.}
#'
#'     \item{\code{"target"}}{\code{numeric} target threshold.}
#'
#'  }
#'
#' @return \code{\link{ConservationProblem-class}} object with the targets added
#'   to it.
#'
#' @seealso \code{\link{targets}}.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' data(sim_pu_raster, sim_features, sim_pu_zones_stack, sim_features_zones)
#'
#' # create problem with 10 % relative targets
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_relative_targets(0.1) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE, box = FALSE)
#' }
#' # create equivalent problem using add_manual_targets
#' p2 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_manual_targets(data.frame(feature = names(sim_features),
#'                                     type = "relative", sense = ">=",
#'                                     target = 0.1)) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2, main = "solution", axes = FALSE, box = FALSE)
#' }
#' # create problem with targets set for only a few features
#' p3 <- problem(sim_pu_raster, sim_features) %>%
#'       add_min_set_objective() %>%
#'       add_manual_targets(data.frame(
#'         feature = names(sim_features)[1:3], type = "relative",
#'         sense = ">=", target = 0.1)) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(s3, main = "solution", axes = FALSE, box = FALSE)
#' }
#' # create problem that aims to secure at least 10 % of the habitat for one
#' # feature whilst ensuring that the solution does not capture more than
#' # 20 units habitat for different feature
#' # create problem with targets set for only a few features
#' p4 <- problem(sim_pu_raster, sim_features[[1:2]]) %>%
#'       add_min_set_objective() %>%
#'       add_manual_targets(data.frame(
#'         feature = names(sim_features)[1:2], type = "relative",
#'         sense = c(">=", "<="), target = c(0.1, 0.2))) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s4 <- solve(p4)
#'
#' # plot solution
#' plot(s4, main = "solution", axes = FALSE, box = FALSE)
#' }
#' # create a multi-zone problem that requires a specific amount of each
#' # feature in each zone
#' targets_matrix <- matrix(rpois(15, 1), nrow = n_feature(sim_features_zones),
#'                          ncol = n_zone(sim_features_zones))
#'
#' p5 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_absolute_targets(targets_matrix) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s5 <- solve(p5)
#'
#' # plot solution
#' plot(category_layer(s5), main = "solution", axes = FALSE, box = FALSE)
#' }
#' # create equivalent problem using add_manual_targets
#' targets_dataframe <- expand.grid(feature = feature_names(sim_features_zones),
#'                                  zone = zone_names(sim_features_zones),
#'                                  sense = ">=", type = "absolute")
#' targets_dataframe$target <- c(targets_matrix)
#'
#' p6 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
#'       add_min_set_objective() %>%
#'       add_manual_targets(targets_dataframe) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s6 <- solve(p6)
#'
#' # plot solution
#' plot(category_layer(s6), main = "solution", axes = FALSE, box = FALSE)
#' }
#' # create a problem that requires a total of 20 units of habitat to be
#' # captured for two species. This can be achieved through representing
#' # habitat in two zones. The first zone represents a full restoration of the
#' # habitat and a second zone represents a partial restoration of the habitat
#' # Thus only half of the benefit that would have been gained from the full
#' # restoration is obtained when planning units are allocated a partial
#' # restoration
#'
#' # create data
#' spp_zone1 <- as.list(sim_features_zones)[[1]][[1:2]]
#' spp_zone2 <- spp_zone1 * 0.5
#' costs <- sim_pu_zones_stack[[1:2]]
#'
#' # create targets
#' targets_dataframe2 <- tibble::tibble(
#'   feature = names(spp_zone1), zone = list(c("z1", "z2"), c("z1", "z2")),
#'   sense = c(">=", ">="), type = c("absolute", "absolute"),
#'   target = c(20, 20))
#'
#' # create problem
#' p7 <- problem(costs, zones(spp_zone1, spp_zone2,
#'                            feature_names = names(spp_zone1),
#'                            zone_names = c("z1", "z2"))) %>%
#'       add_min_set_objective() %>%
#'       add_manual_targets(targets_dataframe2) %>%
#'       add_binary_decisions()
#' \donttest{
#' # solve problem
#' s7 <- solve(p7)
#'
#' # plot solution
#' plot(category_layer(s7), main = "solution", axes = FALSE, box = FALSE)
#' }
#' @aliases add_manual_targets-method add_manual_targets,ConservationProblem,data.frame-method add_manual_targets,ConservationProblem,tbl_df-method
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
    add_manual_targets(x, tibble::as.tibble(targets))
})

#' @name add_manual_targets
#' @rdname add_manual_targets
#' @usage \S4method{add_manual_targets}{ConservationProblem,tbl_df}(x, targets, ...)
methods::setMethod(
  "add_manual_targets",
  methods::signature("ConservationProblem", "tbl_df"),
  function(x, targets, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "ConservationProblem"))
    validate_targets <- function(targets) {
      assertthat::assert_that(
        inherits(targets, "tbl_df"),
        assertthat::has_name(targets, "feature"),
        assertthat::has_name(targets, "target"),
        assertthat::has_name(targets, "type"),
        all(names(targets) %in% c("feature", "zone", "type", "sense",
                                  "target")),
        is.character(targets$feature) || is.factor(targets$feature),
        all(as.character(targets$feature) %in% x$feature_names()),
        is.numeric(targets$target), all(is.finite(targets$target)),
        is.character(targets$type) || is.factor(targets$type),
        all(targets$type %in% c("absolute", "relative")))
      if (x$number_of_zones() > 1 || assertthat::has_name(targets, "zone")) {
        assertthat::assert_that(
          assertthat::has_name(targets, "zone"),
          is.character(targets$zone) || is.factor(targets$zone) ||
          is.list(targets$zone),
          all(unlist(targets$zone) %in% x$zone_names()))
        assertthat::assert_that(
          all(vapply(targets$zone, inherits, logical(1),
              c("character", "factor"))),
          msg = paste("argument to targets has list-type cells in the column",
                      "\"zone\" which contain invalid values"))
      }
      if (assertthat::has_name(targets, "sense"))
       assertthat::assert_that(
         is.character(targets$sense) || is.factor(targets$sense),
         all(as.character(targets$sense) %in% c(">=", "<=", "=")))
      return(TRUE)
    }
    validate_targets(targets)
    # define function to validate changes to the targets object
    vfun <- function(x) !inherits(try(validate_targets(x), silent = TRUE),
                                  "try-error")
    # define function to render targets object
    rfun <- function(x)
      getFromNamespace("rhandsontable", "rHandsontableOutput")(x)
    # add targets to problem
    x$add_targets(pproto(
    "ManualTargets",
    Target,
    name = "Targets",
    data = list(abundances = x$feature_abundances_in_total_units()),
    parameters = parameters(misc_parameter("Targets", targets, vfun, rfun)),
    repr = function(self) {
      targets <- self$parameters$get("Targets")
      if (all(as.character(targets$type) == "relative")) {
        out <- "Relative"
      } else if (all(as.character(targets$type) == "absolute")) {
        out <- "Absolute"
      } else {
        out <- "Mixed"
      }
      out <- paste0(out, " targets [targets (min: ", min(targets$target),
                    ", max: ", max(targets$target), ")]")
      return(out)
     },
     output = function(self) {
       # get data
       targets <- self$parameters$get("Targets")
       abundances <- self$data$abundances
       # add zone column if missing
       if (!assertthat::has_name(targets, "zone"))
         targets$zone <- colnames(abundances)[[1]]
       # convert zone column to list of characters if needed
       if (!inherits(targets$zone, "list"))
        targets$zone <- as.list(targets$zone)
       # add sense column if missing
       if (!assertthat::has_name(targets, "sense"))
         targets$sense <- ">="
       targets$sense <- as.character(targets$sense)
       # convert feature names to indices
       targets$feature <- match(targets$feature, rownames(abundances))
       # convert zone names to indices
       for (i in seq_len(nrow(targets))) {
         targets$zone[[i]] <- match(targets$zone[[i]], colnames(abundances))
       }
       # add compute relative targets as absolute targets and assign
       # zone ids
       targets$value <- as.numeric(targets$target)
       relative_rows <- which(targets$type == "relative")
       for (i in seq_along(relative_rows)) {
          zone_id <- targets$zone[[relative_rows[[i]]]]
          feature_id <- targets$feature[[relative_rows[[i]]]]
          abund_mtx <- as.matrix(data.frame(feature_id, zone_id))
          targets$value[relative_rows[i]] <- sum(abundances[abund_mtx]) *
                                             targets$target[relative_rows[i]]
       }
       # return tibble
       return(targets[, c("feature", "zone", "sense", "value")])
     }))
})
