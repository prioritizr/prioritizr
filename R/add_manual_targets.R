#' @include internal.R ConservationProblem-class.R zones.R tbl_df.R target_optimization_format.R
NULL

#' Add manual targets
#'
#' Set targets for a conservation planning problem by manually
#' specifying all the required information for each target. This function
#' is useful because it can be used to customize all aspects of a target. For
#' most cases, targets can be specified using the
#' [add_absolute_targets()] and [add_relative_targets()]
#' functions. However, this function can be used to (i) mix absolute and
#' relative targets for different features and zones, (ii) set targets that
#' pertain to the allocations of planning units in multiple zones, and (iii)
#' set targets that require different senses (e.g., targets which specify the
#' solution should not exceed a certain quantity using `"<="` values).
#'
#' @param x [problem()] object.
#'
#' @param targets `data.frame` or [tibble::tibble()] object.
#'   See the Targets format section for more information.
#'
#' @inherit add_absolute_targets details
#'
#' @inheritSection add_auto_targets Target setting
#'
#' @section Targets format:
#'
#' The `targets` argument should be a `data.frame` with the following
#' columns:
#'
#' \describe{
#'
#' \item{feature}{`character` name of features in argument
#'   to `x`.}
#'
#' \item{zone}{`character` name of zones in the argument
#'   `x`. It can also be a `list` of `character` vectors if
#'   targets should correspond to multiple zones (see Examples section below).
#'   This column is optional for arguments to `x`
#'   that do not contain multiple zones.}
#'
#' \item{type}{`character` describing the type of target.
#'   Acceptable values include `"absolute"` and `"relative"`.
#'   These values correspond to [add_absolute_targets()],
#'   and [add_relative_targets()] respectively.}
#'
#' \item{sense}{`character` sense of the target. Acceptable
#'   values include: `">="`, `"<="`, and `"="`. This
#'   column is optional and if it is missing then target senses will
#'   default to `">="` values.}
#'
#' \item{target}{`numeric` target threshold.}
#'
#' }
#'
#' @return An updated [problem()] object with the targets added to it.
#'
#' @seealso
#' See [targets] for an overview of all functions for adding targets.
#'
#' @references
#' Carwardine J, Klein CJ, Wilson KA, Pressey RL, Possingham HP (2009) Hitting
#' the target and missing the point: target‐based conservation planning in
#' context. *Conservation Letters*, 2: 4--11.
#'
#' @family targets
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#' sim_zones_pu_raster <- get_sim_zones_pu_raster()
#' sim_zones_features <- get_sim_zones_features()
#'
#' # create problem with 10% relative targets
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # create equivalent problem using add_manual_targets
#' p2 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_manual_targets(
#'     data.frame(
#'       feature = names(sim_features),
#'       type = "relative", sense = ">=",
#'       target = 0.1
#'     )
#'   ) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2, main = "solution", axes = FALSE)
#'
#' # create problem with targets set for only a few features
#' p3 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_manual_targets(
#'     data.frame(
#'       feature = names(sim_features)[1:3],
#'       type = "relative",
#'       sense = ">=",
#'       target = 0.1
#'     )
#'  ) %>%
#'  add_binary_decisions() %>%
#'  add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s3 <- solve(p3)
#'
#' # plot solution
#' plot(s3, main = "solution", axes = FALSE)
#'
#' # create problem that aims to secure at least 10% of the habitat for one
#' # feature whilst ensuring that the solution does not capture more than
#' # 20 units habitat for different feature
#' # create problem with targets set for only a few features
#' p4 <-
#'   problem(sim_pu_raster, sim_features[[1:2]]) %>%
#'   add_min_set_objective() %>%
#'   add_manual_targets(
#'     data.frame(
#'       feature = names(sim_features)[1:2],
#'       type = "relative",
#'       sense = c(">=", "<="),
#'       target = c(0.1, 0.2)
#'     )
#'   ) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s4 <- solve(p4)
#'
#' # plot solution
#' plot(s4, main = "solution", axes = FALSE)
#'
#' # create a multi-zone problem that requires a specific amount of each
#' # feature in each zone
#' targets_matrix <- matrix(rpois(15, 1), nrow = 5, ncol = 3)
#'
#' p5 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets(targets_matrix) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s5 <- solve(p5)
#'
#' # plot solution
#' plot(category_layer(s5), main = "solution", axes = FALSE)
#'
#' # create equivalent problem using add_manual_targets
#' targets_dataframe <- expand.grid(
#'   feature = feature_names(sim_zones_features),
#'   zone = zone_names(sim_zones_features),
#'   sense = ">=",
#'   type = "absolute"
#' )
#' targets_dataframe$target <- c(targets_matrix)
#'
#' p6 <-
#'   problem(sim_zones_pu_raster, sim_zones_features) %>%
#'   add_min_set_objective() %>%
#'   add_manual_targets(targets_dataframe) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s6 <- solve(p6)
#'
#' # plot solution
#' plot(category_layer(s6), main = "solution", axes = FALSE)
#'
#' # create a problem that requires a total of 20 units of habitat to be
#' # captured for two species. This can be achieved through representing
#' # habitat in two zones. The first zone represents a full restoration of the
#' # habitat and a second zone represents a partial restoration of the habitat
#' # Thus only half of the benefit that would have been gained from the full
#' # restoration is obtained when planning units are allocated a partial
#' # restoration
#'
#' # create data
#' spp_zone1 <- as.list(sim_zones_features)[[1]][[1:2]]
#' spp_zone2 <- spp_zone1 * 0.5
#' costs <- sim_zones_pu_raster[[1:2]]
#'
#' # create targets
#' targets_dataframe2 <- tibble::tibble(
#'   feature = names(spp_zone1),
#'   zone = list(c("z1", "z2"), c("z1", "z2")),
#'   sense = c(">=", ">="),
#'   type = c("absolute", "absolute"),
#'   target = c(20, 20)
#' )
#'
#' # create problem
#' p7 <-
#'   problem(
#'     costs,
#'     zones(
#'       spp_zone1, spp_zone2,
#'       feature_names = names(spp_zone1), zone_names = c("z1", "z2")
#'     )
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_manual_targets(targets_dataframe2) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s7 <- solve(p7)
#'
#' # plot solution
#' plot(category_layer(s7), main = "solution", axes = FALSE)
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
  function(x, targets) {
    assert_required(x)
    assert_required(targets)
    assert(
      is_conservation_problem(x),
      is.data.frame(targets)
    )
    standardGeneric("add_manual_targets")
  }
)

#' @name add_manual_targets
#' @rdname add_manual_targets
#' @usage \S4method{add_manual_targets}{ConservationProblem,data.frame}(x, targets)
methods::setMethod(
  "add_manual_targets",
  methods::signature("ConservationProblem", "data.frame"),
  function(x, targets) {
    add_manual_targets(x, tibble::as_tibble(targets))
  }
)

#' @name add_manual_targets
#' @rdname add_manual_targets
#' @usage \S4method{add_manual_targets}{ConservationProblem,tbl_df}(x, targets)
methods::setMethod(
  "add_manual_targets",
  methods::signature("ConservationProblem", "tbl_df"),
  function(x, targets) {
    # assert that arguments are valid
    assert(
      is_conservation_problem(x),
      inherits(targets, "tbl_df"),
      assertthat::has_name(targets, "feature"),
      assertthat::has_name(targets, "target"),
      assertthat::has_name(targets, "type"),
      all_match_of(
        names(targets),
        c("feature", "zone", "type", "sense", "target")
      ),
      is_inherits(targets$feature, c("character", "factor")),
      all_match_of(as.character(targets$feature), feature_names(x)),
      is.numeric(targets$target),
      all_finite(targets$target),
      is_inherits(targets$type, c("character", "factor")),
      all_match_of(as.character(targets$type), c("absolute", "relative"))
    )
    if (x$number_of_zones() > 1 || assertthat::has_name(targets, "zone")) {
      assert(
        assertthat::has_name(targets, "zone"),
        is_inherits(targets$zone, c("character", "factor", "list")),
        all_match_of(unlist(targets$zone), zone_names(x))
      )
      if (is.list(targets$zone)) {
        assert(
          all_elements_inherit(targets$zone, c("character", "factor")),
          msg = c(
            "!" = paste0(
              "{.arg targets$zone} must contain elements are that a ",
              "{.cls character} or {.cls factor} value."
            ),
            "i" = paste0(
              "If {.arg targets$zone} is a {.cls list} column, ",
              "then each element of this {.cls list} column must contain a ",
              "{.cls character} or {.cls factor} value or vector."
            )
          )
        )
      }
    }
    if (assertthat::has_name(targets, "sense")) {
      assert(
        is_inherits(targets$sense, c("character", "factor")),
        all_match_of(targets$sense, c(">=", "<=", "="))
      )
    }
    # only check for negative values if this function is not being called
    # by add_absolute_targets(). this is because add_absolute_targets()
    # has its own checks for negative values
    if (
      !any(
        isTRUE(
          "add_absolute_targets" %in% as.character(rlang::caller_call(n = 1))
        ),
        isTRUE(
          "add_absolute_targets" %in% as.character(rlang::caller_call(n = 2))
        ),
        isTRUE(
          "add_absolute_targets" %in% as.character(rlang::caller_call(n = 3))
        )
      )
    ) {
      verify(all_positive(targets$target))
    }
    # add targets to problem
    x$add_targets(
      R6::R6Class(
        "ManualTargets",
        inherit = Target,
        public = list(
          name = "manual targets",
          data = list(targets = targets),
          repr = function(compact = TRUE) {
            d <- self$get_data("targets")
            if (all(as.character(d$type) == "relative")) {
              type <- "relative"
            } else if (all(as.character(d$type) == "absolute")) {
              type <- "absolute"
            } else {
              type <- "mixed"
            }
            cli::format_inline(
              "{type} targets (between {.val {range(d$target)}})"
            )
          },
          output = function(x) {
            # assert x is a conservation problem
            assert(
              is_conservation_problem(x),
              .internal = TRUE
            )
            # get targets
            targets <- self$get_data("targets")
            # get targets for optimization
            target_optimization_format(x, targets)
          }
        )
      )$new()
    )
  }
)
