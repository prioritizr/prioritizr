#' @include internal.R ConservationProblem-class.R target_optimization_format.R get_target_method.R
NULL

#' Add targets based on feature groups
#'
#' Add targets to a conservation planning problem, wherein each
#' feature is assigned to a particular group and a target setting
#' method is specified for each feature group.
#' This function is designed to provide a convenient alternative to
#' [add_auto_targets()].
#'
#' @inheritParams add_manual_targets
#'
#' @param groups `character` vector of group names.
#'
#' @param method `list` of name-value pairs that describe the target setting
#' method for each group. Here, the names of `method` correspond to different
#' values in `groups`. Additionally, the elements of `method` should be
#' the `character` names of target setting methods, or objects
#' ([`TargetMethod-class`]) that specify target setting methods.
#' See Target methods below for more information.
#'
#' @inheritSection targets Target setting
#'
#' @section Target methods:
#' Here `method` is used to specify a target setting method for each group.
#' In particular, each element should correspond to a different group,
#' with the name of the element indicating the group and the value
#' denoting the target setting method. One option for specifying a
#' particular target setting method is to use a `character` value
#' that denotes the name of the method (e.g., `"jung"` to set targets following
#' Jung *et al.* 2021). This option is particularly useful if default
#' parameters should be considered during target calculations.
#' Alternatively, another option for specifying a particular target setting
#' method is to use a function to define an object (e.g.,
#' [`spec_jung_targets()`] to set targets following Jung *et al.* 2021).
#' This alternative option is particularly useful
#' if customized parameters should be considered during target calculations.
#' Note that a `method` can contain a mix of target setting methods defined
#' using `character` values and functions.
#'
#' ```{r child = "man/rmd/target_method_character.md"}
#' ```
#'
#' ```{r child = "man/rmd/target_method_objects.md"}
#' ```
#'
#' @inheritSection add_auto_targets Data calculations
#'
#' @inherit add_relative_targets return seealso
#'
#' @references
#' Carwardine J, Klein CJ, Wilson KA, Pressey RL, Possingham HP (2009) Hitting
#' the target and missing the point: target‐based conservation planning in
#' context. *Conservation Letters*, 2: 4--11.
#'
#' Jung M, Arnell A, de Lamo X, García-Rangel S, Lewis M, Mark J, Merow C,
#' Miles L, Ondo I, Pironon S, Ravilious C, Rivers M, Schepaschenko D,
#' Tallowin O, van Soesbergen A, Govaerts R, Boyle BL, Enquist BJ, Feng X,
#' Gallagher R, Maitner B, Meiri S, Mulligan M, Ofer G, Roll U, Hanson JO,
#' Jetz W, Di Marco M, McGowan J, Rinnan DS, Sachs JD, Lesiv M, Adams VM,
#' Andrew SC, Burger JR, Hannah L, Marquet PA, McCarthy JK, Morueta-Holme N,
#' Newman EA, Park DS, Roehrdanz PR, Svenning J-C, Violle C, Wieringa JJ,
#' Wynne G, Fritz S, Strassburg BBN, Obersteiner M, Kapos V, Burgess N, Schmidt-
#' Traub G, Visconti P (2021) Areas of global importance for
#' conserving terrestrial biodiversity, carbon and water.
#' *Nature Ecology and Evolution*, 5:1499--1509.
#'
#' Polak T, Watson JEM, Fuller RA, Joseph LN, Martin TG, Possingham HP,
#' Venter O, Carwardine J (2015) Efficient expansion of global protected areas
#' requires simultaneous planning for species and ecosystems.
#' *Royal Society Open Science*, 2: 150107.
#'
#' @family targets
#'
#' @examples
#' \dontrun{
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load data
#' sim_complex_pu_raster <- get_sim_complex_pu_raster()
#' sim_complex_features <- get_sim_complex_features()
#' sim_pu_polygons <- get_sim_pu_polygons()
#'
#' # create grouping data, wherein each feature will be randomly
#' # assigned to the group A, B, C, or D
#' sim_groups <- sample(
#'   c("A", "B", "C", "D"), terra::nlyr(sim_complex_features), replace = TRUE
#' )
#'
#' # create problem where each feature in group A is assigned a 20% target,
#' # each feature in group B is assigned a target based on Jung et al. (2021),
#' # each feature in group C is assigned a target based on Ward et al. (2025),
#' # and each feature in group D is assigned a target based on Rodrigues
#' # et al. (2004)
#' p1 <-
#'   problem(sim_complex_pu_raster, sim_complex_features) %>%
#'   add_min_set_objective() %>%
#'   add_group_targets(
#'     groups = sim_groups,
#'     method = list(
#'       A = spec_relative_targets(0.2),
#'       B = spec_jung_targets(),
#'       C = spec_ward_targets(),
#'       D = spec_rodrigues_targets()
#'     )
#'   ) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s1 <- solve(p1)
#'
#' # plot solution
#' plot(s1, main = "solution", axes = FALSE)
#'
#' # here we will show how to set the feature_units when the feature
#' # are not terra::rast() objects. in this example, we have planning units
#' # stored in an sf object (i.e., sim_pu_polygons) and the feature data will
#' # be stored as columns in the sf object.
#'
#' # we will start by simulating feature data for the planning units.
#' # in particular, the simulated values will describe the amount of habitat
#' # for each feature expressed as acres (e.g., a value of 30 means 30 acres).
#' sim_pu_polygons$feature_1 <- runif(nrow(sim_pu_polygons), 0, 500)
#' sim_pu_polygons$feature_2 <- runif(nrow(sim_pu_polygons), 0, 600)
#' sim_pu_polygons$feature_3 <- runif(nrow(sim_pu_polygons), 0, 300)
#'
#' # we will now build a problem with these data and specify the
#' # the feature units as acres because that those are the units we
#' # used for simulating the data. also, we will specify targets
#' # of 2 km^2 of habitat for the first feature, and 3 km^2 for the
#' # remaining two features by assigning the features to groups.
#' # although the feature units are acres, the function is able to able
#' # to automatically convert the units.
#' p2 <-
#'   problem(
#'     sim_pu_polygons,
#'     c("feature_1", "feature_2", "feature_3"),
#'     cost_column = "cost",
#'     feature_units = "acres"
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_group_targets(
#'     groups = c("A", "B", "B"),
#'     method = list(
#'       A = spec_area_targets(targets = 2, area_units = "km^2"),
#'       B = spec_area_targets(targets = 3, area_units = "km^2")
#'     )
#'   ) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s2 <- solve(p2)
#'
#' # plot solution
#' plot(s2[, "solution_1"], axes = FALSE)
#' }
#' @name add_group_targets
NULL

#' @rdname add_group_targets
#' @export
add_group_targets <- function(x, groups, method) {
  # assert valid arguments
  assert_required(x)
  assert_required(groups)
  assert_required(method)
  assert(
    is_conservation_problem(x),
    has_single_zone(x),
    is.character(groups),
    assertthat::noNA(groups),
    is.list(method)
  )
  assert(
    isTRUE(length(groups) == x$number_of_features()),
    msg = c(
      "!" = "{.arg groups} must specify a value for each feature in {.arg x}",
      "x" = "{.arg groups} has {length(groups)} value{?s}.",
      "x" = "{.arg x} has {x$number_of_features()} value{?s}."
    )
  )
  # additional checks
  assert(
    !is.null(names(method)),
    all(nzchar(names(method))),
    msg = c(
      "!" = "Each element of {.arg method} must have a name."
    )
  )
  assert(
    all_elements_inherit(method, c("character", "TargetMethod")),
    msg = c(
      "!" = paste(
        "{.arg method} must contain a",
        "{.cls character} value or object specifying",
        "a target setting method."
      )
    )
  )
  assert(
    no_duplicates(names(method)),
    msg = c(
      "!" = "Each element of {.arg method} must refer to a different group.",
      "x" = "{.arg method} has duplicated names."
    )
  )
  assert(
    all(groups %in% names(method)),
    msg = c(
      "!" = paste(
        "Each group in {.arg groups} must have a target",
        "setting method in {.arg method}."
      ),
      "x" = paste0(
        "{.arg method} is missing the following groups: ",
        "{repr.character(setdiff(groups, names(method)))}"
      )
    )
  )
  verify(
    all(names(method) %in% groups),
    msg = c(
      "i" = paste(
        "{.arg method} has unused targets for groups",
        "not found in {.arg groups}."
      )
    )
  )

  # create list with targets
  method <- unname(method[groups])

  # verify targets processed correctly
  assert(
    all_elements_inherit(method, c("character", "TargetMethod")),
    msg = "Failed to organize targets into groups.",
    .internal = TRUE
  )

  # return targets
  internal_add_auto_targets.list(x, method = method)
}
