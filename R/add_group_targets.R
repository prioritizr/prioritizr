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
#' the `character` names of target setting methods, or objects used to
#' specify target setting methods. See Target methods below for
#' more information.
#'
#' @inheritSection add_auto_targets Target setting
#' @inheritSection add_auto_targets Data calculations
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
#' method is to use a function to define an object (e.g., `jung_target()` to
#' set targets following Jung *et al.* 2021).
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
#' @inheritSection add_auto_targets Target setting
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
#' #TODO
#'
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
    all_elements_inherit(method, c("character", "Method")),
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
    all_elements_inherit(method, c("character", "Method")),
    msg = "Failed to organize targets into groups.",
    .internal = TRUE
  )

  # return targets
  internal_add_auto_targets.list(x, method = method)
}
