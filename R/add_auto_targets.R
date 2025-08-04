#' @include internal.R ConservationProblem-class.R target_optimization_format.R get_target_method.R
NULL

#' Add targets automatically
#'
#' Add targets to a conservation planning problem based on a particular
#' method for setting targets.
#'
#' @inheritParams add_manual_targets
#'
#' @param method `character` value, `character` vector, `list`, or object
#' specifying the target setting method. See Target methods below for
#' more information.
#'
#' @section Target setting:
#' Many conservation planning problems require targets.
#' Targets are used to specify the minimum amount, or proportion, of a
#' feature's spatial distribution that should ideally be protected.
#' This is important so that the optimization process can weigh the merits
#' and trade-offs between improving the representation of one feature over
#' another feature.
#' Although it can be challenging to set meaningful targets,
#' this is a critical step for ensuring that prioritizations
#' meet the stakeholder objectives that underpin a prioritization exercise
#' (Carwardine *et al.* 2009).
#' In other words, targets play an important role in ensuring that a
#' priority setting process is properly tuned according to stakeholder
#' requirements.
#' For example, targets provide a mechanism for ensuring that a
#' prioritization secures enough habitat to promote the long-term persistence
#' of each threatened species, culturally important species,
#' or economically important ecosystem service under consideration.
#' Since there is often uncertainty regarding stakeholder objectives
#' (e.g., how much habitat should be protected for a given species)
#' or the influence of particular target on a prioritization
#' (e.g., how would setting a 90% or 100% for a threatened species alter
#' priorities), it is often useful to generate and compare a suite of
#' prioritizations based on different target scenarios.
#'
#' @section Target methods:
#' A variety of options are available for specifying target setting methods.
#'
#' \describe{
#'
#' \item{`method` is a `character` value}{
#' This option involves specifying a target setting method based on its name.
#' It is particularly useful when all features should be assigned targets
#' based on the same method, and the method should rely
#' on default parameters. For example, if `x` was a [problem()],
#' then the following code could be used
#' to specify that all features should have their targets calculated based on
#' Jung *et al.* (2021) with default parameters.
#'
#' ```
#' x %>% add_auto_targets(method = "jung"))
#' ```
#
#' ```{r child = "man/rmd/target_method_character.md"}
#' ```
#' }
#'
#' \item{`method` is a `character` vector}{
#' This option involves specifying a target setting method for each feature
#' based on the name of the method.
#' It is particularly useful when particular features should be assigned
#' targets based on different methods, and all methods
#' rely on default parameters.
#' For example, if `x` was a [problem()] with three features,
#' then the following code
#' could be used to specify a target based on Jung *et al.* (2021) for the
#' first feature, target based on Polak *et al.*  (2015) for the second
#' feature, and target based on Jung *et al.* (2021) for the third feature
#' (note that all targets are based on default parameters).
#'
#' ```
#' x %>% add_auto_targets(method = c("jung", "polak", "jung"))
#' ```
#'
#' Note that `method` must specify a value for
#' each feature in `x`, and the order of the method values should follow
#' the order of the features in `x` (i.e, per `feature_names(x)`).
#' For details on the available `method` values, please refer to the
#' first option where `method` is a `character` value.
#' }
#'
#' \item{`method` is an object for specifying a method}{
#' This option involves specifying a target setting method based on
#' an object. It is particularly useful when all features should be assigned
#' targets based on the same method, and the parameters for
#' calculating the targets should be customized.
#' For example, if `x` was a [problem()], then the following could could be used
#' to specify that all features should have their targets calculated based on
#' Jung *et al.* (2021) with an uplift of 5%.
#'
#' ```
#' x %>% add_auto_targets(method = jung_targets(prop_uplift = 0.05))
#' ```
#'
#' ```{r child = "man/rmd/target_method_objects.md"}
#' ```
#' }
#'
#' \item{`method` is a `list`}{
#' This option involves specify a target setting method based on a `list`
#' of objects. It is particularly useful when particular features
#' should be assigned different targets based on different methods,
#' and at least one of the methods requires customized parameters.
#' For example, if `x` was a [problem()] with three features, then the
#' following code could be used to specify
#' (i) a target for the first feature based on Jung *et al.* (2021) with
#' customized parameters (i.e, `jung_targets(prop_uplift = 0.05)`),
#' (ii) a target for the second feature based on Polak *et al.* (2015) with
#' default parameters via the name method name (i.e., `"polak"`),
#' and (iii) a target based on Jung *et al.* (2021) with default parameters
#' via an object (i.e., `jung_targets()`).
#'
#' ```
#' x %>% add_auto_targets(
#'   method = list(jung_targets(prop_uplift = 0.05), "polak", jung_targets())
#' )
#' ```
#'
#' Note that `method` must specify a value for
#' each feature in `x`, and the order of the method values should follow
#' the order of the features in `x` (i.e, per `feature_names(x)`).
#' For details on the available `method` values, please refer to the
#' previous options.
#' }
#'
#' }
#'
#' @section Data calculations:
#' This function involves calculating targets based on the spatial extent
#' of the features in `x`.
#' As such, this function may need to make particular assumptions depending
#' on the data used to specify the planning units and features in `x`.
#' If possible, when building `x` with [problem()], it is recommended to specify
#' `features` as a [terra::rast()] object to ensure correctness.
#' Although this function can be used to specify targets for features
#' in other data formats, you must take care to ensure that the information
#' specifying the amount of each feature are in units of 1 km
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' To help ensure that you are aware of this limitation, this function will
#' produce a warning to alert you when the features in `x` belong to such
#' data formats.
#' If you are confident that your data are in the correct units,
#' then you can safely ignore this warning.
#' Below we provide details for ensuring that the targets are calculated
#' correctly.
#'
#' \describe{
#' \item{`x` has [terra::rast()] or [sf::st_sf()] planning units and [terra::rast()] features}{
#' This function will automatically ensure that the targets are calculated
#' correctly. Given this, it is recommended to specify `features` in either
#' of these when building a [problem()].
#' }
#'
#' \item{`x` has [sf::st_sf()] or `data.frame` planning units and `character` features}{
#' Here, the argument to `features` of [problem()] is used to specify
#' which columns of the planning unit data denote the amount of each feature
#' present within each planning unit.
#' All of these columns must contain values in
#' units of 1 km \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' For example, if `features = c("spp1", "spp2", "spp3")`, then the columns
#' `"spp1"`, `"spp2"`, and `"spp3"` of the planning unit denote data the amount
#' of the first, second, and third features (respectively) present within each
#' planning unit. If the column `"spp1"` has a value of 5 in the first row
#' of the planning unit data, then this means the first planning unit
#' contains 5 km \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}
#' of habitat for the feature `"spp1"`.
#' }
#'
#' \item{`x` has `data.frame` planning units and `data.frame` features}{
#' The argument to `features` of [problem()] is used to specify a
#' unique identifier and name for each of the features.
#' Additionally, the argument to `rij` of
#' [problem()] is used to specify the amount of each feature present within
#' each planning unit. The argument to `rij` must contain a `data.frame`
#' with the columns `"pu"`, `"species"`, `"amount"`. For a given row,
#' the `"pu"` column contains an `integer` value denoting the identifier
#' for the planning unit,
#' the `"species"` column contains an `integer` value denoting the
#' identifier for the feature,
#' and `"amount" contains a `numeric` value denoting the amount of the
#' the feature (per the `"species"` column) that is present in the
#' planning unit (per `"pu"` column).
#' Given this, the "amount" column must contain values that in units of 1 km
#' \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' For example, if the argument to `rij` has a row where the `"pu"` column
#' has a value of 1, the `"species"` column has a value of 2, and the
#' `"amount"` column has a value of 3: then this means that the
#' planning unit with an identifier of 1 contains 3
#' km \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}} of the feature
#' associated with the identifier of 2.
#' }
#'
#' \item{`x` has `numeric` or `matrix` planning units and `data.frame` features}{
#' The argument to `features` of [problem()] is used to specify a
#' unique identifier and name for each of the features.
#' Additionally, the argument to `rij_matrix` of
#' [problem()] is used to specify the amount of each feature present within
#' each planning unit. The argument to `rij_matrix` must contain a `matrix`
#' (or `list` of `matrix` objects) that contains `numeric` values in units of 1
#' km \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}.
#' }
#' }
#'
#' @inherit add_relative_targets return seealso
#'
#' @family targets
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
#' IUCN (2025) The IUCN Red List of Threatened Species. Version 2025-1.
#' Available at <https://www.iucnredlist.org>. Accessed on 23 July 2025.
#'
#' IUCN (2024). Guidelines for the application of IUCN Red List of Ecosystems
#' Categories and Criteria, Version 2.0. Keith DA, Ferrer-Paris JR,
#' Ghoraba SMM, Henriksen S, Monyeki M, Murray NJ, Nicholson E, Rowland J,
#' Skowno A, Slingsby JA, Storeng AB, Valderrábano M, Zager I
#' (Eds.). Gland, Switzerland: IUCN.
#'
#' Polak T, Watson JEM, Fuller RA, Joseph LN, Martin TG, Possingham HP,
#' Venter O, Carwardine J (2015) Efficient expansion of global protected areas
#' requires simultaneous planning for species and ecosystems.
#' *Royal Society Open Science*, 2: 150107.
#'
#' Ward M, Possingham HP, Wintle BA, Woinarski JCZ, Marsh JR, Chapple DG,
#' Lintermans M, Scheele BC, Whiterod NS, Hoskin CJ, Aska B, Yong C, Tulloch A,
#' Stewart R, Watson JEM (2025) The estimated cost of preventing extinction and
#' progressing recovery for Australia's priority threatened species.
#' *Proceedings of the National Academy of Sciences*, 122: e2414985122.
#'
#' Watson JEM, Evans MC, Carwardine J, Fuller RA, Joseph LN, Segan DB,
#' Taylor MFJ, Fensham RJ, Possingham HP (2010) The capacity of Australia's
#' protected-area system to represent threatened species.
#' *Conservation Biology*,25: 324--332.
#' @examples
#' #TODO
#'
#' @name add_auto_targets
#'
#' @aliases add_auto_targets,ConservationProblem,character-method add_auto_targets,ConservationProblem,Method-method add_auto_targets,ConservationProblem,list-method
NULL

#' @export
methods::setGeneric("add_auto_targets",
  signature = methods::signature("x", "method"),
  function(x, method) {
    assert_required(x)
    assert_required(method)
    assert(
      is_conservation_problem(x)
    )
    assert(
      is_inherits(
        method,
        c("character", "Method", "list")
      ),
      msg = paste(
        "{.arg method} must be a {.cls character} value,",
        "{.cls character} vector, {.cls list}, or object specifying",
        "a target method."
      )
    )
    standardGeneric("add_auto_targets")
  }
)

#' @name add_auto_targets
#' @usage \S4method{add_auto_targets}{ConservationProblem,character}(x, method)
#' @rdname add_auto_targets
methods::setMethod("add_auto_targets",
  methods::signature("ConservationProblem", "character"),
  function(x, method) {
    # assert valid arguments
    assert(
      assertthat::is.string(method),
      assertthat::noNA(method)
    )
    # verify that method has a valid value
    ## note this will throw an error if method is not valid
    m <- get_target_method(method)
    assert(inherits(m, "Method"), .internal = TRUE)
    # add method
    add_auto_targets(x, m)
  }
)

#' @name add_auto_targets
#' @usage \S4method{add_auto_targets}{ConservationProblem,list}(x, method)
#' @rdname add_auto_targets
methods::setMethod("add_auto_targets",
  methods::signature("ConservationProblem", "list"),
  function(x, method) {
    # assert arguments are valid
    assert(
      is_conservation_problem(x),
      is.list(method)
    )
    assert(
      isTRUE(length(method) == x$number_of_features()),
      msg = c(
        "!" =
          "{.arg method} must specify a method for each feature in {.arg x}.",
        "x" = "{.arg x} has {.val {x$number_of_features()}} features.",
        "x" = "{.arg method} has {.val {length(method)}} elements."
      )
    )
    # add targets
    internal_add_auto_targets.list(x, method)
  }
)

internal_add_auto_targets.list <- function(x, method, call = fn_caller_env()) {
  # assert valid arguments
  assert(
    is_conservation_problem(x),
    is.list(method),
    call = call,
    .internal = TRUE
  )
  assert(
    all_elements_inherit(method, c("character", "Method")),
    msg = c(
      "!" = paste(
        "All {.arg ...} arguments must contain a",
        "{.cls character} value or object specifying",
        "a target setting method."
      )
    ),
    call = call
  )

  # if method contains any characters, then override with Method
  method <- lapply(method, function(x) {
    if (is.character(x)) {
      return(get_target_method(x, call = call))
    }
    x
  })

  # create data frame with targets
  targets <- tibble::tibble(
    feature = x$feature_names(),
    target = vapply(
      seq_len(x$number_of_features()),
      FUN.VALUE = numeric(1),
      function(i) {
        method[[i]]$calculate_targets(x = x, features = i, call = call)
      }
    ),
    type = vapply(method, FUN.VALUE = character(1), `[[`, "type")
  )

  # determine target names
  target_name <- unique(vapply(method, FUN.VALUE = character(1), `[[`, "name"))
  if (length(target_name) > 1) {
    target_name <- "Multiple method targets"
  }

  # add targets to problem
  x$add_targets(
    R6::R6Class(
      "AutoTargets",
      inherit = Target,
      public = list(
        name = target_name,
        data = list(targets = targets, type = method$type),
        repr = function(compact = TRUE) {
          d <- self$get_data("targets")
          type <- unique(d$type)
          if (identical(length(type), 1L)) {
            out <- cli::format_inline(
              "{self$name} ({type} values between {.val {range(d$target)}})"
            )
          } else {
            out <- cli::format_inline(
              "{self$name} (mixed values between {.val {range(d$target)}})"
            )
          }
          out
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

#' @name add_auto_targets
#' @usage \S4method{add_auto_targets}{ConservationProblem,Method}(x, method)
#' @rdname add_auto_targets
methods::setMethod("add_auto_targets",
  methods::signature("ConservationProblem", "Method"),
  function(x, method) {
    # assert arguments are valid
    assert(
      is_conservation_problem(x),
      inherits(method, "Method")
    )
    # add targets
    internal_add_auto_targets.Method(x, method)
  }
)

internal_add_auto_targets.Method <- function(x, method,
                                             call = fn_caller_env()) {
  # assert valid arguments
  assert(
    is_conservation_problem(x),
    inherits(method, "Method"),
    call = call,
    .internal = TRUE
  )

  # create data frame with targets
  targets <- tibble::tibble(
    feature = x$feature_names(),
    target = method$calculate_targets(
      x = x,
      features = seq_len(x$number_of_features()),
      call = call
    ),
    type = method$type
  )

  # add targets to problem
  x$add_targets(
    R6::R6Class(
      "AutoTargets",
      inherit = Target,
      public = list(
        name = method$name,
        data = list(targets = targets),
        repr = function(compact = TRUE) {
          d <- self$get_data("targets")
          type <- d$type[[1]]
          cli::format_inline(
            "{self$name} ({type} values between {.val {range(d$target)}})"
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
