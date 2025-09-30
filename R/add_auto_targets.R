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
#' ([`Method-class`]) specifying the target setting method. See Target methods
#' below for more information.
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
#' or economically important ecosystem services under consideration.
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
#' Many of the functions for specifying target setting methods involve
#' calculating targets based on the spatial extent of the features in `x`
#' (e.g., [spec_jung_targets()], [spec_rodrigues_targets(), and others).
#' Although this function for adding targets can be readily applied to
#' [problem()] objects that
#' have the feature data provided as a [terra::rast()] object,
#' you will need to specify the spatial units for the features
#' when building a [problem()] object if the feature data
#' are provided in a different format. In particular, if the feature
#' data are provided as a `data.frame` or `character` vector,
#' then you will need to specify an argument to `feature_units` when
#' using the [problem()] function. See the Examples section below for a
#' demonstration of using the `feature_units` parameter.
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
#' # create base problem
#' p0 <-
#'   problem(sim_complex_pu_raster, sim_complex_features) %>%
#'   add_min_set_objective() %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # create problem with Polak et al. (2015) targets by using a function to
#' # specify the target setting method
#' p1 <-
#'   p0 %>%
#'   add_auto_targets(method = spec_polak_targets())
#'
#' # create problem with Polak et al. (2015) targets by using a character value
#' # to specify the target setting method
#' # (note that this yields exactly the same targets as p1, and simply
#' # offers an alternative for specifying targets with default parameters)
#' p2 <-
#'   p0 %>%
#'   add_auto_targets(method = "polak")
#'
#' # create problem with modified Polak et al. (2015) targets by using a
#' # a function to customize the parameters
#' # (note that this yields exactly the same targets as p1, and simply
#' # offers an alternative for specifying targets with default parameters)
#' p3 <-
#'   p0 %>%
#'   add_auto_targets(method = spec_polak_targets(common_relative_target = 0.3))
#'
#' # solve problems
#' s <- c(solve(p1), solve(p2), solve(p3))
#' names(s) <- c(
#'   "default Polak targets", "default Polak targets", "modified Polak targets"
#' )
#'
#' # plot solutions
#' plot(s, axes = FALSE)
#'
#' # in the previous examples, we specified the targets for each of the features
#' # based on the same target setting method. we can also specify a particular
#' # target setting method for each feature. to achieve this, we can
#' # provide the targets as a list. for example, here we will specify that
#' # the first 10 features should have their targets based on
#' # Jung et al. (2021) targets (with default parameters), the following 15
#' # features should have their targets based on Ward et al. (2025) targets
#' # (with default parameters), and all the remaining features should have
#' # their targets based on modified Jung et al. (2021) targets.
#'
#' # note that add_group_targets provides a more convenient interface for
#' # specifying targets for multiple features
#'
#' # create a list with the target setting methods
#' target_list <- append(
#'   append(
#'     rep(list(spec_jung_targets()), 10),
#'     rep(list(spec_ward_targets()), 15)
#'   ),
#'    rep(
#'     list(spec_jung_targets(prop_uplift = 0.3)),
#'     terra::nlyr(sim_complex_features) - 25
#'   )
#' )
#'
#' # create problem with the list of target setting methods
#' p4 <- p0 %>% add_auto_targets(method = target_list)
#'
#' # solve problem
#' s4 <- solve(p4)
#'
#' # plot solution
#' plot(s4, main = "solution", axes = FALSE)
#'
#' # here we will show how to set the feature_units when the feature
#' # are not terra::rast() objects. in this example, we have planning units
#' # stored in an sf object (i.e., sim_pu_polygons) and the feature data will
#' # be stored as columns in the sf object.
#'
#' # we will start by simulating feature data for the planning units.
#' # in particular, the simulated values will describe the amount of habitat
#' # for each feature expressed as acres (e.g., a value of 30 means 30 acres)
#' sim_pu_polygons$feature_1 <- runif(nrow(sim_pu_polygons), 0, 500)
#' sim_pu_polygons$feature_2 <- runif(nrow(sim_pu_polygons), 0, 600)
#' sim_pu_polygons$feature_3 <- runif(nrow(sim_pu_polygons), 0, 300)
#'
#' # we will now build a problem with these data and specify the
#' # the feature units as acres because that those are the units we
#' # used for simulating the data. also, we will specify targets
#' # of 2 km^2 of habitat for each feature. although the feature units are
#' # acres, the function is able to able to automatically convert the units.
#' p5 <-
#'   problem(
#'     sim_pu_polygons,
#'     c("feature_1", "feature_2", "feature_3"),
#'     cost_column = "cost",
#'     feature_units = "acres"
#'   ) %>%
#'   add_min_set_objective() %>%
#'   add_auto_targets(
#'     method = spec_area_targets(targets = 2, area_units = "km2")
#'   ) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' s5 <- solve(p5)
#'
#' # plot solution
#' plot(s5[, "solution_1"], axes = FALSE)
#' }
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
              "{self$name} ({type} values between ",
              "{repr.numeric(range(d$target))})"
            )
          } else {
            out <- cli::format_inline(
              "{self$name} (mixed values between ",
              "{repr.numeric(range(d$target))})"
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
            "{self$name} ({type} values between ",
            "{repr.numeric(range(d$target))})"
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
