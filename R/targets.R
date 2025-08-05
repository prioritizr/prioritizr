#' @include internal.R ConservationProblem-class.R
NULL

#' Add representation targets
#'
#' Targets are used to specify the minimum amount or proportion of a feature's
#' distribution that should (ideally) be covered (represented) by a solution.
#'
#' **Please note that most objectives require targets, and attempting
#' to solve a problem that requires targets will throw an error.**
#'
#' @details
#' A variety of functions can be used to specify targets for a
#' conservation planning problem. Below we describe them in detail.
#'
#' The [add_auto_targets()] function can be used to add targets
#' to a conservation planning problem. It provides a flexible interface
#' for specifying targets based on a single method, or a combination
#' of methods.
#' Note that this function is specifically designed for problems that have a
#' single zone, and cannot be used for problems that have multiple zones.
#' For example, given problem `x`, it could be used to specify
#' targets for all features calculated following Jung *et al.* (2021) with
#' the following code.
#'
#' ```
#' # specify targets based on default parameters via method name
#' x %>% add_auto_targets(method = "jung")
#'
#' # specify targets based on default parameters via function
#' x %>% add_auto_targets(method = jung_targets())
#'
#' # specify targets based on customized parameters via function
#' x %>% add_auto_targets(method = jung_targets(prop_uplift = 0.05))
#' ```
#'
#' Additionally, if `x` had three features -- with the first feature
#' corresponding to an ecosystem and the latter two to different species --
#' this function could be used to specify a target for the ecosystem feature
#' based on Polak *et al.* (2015) and
#' targets for the species features based on Jung *et al.* (2021) with
#' the following code.
#'
#' ```
#' # specify target setting methods for each feature with default parameters
#' # via method name
#' x %>% add_auto_targets(
#'   method = list("polak", "jung", "jung")
#' )
#'
#' # specify target setting methods for each feature with customized parameters
#' # via functions
#' x %>% add_auto_targets(
#'   method = list(
#'     polak_targets(),
#'     jung_targets(prop_uplift = 0.05),
#'     jung_targets(prop_uplift = 0.07)
#'   )
#' )
#' ```
#'
#' The following functions can be used to specify targets in conjunction with
#' the [add_auto_targets()] function. Note that some of these functions
#' do not have default parameters for all arguments and, as such, cannot be
#' specified using their name (e.g., `method = "relative"` will
#' not work because [relative_targets()] requires the user to specify
#' an argument for the `targets` parameter).
#'
#' \describe{
#'
#' \item{[relative_targets()]}{
#' Specify targets as a proportion (between 0 and 1) of the total amount of each
#' feature in the the study area.
#' }
#'
#' \item{[absolute_targets()]}{
#' Specify targets that denote the minimum amount of each feature required in
#' the prioritization.
#' }
#'
#' \item{[interpolated_absolute_targets()]}{
#' Specify targets based on an interpolation procedure between thresholds
#' calculated as the sum of the features values.
#' }
#'
#' \item{[interpolated_area_targets()]}{
#' Specify targets based on an interpolation procedure between area-based
#' thresholds.
#' }
#'
#' \item{[rule_targets()]}{
#' Add targets calculated following a rule-based procedure based on a set of
#' ecological and ecosystem criteria. This is a customizable version of the
#' approach in Harris and Holness (2023).
#' }
#'
#' \item{[polak_targets()]}{
#' Specify targets following Polak *et al.* (2015).
#' This method was designed to set targets for species in national-scale
#' prioritizations.
#' }
#'
#' \item{[ward_targets()]}{
#' Specify targets following Watson *et al.* (2005).
#' This method was designed to set targets for species in national-scale
#' prioritizations.
#' }
#'
#' \item{[watson_targets()]}{
#' Specify targets following Watson *et al.* (2010).
#' This method was designed to set targets for species in national-scale
#' prioritizations.
#' }
#'
#' \item{[jung_targets()]}{
#' Specify targets following Jung *et al.* (2021).
#' This method was designed to set targets for species in global-scale
#' prioritizations.
#' }
#'
#' \item{[rodrigues_targets()]}{
#' Specify targets following Rodrigues *et al.* (2004).
#' This method was designed to set targets for species in global-scale
#' prioritizations.
#' }
#'
#' \item{[rl_species_targets()]}{
#' Specify targets based on criteria from the
#' International Union for the Conservation of Nature (IUCN) Red List of
#' Threatened Species (IUCN 2025).
#' }
#'
#' \item{[rl_ecosystem_targets()]}{
#' Specify targets based on criteria from the
#' International Union for the Conservation of Nature (IUCN) Red List of
#' Ecosystems (IUCN 2024).
#' }
#'}
#'
#' The [add_group_targets()] function provides a convenient interface
#' for adding targets to a conservation planning problem. By assigning each
#' feature to a group and then specifying a target setting method for each
#' group, it can be used to assign targets for features based on different
#' target setting methods.
#' This function is designed to be used in conjunction with the previously
#' described functions specifying targets for the [add_auto_targets()]
#' function (e.g., [relative_targets()], [absolute_targets()],
#' [jung_targets()]).
#' Note that this function is specifically designed for problems that have a
#' single zone, and cannot be used for problems that have multiple zones.
#' For example, if the problem `x` had three features -- with the first feature
#' corresponding to an ecosystem and the latter two to different species --
#' this function could be used to set targets for the ecosystem feature based
#' on Polak *et al.* (2015) and
#' targets for the species features based on Jung *et al.* (2021) with
#' the following code.
#'
#' ```
#' # specify target setting methods for groups with default parameters
#' # via method names
#' x %>% add_group_targets(
#'   group = c("eco", "spp", "spp"),
#'   list(eco = "polak", spp = "jung")
#' )
#'
#' # specify target setting methods for groups with default parameters
#' # via functions
#' x %>% add_group_targets(
#'   group = c("eco", "spp", "spp"),
#'   list(eco = polak_targets(), spp = jung_targets())
#' )
#'
#' # specify target setting methods for groups with customized parameters
#' # via functions
#' x %>% add_group_targets(
#'   group = c("eco", "spp", "spp"),
#'   list(eco = polak_targets(), spp = jung_targets(prop_uplift = 0.05))
#' )
#' ```
#'
#' A set of functions are available for adding targets to a conservation
#' planning problem based on a particular method.
#' These functions are especially useful when all features should have
#' their targets calculated following the same method, and the default
#' parameters for the method need to be customized.
#' Note that these functions are specifically designed for problems that have a
#' single zone, and cannot be used for problems that have multiple zones.
#' For example, given problem `x`, then following code could be used to specify
#' that all features should have their targets calculated following
#' Rodrigues *et al.* (2004).
#'
#' ```
#' # specify the same target setting method for all features with default
#' # parameters via method name
#'
#' # note that this is the same as:
#' # x %>% add_auto_targets(method = rodrigues_target())
#' # x %>% add_auto_targets(method = "rodrigues")
#'
#' x %>% add_rodrigues_targets()
#'
#' # specify the same target setting method for all features with customized
#' # parameters
#'
#' # note that this is the same as:
#' # x %>% add_auto_targets(
#' #   method = rodrigues_target(cap_threshold = 1e5)
#' # )
#'
#' x %>% add_rodrigues_targets(cap_threshold = 1e5)
#' ```
#'
#' In particular, the following functions are are available.
#'
#' \describe{
#'
#' \item{[add_relative_targets()]}{
#' Add targets as a proportion (between 0 and 1) of the total amount of each
#' feature in the the study area.
#' }
#'
#' \item{[add_absolute_targets()]}{
#' Add targets that denote the minimum amount of each feature required in the
#' prioritization.
#' }
#'
#' \item{[add_interpolated_absolute_targets()]}{
#' Add targets based on an interpolation procedure between thresholds
#' calculated as the sum of the features values.
#' }
#'
#' \item{[add_interpolated_area_targets()]}{
#' Add targets based on an interpolation procedure between area-based
#' thresholds.
#' }
#'
#' \item{[add_rule_targets()]}{
#' Add targets calculated following a rule-based procedure based on a set of
#' ecological and ecosystem criteria. This is a customizable version of the
#' approach in Harris and Holness (2023).
#' }
#'
#' \item{[add_polak_targets()]}{
#' Add targets following Polak *et al.* (2015).
#' This method was designed to set targets for species in national-scale
#' prioritizations.
#' }
#'
#' \item{[add_ward_targets()]}{
#' Add targets following Watson *et al.* (2005).
#' This method was designed to set targets for species in national-scale
#' prioritizations.
#' }
#'
#' \item{[add_watson_targets()]}{
#' Add targets following Watson *et al.* (2010).
#' This method was designed to set targets for species in national-scale
#' prioritizations.
#' }
#'
#' \item{[add_jung_targets()]}{
#' Add targets following Jung *et al.* (2021).
#' This method was designed to set targets for species in global-scale
#' prioritizations.
#' }
#'
#' \item{[add_rodrigues_targets()]}{
#' Add targets following Rodrigues *et al.* (2004).
#' This method was designed to set targets for species in global-scale
#' prioritizations.
#' }
#'
#' \item{[add_rl_species_targets()]}{
#' Add targets based on criteria from the
#' International Union for the Conservation of Nature (IUCN) Red List of
#' Threatened Species (IUCN 2025).
#' }
#'
#' \item{[add_rl_ecosystem_targets()]}{
#' Add targets based on criteria from the
#' International Union for the Conservation of Nature (IUCN) Red List of
#' Ecosystems (IUCN 2024).
#' }
#'
#' }
#'
#' Finally, the [add_manual_targets()] function provides an interface
#' for adding highly customized targets to a conservation planning problem.
#' Although this function can be used with problems that have a single
#' zone, it is especially useful when setting targets for
#' problems that have multiple zones.
#' This is because it can be used to specify, for a given target, which
#' zone -- or combination of zones -- should be considered for assessing
#' feature representation.
#' Furthermore, in addition to specifying targets where feature representation
#' should ideally be greater than or equal to target thresholds (i.e., as is
#' the case for all other target setting functions), this function provides
#' the functionality to specify targets where feature representation should be
#' smaller than or equal to -- or equal to -- target thresholds
#' (i.e., the constraint sense for the target).
#'
#' \describe{
#'
#' \item{[add_manual_targets()]}{Set targets manually.}
#'
#' }
#'
#' @family overviews
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#' @name targets
NULL
