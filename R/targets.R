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
#' x %>% add_auto_targets(method = spec_jung_targets())
#'
#' # specify targets based on customized parameters via function
#' x %>% add_auto_targets(method = spec_jung_targets(prop_uplift = 0.05))
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
#'     spec_polak_targets(),
#'     spec_jung_targets(prop_uplift = 0.05),
#'     spec_jung_targets(prop_uplift = 0.07)
#'   )
#' )
#' ```
#'
#' The following functions can be used to specify targets in conjunction with
#' the [add_auto_targets()] function. Note that some of these functions
#' do not have default parameters for all arguments and, as such, cannot be
#' specified using their name (e.g., `method = "relative"` will
#' not work because [spec_relative_targets()] requires the user to specify
#' an argument for the `targets` parameter).
#'
#' \describe{
#'
#' \item{[spec_relative_targets()]}{
#' Specify targets as a proportion (between 0 and 1) of the total amount of each
#' feature in the the study area.
#' }
#'
#' \item{[spec_absolute_targets()]}{
#' Specify targets that denote the minimum amount of each feature required in
#' the prioritization.
#' }
#'
#' \item{[spec_interp_absolute_targets()]}{
#' Specify targets based on an interpolation procedure between thresholds
#' calculated as the sum of the features values.
#' }
#'
#' \item{[spec_interp_area_targets()]}{
#' Specify targets based on an interpolation procedure between area-based
#' thresholds.
#' }
#'
#' \item{[spec_rule_targets()]}{
#' Add targets calculated following a rule-based procedure based on a set of
#' ecological and ecosystem criteria. This is a customizable version of the
#' approach in Harris and Holness (2023).
#' }
#'
#' \item{[spec_polak_targets()]}{
#' Specify targets following Polak *et al.* (2015).
#' This method was designed to set targets for species in national-scale
#' prioritizations.
#' }
#'
#' \item{[spec_ward_targets()]}{
#' Specify targets following Watson *et al.* (2005).
#' This method was designed to set targets for species in national-scale
#' prioritizations.
#' }
#'
#' \item{[spec_watson_targets()]}{
#' Specify targets following Watson *et al.* (2010).
#' This method was designed to set targets for species in national-scale
#' prioritizations.
#' }
#'
#' \item{[spec_jung_targets()]}{
#' Specify targets following Jung *et al.* (2021).
#' This method was designed to set targets for species in global-scale
#' prioritizations.
#' }
#'
#' \item{[spec_rodrigues_targets()]}{
#' Specify targets following Rodrigues *et al.* (2004).
#' This method was designed to set targets for species in global-scale
#' prioritizations.
#' }
#'
#' \item{[spec_rl_species_targets()]}{
#' Specify targets based on criteria from the
#' International Union for the Conservation of Nature (IUCN) Red List of
#' Threatened Species (IUCN 2025).
#' }
#'
#' \item{[spec_rl_ecosystem_targets()]}{
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
#' function (e.g., [spec_relative_targets()], [spec_absolute_targets()],
#' [spec_jung_targets()]).
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
#'   list(eco = spec_polak_targets(), spp = spec_jung_targets())
#' )
#'
#' # specify target setting methods for groups with customized parameters
#' # via functions
#' x %>% add_group_targets(
#'   group = c("eco", "spp", "spp"),
#'   list(
#'     eco = spec_polak_targets(),
#'     spp = spec_jung_targets(prop_uplift = 0.05)
#'   )
#' )
#' ```
#'
#' A set of functions are available for adding targets directly to a
#' conservation planning problem.
#' These functions are especially useful when target thresholds have
#' been pre-computed, or when considering problems that have multiple zones.
#' In particular, the following functions are available.
#'
#' \describe{
#'
#' \item{[add_relative_targets()]}{
#' Add targets as a proportion (between 0 and 1) of the total amount of each
#' feature in the the study area
#' (i.e., where a value of 1 is equivalent to 100%).
#' }
#'
#' \item{[add_absolute_targets()]}{
#' Add targets that denote the minimum amount of each feature held by the
#' prioritization.
#' }
#'
#' \item{[add_manual_targets()]}{
#' Add targets by manually specifying the target setting information.
#' This function is especially useful for problems that
#' have multiple zones because it can be used to specify, for a given
#' feature, which zone -- or combination of zones -- should be considered for
#' assessing feature representation.
#' In addition to specifying targets where feature representation
#' should ideally be greater than or equal to target thresholds (i.e., as is
#' the case for all other target setting functions), this function provides
#' the functionality to specify targets where feature representation should be
#' smaller than or equal to -- or equal to -- target thresholds
#' (i.e., the constraint sense for the target).
#' }
#'
#' }
#'
#' @references
#' TODO
#'
#' @family overviews
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#' @name targets
NULL
