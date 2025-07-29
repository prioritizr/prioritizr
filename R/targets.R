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
#' The [add_auto_targets()] function can be used to add targets directly
#' to a conservation planning problem. It provides a flexible interface
#' for specifying targets based on a single method, or a combination
#' of methods. For example, it could be used to set targets
#' for all features in problem `x` based on Jung *et al.* (2021) with
#' the following code:
#' `x %>% add_auto_targets(method = jung_targets())`.
#' Additionally, if `x` had three features -- with the first feature
#' corresponding to an ecosystem and the latter two to different species --
#' this function could be used to set targets for the ecosystem feature based
#' on 30% of its geographic distribution and
#' targets for the species features based on Polak *et al.* 2015 with
#' the following code:
#' `x %>% add_auto_targets(method = list(relative_targets(0.3), jung_targets(), jung_targets())`.
#' Note that this function is specifically designed for problems with a single
#' zone, and cannot be used for multiple zones. In particular, the following
#' functions can be used to specify targets in conjunction with
#' [add_auto_targets()] function.
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
#' In addition to [add_auto_targets()], the following functions can also be
#' used to add targets directly to a conservation planning problem.
#' These functions are provided as convenient alternatives to using
#' the [add_auto_targets()] function when the targets for all features
#' should be specified following the same method. For example,
#' if the targets for all features in the problem `x` should be set following
#' Rodrigues *et al.* (2004), then this could be accomplished with
#' `x %>% add_auto_targets(method = rodrigues_targets())` or
#' `x %>% add_rodrigues_targets()`.
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
#' The [add_manual_targets()] function can be used to set customized targets.
#' In particular, it is especially useful when setting targets for
#' problems that contain multiple zones. It can also be used
#' to customize how the target threshold should be applied (i.e., the
#' constraint sense). In addition to specifying that feature representation
#' should ideally be greater than or equal to a target threshold, this function
#' can be used to specify that feature representation should ideally
#' be smaller than or equal to, or equal to, a target threshold.
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
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create base problem
#' p <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with added relative targets
#' p1 <- p %>% add_relative_targets(0.1)
#'
#' # create problem with added absolute targets
#' p2 <- p %>% add_absolute_targets(3)
#'
#' # create problem with added loglinear targets
#' p3 <- p %>% add_loglinear_targets(10, 0.9, 100, 0.2)
#'
#' # create problem with manual targets that equate to 10% relative targets
#' targs <- data.frame(
#'   feature = names(sim_features),
#'   target = 0.1,
#'   type = "relative"
#' )
#'
#' p4 <- p %>% add_manual_targets(targs)
#'
#' # solve problem
#' s <- c(solve(p1), solve(p2), solve(p3), solve(p4))
#' names(s) <- c(
#'   "relative targets", "absolute targets", "loglinear targets",
#'   "manual targets"
#' )
#' # plot solution
#' plot(s, axes = FALSE)
#' }
#' @name targets
NULL
