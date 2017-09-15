NULL

#' Run one of the Shiy apps for prioritizr
#'
#' Description of the prshiny function.
#'
#' @param x string for the app directory object.
#'
#' @param ... not used.
#'
#' @details
#' Need to provide details for the function.
#'
#' @return Starts a Shiny app wrapper for prioritzr functions.
#'
#' @seealso \code{\link{problem}},  \code{\link{objectives}}, \code{\link{targets}}.
#'
#' @examples
#' # list available shiny apps
#' prshiny()
#'
#' # run the basic app
#' prshiny("base_app")
#'
#' @aliases prioritizrshiny
#'
#' @name prshiny
#' @export
NULL

prshiny <- function(app) {
  # locate all the shiny apps that exist
  validapps <- list.files(system.file("shiny-apps", package = "prioritizr"))

  validappsMsg <-
    paste0(
      "Valid apps are: '",
      paste(validapps, collapse = "', '"),
      "'")

  # if an invalid app is given, throw an error
  if (missing(app) || !nzchar(app) ||
      !app %in% validapps) {
    stop(
      'Please run `prshiny()` with a valid app as an argument.\n',
      validappsMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-apps", app, package = "prioritizr")
  shiny::runApp(appDir, display.mode = "normal")
}
