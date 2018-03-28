 #' @include internal.R ConservationProblem-proto.R generics.R
NULL

#' Conservation problem methods
#'
#' These functions are used to access data from an
#' \code{\link{ConservationProblem-class}} object.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @details The functions return the following data:
#'
#' \describe{
#'
#' \item{number_of_planning_units}{\code{integer} number of planning units in
#'   the problem.}
#'
#' \item{number_of_planning_units}{\code{integer} number of total units in
#'   the problem.}
#'
#' \item{number_of_features}{\code{integer} number of features in the problem.}
#'
#' \item{number_of_zones}{\code{integer} number of zones in the problem.}
#'
#' \item{feature_names}{\code{character} names of features in the problem.}
#'
#' \item{zone_names}{\code{character} names of zones in the problem.}
#'
#' }
#'
#' @return \code{integer} number or \code{character} depending on the function.
#'
#' @name ConservationProblem-methods
#'
#' @aliases number_of_features,ConservationProblem-method number_of_planning_units,ConservationProblem-method number_of_total_units,ConservationProblem-method number_of_zones,ConservationProblem-method feature_names,ConservationProblem-method zone_names,ConservationProblem-method
NULL

#' @name ConservationProblem-methods
#'
#' @rdname ConservationProblem-methods
#'
#' @usage \S4method{number_of_features}{ConservationProblem}(x)
#'
methods::setMethod("number_of_features", "ConservationProblem",
  function(x) x$number_of_features())

#' @name ConservationProblem-methods
#'
#' @rdname ConservationProblem-methods
#'
#' @usage \S4method{number_of_zones}{ConservationProblem}(x)
#'
methods::setMethod("number_of_zones", "ConservationProblem",
  function(x) x$number_of_zones())

#' @name ConservationProblem-methods
#'
#' @rdname ConservationProblem-methods
#'
#' @usage \S4method{number_of_planning_units}{ConservationProblem}(x)
#'
methods::setMethod("number_of_planning_units", "ConservationProblem",
  function(x) x$number_of_planning_units())

#' @name ConservationProblem-methods
#'
#' @rdname ConservationProblem-methods
#'
#' @usage \S4method{number_of_total_units}{ConservationProblem}(x)
#'
methods::setMethod("number_of_total_units", "ConservationProblem",
  function(x) x$number_of_total_units())

#' @name ConservationProblem-methods
#'
#' @rdname ConservationProblem-methods
#'
#' @usage \S4method{feature_names}{ConservationProblem}(x)
#'
methods::setMethod("feature_names", "ConservationProblem",
  function(x) x$feature_names())

#' @name ConservationProblem-methods
#'
#' @rdname ConservationProblem-methods
#'
#' @usage \S4method{zone_names}{ConservationProblem}(x)
#'
methods::setMethod("zone_names", "ConservationProblem",
  function(x) x$zone_names())
