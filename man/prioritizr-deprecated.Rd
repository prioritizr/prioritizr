% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{prioritizr-deprecated}
\alias{prioritizr-deprecated}
\alias{add_connected_constraints}
\alias{add_corridor_constraints}
\alias{set_number_of_threads}
\alias{get_number_of_threads}
\alias{is.parallel}
\alias{add_pool_portfolio}
\alias{connected_matrix}
\alias{feature_representation}
\alias{replacement_cost}
\alias{rarity_weighted_richness}
\alias{ferrier_score}
\alias{distribute_load}
\alias{new_optimization_problem}
\alias{predefined_optimization_problem}
\title{Deprecation notice}
\usage{
add_connected_constraints(...)

add_corridor_constraints(...)

set_number_of_threads(...)

get_number_of_threads(...)

is.parallel(...)

add_pool_portfolio(...)

connected_matrix(...)

feature_representation(...)

replacement_cost(...)

rarity_weighted_richness(...)

ferrier_score(...)

distribute_load(...)

new_optimization_problem(...)

predefined_optimization_problem(...)
}
\arguments{
\item{...}{not used.}
}
\description{
The functions listed here are deprecated.
This means that they once existed in earlier versions of the
of the \pkg{prioritizr} package, but they have since been removed
entirely, replaced by other functions, or renamed as other functions
in newer versions.
To help make it easier to transition to new versions of the \pkg{prioritizr}
package, we have listed alternatives for deprecated the functions
(where applicable).
If a function is described as being renamed, then this means
that only the name of the function has changed
(i.e., the inputs, outputs, and underlying code remain the same).
}
\details{
The following functions have been deprecated:

\describe{

\item{\code{add_connected_constraints()}}{renamed
as the \code{\link[=add_contiguity_constraints]{add_contiguity_constraints()}} function.}

\item{\code{add_corridor_constraints()}}{replaced by the
\code{\link[=add_feature_contiguity_constraints]{add_feature_contiguity_constraints()}} function.}

\item{\code{set_number_of_threads()}}{
no longer needed due to improved data extraction methods.}

\item{\code{get_number_of_threads()}}{
no longer needed due to improved data extraction methods.}

\item{\code{is.parallel()}}{
no longer needed due to improved data extraction methods.}

\item{\code{add_pool_portfolio()}}{replaced by the
\code{\link[=add_extra_portfolio]{add_extra_portfolio()}} and \code{\link[=add_top_portfolio]{add_top_portfolio()}}.}

\item{\code{connected_matrix()}}{renamed as
the \code{\link[=adjacency_matrix]{adjacency_matrix()}} function.}

\item{\code{feature_representation()}}{replaced by
the \code{\link[=eval_feature_representation_summary]{eval_feature_representation_summary()}} function for consistency with
other functions.}

\item{\code{replacement_cost()}}{renamed as
the \code{\link[=eval_replacement_importance]{eval_replacement_importance()}} function for consistency with
other functions for evaluating solutions.}

\item{\code{rarity_weighted_richness()}}{renamed as
the \code{\link[=eval_rare_richness_importance]{eval_rare_richness_importance()}} function for consistency with
other functions for evaluating solutions.}

\item{\code{ferrier_score()}}{renamed as
the \code{\link[=eval_ferrier_importance]{eval_ferrier_importance()}} function for consistency with
other functions for evaluating solutions.}

\item{\code{distribute_load()}}{has been removed because it is no longer used.
See \code{parallel::splitIndices()} for equivalent functionality.}

\item{\code{new_optimization_problem()}}{
replaced by \code{\link[=optimization_problem]{optimization_problem()}}.}

\item{\code{predefined_optimization_problem()}}{
replaced by \code{\link[=optimization_problem]{optimization_problem()}}.}

}
}
\keyword{deprecated}
