#' @include internal.R
NULL

#' @useDynLib prioritizr, .registration = TRUE
NULL

#' prioritizr: Systematic Conservation Prioritization in R
#'
#' The \pkg{prioritizr R} package uses mixed integer linear programming (MILP)
#' techniques to provide a flexible interface for building and solving
#' conservation planning problems (Hanson *et al.* 2024).
#' It supports a broad range of objectives, constraints, and penalties
#' that can be used to custom-tailor conservation planning problems to the
#' specific needs of a conservation planning exercise
#' (e.g., Rodrigues *et al.* 2000; Billionnet 2013). Once built, conservation
#' planning problems can be solved using a variety of commercial and
#' open-source exact algorithm solvers. In contrast to the algorithms
#' conventionally used to solve conservation problems, such as heuristics or
#' simulated annealing (Ball *et al.* 2009), the exact algorithms used
#' here are guaranteed to find optimal solutions (Schuster *et al.* 2020).
#' Furthermore, conservation problems can be constructed to optimize the
#' spatial allocation of different
#' management actions or zones, meaning that conservation practitioners can
#' identify solutions that benefit multiple stakeholders. Finally, this package
#' has the functionality to read input data formatted for the *Marxan*
#' conservation planning program (Ball *et al.* 2009), and find much
#' cheaper solutions in a much shorter period of time than *Marxan* (Beyer
#' *et al.* 2016). See the Hanson *et al.* (2024) and the
#' [online code repository](https://github.com/prioritizr/prioritizr)
#' for more information.
#'
#' @details This package contains several vignettes that are designed to
#'   showcase its functionality. To view them, please use the code
#'   `vignette("name", package = "prioritizr")` where `"name"` is the
#'   name of the desired vignette (e.g., `"gurobi_installation"`).
#'
#'   \describe{
#'
#'   \item{prioritizr}{Brief introduction to systematic conservation
#'     planning and demonstration of the main package features.
#'   }
#'
#'   \item{package_overview}{Comprehensive introduction to
#'    systematic conservation planning and detailed overview of
#'     the package features.
#'   }
#'
#'   \item{calibrating_trade-offs_tutorial}{Examples of balancing different
#'     criteria to identify candidate prioritizations.}
#'
#'   \item{connectivity_tutorial}{Examples of incorporating and evaluating
#'     connectivity in prioritizations using a range of approaches.}
#'
#'   \item{management_zones_tutorial}{Tutorial on using multiple management
#'   actions or zones to create detailed prioritizations.}
#'
#'   \item{gurobi_installation}{Instructions for installing and setting up
#'     the *Gurobi* optimization software for use with the package.}
#'
#'   \item{solver_benchmark}{Reports run times for solving
#'     conservation planning problems of varying size and complexity
#'     using different solvers.}
#'
#'   \item{publication_record}{List of publications that have cited the
#'     package.}
#'
#'   }
#'
#' @section Citation:
#' Please cite the _prioritizr R_ package when using it in publications. To
#' cite the package, please use:
#'
#' Hanson JO, Schuster R, Strimas‐Mackey M, Morrell N, Edwards BPM, Arcese P,
#' Bennett JR, and Possingham HP (2024) Systematic conservation prioritization
#' with the prioritizr R package. *Conservation Biology*,
#' In press: \doi{10.1111/cobi.14376}.
#'
#' @seealso
#' Useful links:
#' * Package website (<https://prioritizr.net>)
#' * Source code repository (<https://github.com/prioritizr/prioritizr>)
#' * Report bugs (<https://github.com/prioritizr/prioritizr/issues>)
#'
#' @author
#'  Authors:
#' * Jeffrey O Hanson \email{jeffrey.hanson@uqconnect.edu.au} ([ORCID](https://orcid.org/0000-0002-4716-6134))
#' * Richard Schuster \email{richard.schuster@glel.carleton.ca} ([ORCID](https://orcid.org/0000-0003-3191-7869), maintainer)
#' * Nina Morrell \email{nina.morrell@ubc.ca}
#' * Matthew Strimas-Mackey \email{mstrimas@gmail.com} ([ORCID](https://orcid.org/0000-0001-8929-7776))
#' * Brandon P M Edwards \email{brandonedwards3@cmail.carleton.ca} ([ORCID](https://orcid.org/0000-0003-0865-3076))
#' * Matthew E Watts \email{m.watts@uq.edu.au}
#' * Peter Arcese \email{peter.arcese@ubc.ca} ([ORCID](https://orcid.org/0000-0002-8097-482X))
#' * Joseph Bennett \email{joseph.bennett@carleton.ca} ([ORCID](https://orcid.org/0000-0002-3901-9513))
#' * Hugh P Possingham \email{hugh.possingham@tnc.org} ([ORCID](https://orcid.org/0000-0001-7755-996X))
#'
#' @references
#' Ball IR, Possingham HP, and Watts M (2009) *Marxan and relatives:
#' Software for spatial conservation prioritisation* in Spatial conservation
#' prioritisation: Quantitative methods and computational tools. Eds Moilanen
#' A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
#'
#' Beyer HL, Dujardin Y, Watts ME, and Possingham HP (2016) Solving
#' conservation planning problems with integer linear programming.
#' *Ecological Modelling*, 228: 14--22.
#'
#' Billionnet A (2013) Mathematical optimization ideas for biodiversity
#' conservation. *European Journal of Operational Research*, 231:
#' 514--534.
#'
#' Hanson JO, Schuster R, Strimas‐Mackey M, Morrell N, Edwards BPM, Arcese P,
#' Bennett JR, and Possingham HP (2024) Systematic conservation prioritization
#' with the prioritizr R package. *Conservation Biology*,
#' In press: \doi{10.1111/cobi.14376}.
#'
#' Rodrigues AS, Cerdeira OJ, and Gaston KJ (2000) Flexibility,
#' efficiency, and accountability: adapting reserve selection algorithms to
#' more complex conservation problems. *Ecography*, 23: 565--574.
#'
#' Schuster R, Hanson JO, Strimas-Mackey M, and Bennett JR (2020) Exact integer
#' linear programming solvers outperform simulated annealing for solving
#' conservation planning problems. *PeerJ* 8: e9258
#'
#' @name prioritizr
#' @docType package
#' @aliases prioritizr-package
"_PACKAGE"

# avoid false positive NOTES
#' @importFrom exactextractr exact_extract
NULL

# avoid CRAN check NOTES due to R6 classes
# see: https://github.com/r-lib/R6/issues/230
if (getRversion() >= "2.15.1")  utils::globalVariables(c("self"))
