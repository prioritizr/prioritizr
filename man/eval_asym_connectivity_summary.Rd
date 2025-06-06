% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval_asym_connectivity_summary.R
\name{eval_asym_connectivity_summary}
\alias{eval_asym_connectivity_summary}
\alias{eval_asym_connectivity_summary,ConservationProblem,ANY,ANY,Matrix-method}
\alias{eval_asym_connectivity_summary,ConservationProblem,ANY,ANY,matrix-method}
\alias{eval_asym_connectivity_summary,ConservationProblem,ANY,ANY,dgCMatrix-method}
\alias{eval_asym_connectivity_summary,ConservationProblem,ANY,ANY,data.frame-method}
\alias{eval_asym_connectivity_summary,ConservationProblem,ANY,ANY,array-method}
\title{Evaluate asymmetric connectivity of solution}
\usage{
\S4method{eval_asym_connectivity_summary}{ConservationProblem,ANY,ANY,matrix}(x, solution, zones, data)

\S4method{eval_asym_connectivity_summary}{ConservationProblem,ANY,ANY,Matrix}(x, solution, zones, data)

\S4method{eval_asym_connectivity_summary}{ConservationProblem,ANY,ANY,data.frame}(x, solution, zones, data)

\S4method{eval_asym_connectivity_summary}{ConservationProblem,ANY,ANY,dgCMatrix}(x, solution, zones, data)

\S4method{eval_asym_connectivity_summary}{ConservationProblem,ANY,ANY,array}(x, solution, zones, data)
}
\arguments{
\item{x}{\code{\link[=problem]{problem()}} object.}

\item{solution}{\code{numeric}, \code{matrix}, \code{data.frame},
\code{\link[terra:rast]{terra::rast()}}, or \code{\link[sf:sf]{sf::sf()}} object.
The argument should be in the same format as the planning unit cost
data in the argument to \code{x}.
See the Solution format section for more information.}

\item{zones}{\code{matrix} or \code{Matrix} object describing the
level of connectivity between different zones. Each row and column
corresponds to a different zone in the argument to \code{x}, and cell
values indicate the level of connectivity between each combination
of zones. Cell values along the diagonal of the matrix represent
the level of connectivity between planning units allocated to the
same zone. Cell values must lay between 1 and -1, where negative
values favor solutions with weak connectivity. The default argument to
\code{zones} is an identity matrix (i.e., a matrix with ones along the
matrix diagonal and zeros elsewhere), so that planning units are
only considered to be connected when they are allocated to the same zone.
This argument is required when working with multiple zones and the
argument to \code{data} is a \code{matrix} or \code{Matrix} object.
If the argument to \code{data} is an \code{array} or \code{data.frame} with data for
multiple zones (e.g., using the \code{"zone1"} and \code{"zone2"} column names),
this argument must explicitly be set to \code{NULL} otherwise an error will be
thrown.}

\item{data}{\code{matrix}, \code{Matrix}, \code{data.frame}, or
\code{array} object containing connectivity data. The connectivity values
correspond to the strength of connectivity between
different planning units. Thus connections between planning units
that are associated with higher values are more favorable in the solution.
See the Data format section for more information.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}} object describing the connectivity of the
solution.
It contains the following columns:

\describe{

\item{summary}{\code{character} description of the summary statistic.
The statistic associated with the \code{"overall"} value
in this column is calculated using the entire solution
(including all management zones if there are multiple zones).
If multiple management zones are present, then summary statistics
are also provided for each zone separately
(indicated using zone names).}

\item{asym_connectivity}{\code{numeric} connectivity value.
Greater values correspond to solutions associated with greater
connectivity.
Thus conservation planning exercises typically prefer solutions
with greater values.}

}
}
\description{
Calculate the connectivity held within a solution to a conservation
planning problem.
This summary statistic evaluates the connectivity of a solution using
pair-wise connectivity values between combinations of planning units.
It is specifically designed for asymmetric connectivity data.
}
\details{
This summary statistic is comparable to the \code{Connectivity} metric
reported by the
\href{https://marxansolutions.org}{\emph{Marxan} software} (Ball \emph{et al.} 2009).
It is calculated using the same equations used to penalize solutions
with asymmetric connectivity data
(i.e., \code{\link[=add_asym_connectivity_penalties]{add_asym_connectivity_penalties()}}).
Specifically, it is calculated as the sum of the connectivity
values (in the argument to \code{data}) that correspond pairs of planning
units, wherein one planning unit is selected by the solution
and the other planning unit is not selected by solution.
}
\section{Solution format}{

Broadly speaking, the argument to \code{solution} must be in the same format as
the planning unit data in the argument to \code{x}.
Further details on the correct format are listed separately
for each of the different planning unit data formats:

\describe{

\item{\code{x} has \code{numeric} planning units}{The argument to \code{solution} must be a
\code{numeric} vector with each element corresponding to a different planning
unit. It should have the same number of planning units as those
in the argument to \code{x}. Additionally, any planning units missing
cost (\code{NA}) values should also have missing (\code{NA}) values in the
argument to \code{solution}.
}

\item{\code{x} has \code{matrix} planning units}{The argument to \code{solution} must be a
\code{matrix} vector with each row corresponding to a different planning
unit, and each column correspond to a different management zone.
It should have the same number of planning units and zones
as those in the argument to \code{x}. Additionally, any planning units
missing cost (\code{NA}) values for a particular zone should also have a
missing (\code{NA}) values in the argument to \code{solution}.
}

\item{\code{x} has \code{\link[terra:rast]{terra::rast()}} planning units}{The argument to \code{solution}
be a \code{\link[terra:rast]{terra::rast()}} object where different cells correspond
to different planning units and layers correspond to
a different management zones. It should have the same dimensionality
(rows, columns, layers), resolution, extent, and coordinate reference
system as the planning units in the argument to \code{x}. Additionally,
any planning units missing cost (\code{NA}) values for a particular zone
should also have missing (\code{NA})  values in the argument to \code{solution}.
}

\item{\code{x} has \code{data.frame} planning units}{The argument to \code{solution} must
be a \code{data.frame} with each column corresponding to a different zone,
each row corresponding to a different planning unit, and cell values
corresponding to the solution value. This means that if a \code{data.frame}
object containing the solution also contains additional columns, then
these columns will need to be subsetted prior to using this function
(see below for example with \code{\link[sf:sf]{sf::sf()}} data).
Additionally, any planning units missing cost
(\code{NA}) values for a particular zone should also have missing (\code{NA})
values in the argument to \code{solution}.
}

\item{\code{x} has \code{\link[sf:sf]{sf::sf()}} planning units}{The argument to \code{solution} must be
a \code{\link[sf:sf]{sf::sf()}} object with each column corresponding to a different
zone, each row corresponding to a different planning unit, and cell values
corresponding to the solution value. This means that if the
\code{\link[sf:sf]{sf::sf()}} object containing the solution also contains additional
columns, then these columns will need to be subsetted prior to using this
function (see below for example).
Additionally, the argument to \code{solution} must also have the same
coordinate reference system as the planning unit data.
Furthermore, any planning units missing cost
(\code{NA}) values for a particular zone should also have missing (\code{NA})
values in the argument to \code{solution}.
}
}
}

\section{Data format}{

The argument to \code{data} can be specified using several different formats.

\describe{

\item{\code{data} as a \code{matrix}/\code{Matrix} object}{where rows and columns represent
different planning units and the value of each cell represents the
strength of connectivity between two different planning units. Cells
that occur along the matrix diagonal are treated as weights which
indicate that planning units are more desirable in the solution.
The argument to \code{zones} can be used to control
the strength of connectivity between planning units in different zones.
The default argument for \code{zones} is to treat planning units
allocated to different zones as having zero connectivity.}

\item{\code{data} as a \code{data.frame} object}{containing columns that are named
\code{"id1"}, \code{"id2"}, and \code{"boundary"}. Here, each row
denotes the connectivity between a pair of planning units
(per values in the \code{"id1"} and \code{"id2"} columns) following the
\emph{Marxan} format.
If the argument to \code{x} contains multiple zones, then the
\code{"zone1"} and \code{"zone2"} columns can optionally be provided to manually
specify the connectivity values between planning units when they are
allocated to specific zones. If the \code{"zone1"} and
\code{"zone2"} columns are present, then the argument to \code{zones} must be
\code{NULL}.}

\item{\code{data} as an \code{array} object}{
containing four-dimensions where cell values
indicate the strength of connectivity between planning units
when they are assigned to specific management zones. The first two
dimensions (i.e., rows and columns) indicate the strength of
connectivity between different planning units and the second two
dimensions indicate the different management zones. Thus
the \code{data[1, 2, 3, 4]} indicates the strength of
connectivity between planning unit 1 and planning unit 2 when planning
unit 1 is assigned to zone 3 and planning unit 2 is assigned to zone 4.}

}
}

\examples{
\dontrun{
# set seed for reproducibility
set.seed(500)

# load data
sim_pu_polygons <- get_sim_pu_polygons()
sim_features <- get_sim_features()
sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
sim_zones_features <- get_sim_zones_features()

# build minimal conservation problem with polygon data
p1 <-
  problem(sim_pu_polygons, sim_features, cost_column = "cost") \%>\%
  add_min_set_objective() \%>\%
  add_relative_targets(0.1) \%>\%
  add_binary_decisions() \%>\%
  add_default_solver(verbose = FALSE)

# solve the problem
s1 <- solve(p1)

# print solution
print(s1)

# plot solution
plot(s1[, "solution_1"])

# simulate connectivity matrix
# here, we will generate connectivity values randomly
# between all pairs of planning units
acm1 <- matrix(
  runif(nrow(sim_pu_polygons) ^ 2),
  nrow = nrow(sim_pu_polygons)
)

# calculate connectivity associated with the solution
r1 <- eval_asym_connectivity_summary(p1, s1[, "solution_1"], data = acm1)
print(r1)

# build multi-zone conservation problem with polygon data
p2 <-
  problem(
    sim_zones_pu_polygons, sim_zones_features,
     cost_column = c("cost_1", "cost_2", "cost_3")
  ) \%>\%
  add_min_set_objective() \%>\%
  add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) \%>\%
  add_binary_decisions() \%>\%
  add_default_solver(verbose = FALSE)

# solve the problem
s2 <- solve(p2)

# print solution
print(s2)

# create new column representing the zone id that each planning unit
# was allocated to in the solution
s2$solution <- category_vector(
  s2[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
)
s2$solution <- factor(s2$solution)

# plot solution
plot(s2[, "solution"])

# simulate asymmetric connectivity matrix
acm2 <- matrix(
  runif(nrow(sim_zones_pu_polygons) ^ 2),
  nrow = nrow(sim_zones_pu_polygons)
)

# calculate connectivity associated with the solution
r2 <- eval_asym_connectivity_summary(
  p2,
  s2[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")],
  data = acm2
)
print(r2)

}

}
\references{
Ball IR, Possingham HP, and Watts M (2009) \emph{Marxan and relatives:
Software for spatial conservation prioritisation} in Spatial conservation
prioritisation: Quantitative methods and computational tools. Eds Moilanen
A, Wilson KA, and Possingham HP. Oxford University Press, Oxford, UK.
}
\seealso{
See \link{summaries} for an overview of all functions for summarizing solutions.
Also, see \code{\link[=add_asym_connectivity_penalties]{add_asym_connectivity_penalties()}} to penalize solutions with low
asymmetric connectivity.

Other summaries: 
\code{\link{eval_boundary_summary}()},
\code{\link{eval_connectivity_summary}()},
\code{\link{eval_cost_summary}()},
\code{\link{eval_feature_representation_summary}()},
\code{\link{eval_n_summary}()},
\code{\link{eval_target_coverage_summary}()}
}
\concept{summaries}
