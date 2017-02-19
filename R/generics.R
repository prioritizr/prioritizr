#' @include internal.R ScalarParameter-proto.R ArrayParameter-proto.R
#'   Solver-proto.R ConservationProblem-proto.R OptimizationProblem-proto.R 
NULL

#' Find intersecting units
#'
#' Determine the units in any given spatial data object that intersect
#' with any of the units in another spatial data object.
#'
#' @param x Object.
#'
#' @param y Object.
#'
#' @return the units in \code{x} that intersect with \code{y}.
#'
#' @name intersecting_units
NULL

#' @export
methods::setGeneric('intersecting_units', 
                    signature=methods::signature('x', 'y'),
                    function(x, y) 
                      standardGeneric('intersecting_units'))


#' Compile a problem
#' 
#' Compile a \code{\link{ConservationProblem}} into an
#' \code{OptimizationProblem}.
#' 
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param ... not used.
#' 
#' @return \code{OptimizationProblem} object.
#'
#' @examples
#' # build minimal conservation problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   minimum_set_objective() %>%
#'   relative_targets(0.1)
#'
#' # compile the conservation problem into an optimization problem
#' o <- compile(p)
#'
#' # print the optimization problem
#' print(o)
#'
#' @export
compile <- function(x, ...) UseMethod('compile')

#' Solve a problem
#'
#' Solve a \code{\link{ConservationProblem}} or an \code{OptimizationProblem}.
#'
#' @param a \code{\link{ConservationProblem}} or an \code{OptimizationProblem}
#'   object.
#'
#' @param b \code{\link{Solver}} object. Not need if \code{a} is an
#'   \code{\link{ConservationProblem}} object.
#'
#' @details If a \code{\link{OptimizationProblem}} is supplied, then the solution
#'   is returned as a \code{logical} showing the status of each planning unit.
#'   If a \code{\link{ConservationProblem}} is supplied, then the 
#'   solution is returned in the format of the cost data contained inside it:
#'   \itemize{
#'   \item{\code{\link[raster]{RasterLayer-class}}}{the solution is returned as a
#'      \code{\link[raster]{RasterLayer-class}} object. Cell values denote the 
#'      decision.}
#'   \item{\code{Spatial}}{the solution is returned as a 
#'      \code{\link{SpatialPolygonsDataFrame}} or
#'      \code{\link{SpatialLinesDataFrame}} with a "Solution" column containing
#'      the decision values.}
#'   }
#'
#' @return A \code{\link[raster]{RasterLayer-class}}, \code{\link[sp]{Spatial}}, or a 
#'   \code{numeric} vector containing the solution depending on the
#'   argument to \code{a}.
#'
#' @examples
#' # build minimal conservation problem
#' p <- problem(sim_pu_raster, sim_features) %>%
#'   minimum_set_objective() %>%
#'   relative_targets(0.1)
#'
#' # solve the problem
#' s <- solve(p)
#'
#' # print the solution
#' print(s)
#'
#' @name solve
#' @importFrom Matrix solve
#' @exportMethod solve
NULL

#' Show
#'
#' Show an object.
#'
#' @param object An object.
#' 
#' @return An object.
#' @name show
NULL

#' @name show
#' @rdname show
methods::setMethod('show', 'ConservationModifier',
                   function(object) object$show())

#' @name show
#' @rdname show
methods::setMethod('show', 'ConservationProblem',
                   function(object) object$show())

#' @name show
#' @rdname show
methods::setMethod('show', 'Id',
                   function(object) print(object))

#' @name show
#' @rdname show
methods::setMethod('show', 'OptimizationProblem',
                   function(object) object$show())

#' @name show
#' @rdname show
methods::setMethod('show', 'Parameter',
                   function(object) object$show())

#' @name show
#' @rdname show
methods::setMethod('show', 'Solver', function(object) object$show())

#' Print
#'
#' Print an object.
#'
#' @param x An object.
#'
#' @param ... not used.
#' 
#' @return An object.
#'
#' @name print
NULL

#' @rdname print
#' @export
print.ConservationModifier <- function(x, ...) x$print()

#' @rdname print
#' @export
print.ConservationProblem <- function(x, ...) x$print()

#' @rdname print
#' @export
print.id <- function(x, ...) message('id: ', x)

#' @rdname print
#' @export
print.OptimizationProblem <- function(x, ...) x$print()

#' @rdname print
#' @export
print.ScalarParameter <- function(x, ...) x$print()

#' @rdname print
#' @export
print.ArrayParameter <- function(x, ...) x$print()

#' @rdname print
#' @export
print.Solver <- function(x, ...) x$print()

#' Coerce object to another object
#'
#' Coerce an object.
#'
#' @param x Object.
#'
#' @param ... unused arguments.
#'
#' @return An Object.
#' 
#' @name as
NULL

#' @rdname as
#' @export
as.id <- function(x, ...) UseMethod('as.Id')

#' @rdname as
#' @export
as.Id.character <- function(x, ...) {
  class(x) <- c('Id', class(x))
  x
}

#' @rdname as
#' @export
as.list.Parameters <- function(x, ...)
  structure(lapply(x$parameters, function(x) x$value),
            .Names=sapply(x$parameters, function(x) x$name),
            id=sapply(x$parameters, function(x) x$id))

#' Is it?
#'
#' Test if an object inherits from a class.
#'
#' @param x Object.
#'
#' @return \code{logical} indicatig if it inherits from the class.
#' 
#' @name is
NULL

#' @rdname is
#' @export
is.Id <- function(x) inherits(x, 'Id')

#' @rdname is
#' @export
is.Waiver <- function(x) inherits(x, 'Waiver')


#' \code{rij} matrix
#'
#' Generate a \code{rij} matrix using spatial data sets. The \code{rij}
#' contains data on the amount of each feature in each planning unit.
#'
#' @param x \code{\link[raster]{RasterLayer-class}} or
#'   \code{\link[sp]{Spatial-class}} object representing the 
#'   planning units
#'
#' @param y \code{\link[raster]{Raster-class}} object representing the 
#'   features
#'
#' @param fun \code{function} used to summarise values. Defaults to
#'   \code{\link{sum}}. Note that this only used when \code{x} is a
#'   \code{\link[sp]{SpatialPolygons-class}} or a 
#'   \code{\link[sp]{SpatialLines-class}} object. This function must
#'   have an 'na.rm' argument.
#'
#' @param na.rm \code{logical} should missing values be omitted when
#'   performing calculations? Defaults to \code{TRUE}. Note this argument
#'   only has an effect when \code{x} is a 
#'   \code{\link[sp]{SpatialPolygons-class}} or a 
#'   \code{\link[sp]{SpatialLines-class}} and \code{velox} is \code{FALSE}.
#'
#' @param velox \code{logical} should the \code{\link[velox]{velox}}
#'   be used for geoprocessing? Defaults to \code{TRUE} if the package
#'   is installed. Note that this only used when \code{x} is a
#'   \code{\link[sp]{SpatialPolygons-class}} object.
#'
#' @param ... not used.
#'
#' @details The sparse matrix represents the spatial intersection between the
#'   planning units and the features. Rows correspond to planning units,
#'   and columns correspond to features. Values correspond to the amount
#'   of the feature in the planning unit. For example, the amount of the
#'   third species in the second planning unit would be contained in the
#'   cell in the third column and in the second column.
#'
#'   This function can take a long to run for big data sets. To reduce 
#'   processing time, the \code{\link{set_number_of_threads}} function
#'   can be used to allocate more computational resources. Additionally, 
#'   dealing with planning units represented with
#'   \code{\link[sp]{SpatialPolygonsDataFrame}} object, the 
#'   \code{\link[velox]{velox}} package can be installed to reduce 
#'   processing time.
#'
#'   Generally, processing \code{\link[sp]{Spatial-class}} data takes much
#'   longer to process then \code{\link[raster]{Raster-class}} data, and 
#'   so it is recomended to use \code{\link[raster]{Raster-class}} data
#'   for planning units where possible.
#'
#' @seealso \code{\link{set_number_of_threads}}, \code{\link[velox]{velox}}.
#' 
#' @return \code{\link{Matrix}{dgCMatrix-class}} object.
#'
#' @name rij_matrix
#'
#' @exportMethod rij_matrix
#'
#' @export
methods::setGeneric('rij_matrix', 
                    signature=methods::signature('x', 'y'),
                    function(x, y, ...) standardGeneric('rij_matrix'))


#' Boundary matrix
#'
#' Generate a boundary matrix showing the shared and exposed edges of 
#' planning units. \strong{This function assumes the 
#' data are in a projected coordinate system where distances can
#' be correctly calculated. Thus spatial data in a longitude/latitude
#' coordinate system should be reprojected prior to using this function.}
#' 
#' @param x \code{\link[raster]{Raster-class}},
#'   \code{\link[sp]{SpatialLines-class}}, or
#'   \code{\link[sp]{SpatialPolygons-class}} object. Note that if \code{x} is a 
#'   \code{\link[raster]{Raster-class}} object then it must have only one
#'   layer.
#' 
#' @param edges \code{integer} number of edges that each planning unit has when
#'   argument to \code{x} is a \code{\link[raster]{Raster-class}} object. 
#'  Defaults to 4. Valid values are 4 and 6.
#' 
#' @details This function returns a matrix. Cells along the off-diagonal 
#'   indicating the length of the shared boundary between two planning units. 
#'   Cells along the diagonal indicate length of a given planning unit's edges 
#'   that have no neighbors (eg. for edges of planning units found along the 
#'   coastline).
#'
#' @return \code{\link{Matrix}{dsCMatrix-class}} object.
#'
#' @name boundary_matrix
#'
#' @exportMethod boundary_matrix
#'
#' @export
methods::setGeneric('boundary_matrix', 
                    signature=methods::signature('x'),
                    function(x, ...) standardGeneric('boundary_matrix'))


#' Connected matrix
#'
#' Generate a matrix showing which planning units are connected.
#' 
#' @param x \code{\link[raster]{Raster-class}} or 
#'   \code{\link[sp]{Spatial-class}} object. Note that if \code{x} is a 
#'   \code{\link[raster]{Raster-class}} object then it must have only one
#'   layer.
#' 
#' @param directions \code{integer} If \code{x} is a
#'   \code{\link[raster]{Raster-class}} object, the number of directions
#'    in which cells should be connected: 4 (rook's case), 8 (queen's case), 
#'    16 (knight and one-cell queen moves), or 'bishop' to connect cells with
#'    one-cell diagonal moves. 

#' @param distance \code{numeric} If \code{x} is a \code{SpatialPoints} object,
#'   the distance that planning units have to be within in order
#'   to qualify as being connected.
#' 
#' @details This function returns a matrix. Cells along the off-diagonal 
#'   indicating if two planning units are connected. Cells along the 
#'   diagonal are zero to reduce computation burden.
#'
#' @return \code{\link{Matrix}{dsCMatrix-class}} object.
#'
#' @name connected_matrix
#'
#' @exportMethod connected_matrix
#'
#' @export
methods::setGeneric('connected_matrix', 
                    signature=methods::signature('x'),
                    function(x, ...) standardGeneric('connected_matrix'))


#' Fast extract
#'
#' Extract data from a \code{\link[raster]{Raster-class}} object from a 
#' \code{\link[sp]{Spatial-class}} object using performance enhancing tricks.
#'
#' @param x \code{\link[raster]{Raster-class}} object.
#'
#' @param y \code{\link[sp]{Spatial-class}} object.
#'
#' @param fun \code{function} to compute values.
#'
#' @param ... additional arguments passed to \code{\link[raster]{extract}}.
#'
#' @return \code{data.frame}, \code{matrix}, or \code{list} object 
#'   depending on the arguments.
#'
#' @seealso \code{\link[raster]{extract}},
#'   \code{\link[velox]{VeloxRaster_extract}}
#'
#' @name fast_extract
#'
#' @exportMethod fast_extract
#'
#' @export
methods::setGeneric('fast_extract',
                    signature=methods::signature('x', 'y'),
                    function(x, y, ...) standardGeneric('fast_extract'))

#' Add locked in constraints
#'
#' Add constraints constraints to lock in planning units
#' so that they are selected in the solution. For example, it may
#' be desirable to lock in planning units already within existing
#' protected areas so that the solutions fills in the gaps in the existing
#' reserve network.
#' 
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param locked_in Object that determines which planning units that should be 
#'   locked in. See details for more information.
#'
#' @details The locked in planning units can be specified in several 
#'   different ways:
#'
#'   \describe{
#'   
#'   \item{\code{integer} \code{vector}}{indices for which planning units should
#'     be locked in.}
#'
#'   \item{\code{character}}{column name in the attribute table with 
#'     \code{logical} values indicating if planning units should be locked in. 
#'     Note that \code{locked_in} can only a \code{character} if the planning
#'     units in \code{x} are a \code{\link{SpatialPolygonsDataFrame-class}}, 
#'     \code{\link{SpatialLinesDataFrame-class}}, or 
#'     \code{\link{SpatialPointsDataFrame-class}} object.}
#'
#'   \item{\code{\link[raster]{Raster-class}} object}{Planning units in \code{x} 
#'     that intersect with cells in \code{y} that are not equal to zero and 
#'     not \code{NA} are locked in.}
#'
#'   \item{\code{\link[sp]{Spatial-class}} object.}{planning units in \code{x} 
#'     that spatially intersect with \code{locked_in} are locked in.}
#'
#'  }
#'
#' @return \code{\link{ConservationProblem}} object.
#'
#' @examples
#' # create basic problem
#' p <- problem(sim_pu_polygons, sim_features) %>%
#'   add_minimum_set_objective() %>%
#'   add_relative_targets(0.2)
#'
#' # create problem with added locked in constraints using integers
#' p2 <- p %>% add_locked_in_constraint(which(sim_pu_polygons$locked_in))
#'
#' # create problem with added locked in constraints using a field name
#' p3 <- p %>% add_locked_in_constraint('locked_in')
#'
#' # create problem with added locked in constraints using raster data
#' p4 <- p %>% add_locked_in_constraint(sim_locked_in_raster)
#'
#' # create problem with added locked in constraints using spatial polygons data
#' locked_in <- sim_pu_polygons[sim_pu_polygons$locked_in == 1,]
#' p5 <- p %>% add_locked_in_constraint(locked_in)
#'
#' # solve problems
#' s <- stack(solve(p), solve(p2), solve(p3), solve(p4), solve(p5))
#' names(s) <- c('basic solution', 'locked in (integer input)', 
#'               'locked in (character input)', 'locked in (raster input)', 
#'               'locked in (polygons input)')
#'
#' # plot solutions
#' plot(s)
#' 
#' @seealso \code{\link{constraints}} for all the available constraints.
#'
#' @name add_locked_in_constraint
#'
#' @exportMethod add_locked_in_constraint
#'
#' @export
methods::setGeneric('add_locked_in_constraint', 
                    signature=methods::signature('x', 'locked_in'),
                    function(x, locked_in) 
                      standardGeneric('add_locked_in_constraint'))


#' Add locked out constraints
#'
#' Add constraints to ensure that certain planning units are locked out 
#' from the solution. For example, it may be useful to lock out planning 
#' units that have been substantially altered by anthropogenic development,
#' and so contain little remaining habitat.
#' 
#' @param x \code{\link{ConservationProblem}} object.
#'
#' @param locked_in Object that determines which planning units that should be 
#'   locked out. See details for more information.
#'
#' @details The locked out planning units can be specified in several 
#'   different ways:
#'
#'   \describe{
#'   
#'   \item{\code{integer} \code{vector}}{indices for which planning units should
#'     be locked out.}
#'
#'   \item{\code{character}}{column name in the attribute table with 
#'     \code{logical} values indicating if planning units should be locked out. 
#'     Note that \code{locked_out} can only a \code{character} if the planning
#'     units in \code{x} are a \code{\link{SpatialPolygonsDataFrame-class}}, 
#'     \code{\link{SpatialLinesDataFrame-class}}, or 
#'     \code{\link{SpatialPointsDataFrame-class}} object.}
#'
#'   \item{\code{\link{Raster-class}} object}{with \code{logical} cells values. 
#'     Planning units in \code{x} that spatially intersect with at least one 
#'     \code{TRUE} pixel are locked in.}
#'
#'   \item{\code{\link{Spatial-class}} object.}{planning units in \code{x} that
#'     spatially intersect with \code{locked_in} are locked in.}
#'
#'  }
#'
#' @return \code{\link{ConservationProblem}} object.
#'
#' @examples
#' # create basic problem
#' p <- problem(sim_pu_polygons, sim_features) %>%
#'   add_minimum_set_objective() %>%
#'   add_relative_targets(0.2)
#'
#' # create problem with added locked out constraints using integers
#' p2 <- p %>% add_locked_out_constraint(which(sim_pu_polygons$locked_in))
#'
#' # create problem with added locked out constraints using a field name
#' p3 <- p %>% add_locked_out_constraint('locked_out')
#'
#' # create problem with added locked out constraints using raster data
#' p4 <- p %>% add_locked_out_constraint(sim_locked_out_raster)
#'
#' # create problem with added locked out constraints using spatial polygons 
#' # data
#' locked_out <- sim_locked_in_polygons[sim_pu_polygons$locked_out == 1,]
#' p5 <- p %>% add_locked_out_constraint(locked_out)
#'
#' # solve problems
#' s <- stack(solve(p), solve(p2), solve(p3), solve(p4), solve(p5))
#' names(s) <- c('basic solution', 'locked out (integer input)', 
#'               'locked out (character input)', 'locked out (raster input)', 
#'               'locked out (polygons input)')
#'
#' # plot solutions
#' plot(s)
#' 
#' @seealso \code{\link{constraints}} for all the available constraints.
#'
#' @name add_locked_out_constraint
#'
#' @exportMethod add_locked_out_constraint
#'
#' @export
methods::setGeneric('add_locked_out_constraint', 
                    signature=methods::signature('x', 'locked_out'),
                    function(x, locked_out) 
                      standardGeneric('add_locked_out_constraint'))
