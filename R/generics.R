#' @include internal.R ScalarParameter-proto.R ArrayParameter-proto.R
#'   Solver-proto.R ConservationProblem-proto.R OptimizationProblem-proto.R 
NULL

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
#' p <- problem(sim_pu_raster, sim_features) +
#'   minimum_set_objective() +
#'   relative_targets(0.1)
#'
#' # compile the optimization problem
#' compile(p)
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
#' p <- problem(sim_pu_raster, sim_features) +
#'   minimum_set_objective() +
#'   relative_targets(0.1)
#'
#' # solve the problem
#' solve(p)
#'
#' @name solve
NULL

#' Object representation
#'
#' Generate a \code{character} representation of an object.
#'
#' @param x object.
#'
#' @param ... not used.
#' 
#' @return \code{character} object.
#'
#' @export
repr <- function(x, ...) UseMethod('repr')

#' @rdname repr
#' @export
repr.Objective <- function(x, ...) x$repr()

#' @rdname repr
#' @export
repr.Decision <- function(x, ...) x$repr()

#' @rdname repr
#' @export
repr.Parameter <- function(x, ...) x$repr()

#' @rdname repr
#' @export
repr.Parameters <- function(x, ...) x$repr()

#' @rdname repr
#' @export
repr.Constraint <- function(x, ...) x$repr()

#' @rdname repr
#' @export
repr.Constraints <- function(x, ...) x$repr()

#' @rdname repr
#' @export
repr.waiver <- function(x, ...) '(default)'

#' Show
#'
#' Show an object.
#'
#' @param object An object.
#' 
#' @return An object.
#' @name show
NULL

#' @export
methods::setMethod('show', 'ConservationModifier',
                   function(object) object$show())

#' @export
methods::setMethod('show', 'ConservationProblem',
                   function(object) object$show())

#' @export
methods::setMethod('show', 'id',
                   function(object) print(object))

#' @export
methods::setMethod('show', 'OptimizationProblem',
                   function(object) object$show())

#' @export
methods::setMethod('show', 'ScalarParameter',
                   function(object) object$show())

#' @export
methods::setMethod('show', 'ArrayParameter',
                   function(object) object$show())

#' @export
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
as.id <- function(x, ...) UseMethod('as.id')

#' @rdname as
#' @export
as.id.character <- function(x, ...) {
  class(x) <- c('id', class(x))
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
is.id <- function(x) inherits(x, 'id')

#' @rdname is
#' @export
is.waiver <- function(x) inherits(x, 'waiver')


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
#' @return \code{\link{Matrix}{dgTMatrix-class}} object.
#'
#' @name rij_matrix
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
#' @return \code{\link{Matrix}{dgTMatrix-class}} object.
#'
#' @name boundary_matrix
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
#' @return \code{\link{Matrix}{dgTMatrix-class}} object.
#'
#' @name connected_matrix
#'
#' @export
methods::setGeneric('connected_matrix', 
                    signature=methods::signature('x'),
                    function(x, ...) standardGeneric('connected_matrix'))

