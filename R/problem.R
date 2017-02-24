#' @include internal.R ConservationProblem-proto.R
NULL

#' Conservation planning problem
#'
#' Create a systematic conservation planning problem. This function is used to
#' specify the basic data used in a spatial prioritization problem: the 
#' spatial distribution of the planning units and their costs, as well as 
#' the features (eg. species, ecosystems) that need to be conserved. After 
#' constructing this object, it can be customized to meet specific 
#' objectives using targets (see \code{\link{targets}}) and constraints 
#' (see \code{\link{constraints}}).
#'
#' @param x \code{\link[raster]{Raster-class}}, 
#'   \code{\link[sp]{SpatialPolygonsDataFrame-class}}, or
#'   \code{\link[sp]{SpatialLinesDataFrame-class}} object specifying the planning
#'   units to use in the reserve design exercise and their corresponding cost.
#'   It may be desirable to exlcude some planning units from the analysis, for
#'   example those outside the study area. To exclude planning units, set the
#'   cost for those raster cells to \code{NA}.
#'
#' @param features \code{\link[raster]{Raster-class}} object showing the 
#'   distribution of conservation features. Missing values (i.e. \code{NA}s) 
#'   can be used to indicate the absence of a feature in a particular cell 
#'   instead of explicitly setting these cells to zero.
#'
#' @param cost_column \code{character} name or \code{integer} indicating the 
#'   column in the attribute table of a \code{\link[sp]{Spatial-class}} object 
#'   with the cost data.
#'
#' @param ... additional arguments.
#'
#' @return A \code{\link{ConservationProblem-class}} object containing the 
#'   basic data used to build a prioritization problem.
#'
#' @seealso \code{\link{constraints}}, \code{\link{objectives}},
#'  \code{\link{targets}}.
#'
#' @examples
#' # create problem using raster planning unit data
#' problem(sim_pu_raster, sim_features)
#'
#' # create problem using polygon planning unit data
#' problem(sim_pu_polygons, sim_features)
#'
#' # create problem using line planning unit data
#' problem(sim_pu_lines, sim_features)
#'
#' # create problem using point planning unit data
#' problem(sim_pu_points, sim_features)
#'
#' @export
problem <- function(x, features, ...) UseMethod('problem')

#' @rdname problem
#' @method problem Raster
#' @export
problem.Raster <- function(x, features, ...) {
  assertthat::assert_that(inherits(x, 'Raster'), inherits(features, 'Raster'))
  assertthat::assert_that(isTRUE(raster::cellStats(x, 'min') > 0),
    isTRUE(all(raster::cellStats(features, 'max') > 0)),
    raster::nlayers(x) == 1, raster::nlayers(features) >= 1,
    raster::compareRaster(x, features, res=TRUE, tolerance=1e-5,
      stopiffalse=FALSE))
  if (inherits(x, c('RasterStack', 'RasterBrick')))
    x <- x[[1]]
  pproto(NULL, ConservationProblem,
    constraints=pproto(NULL, Collection), penalties=pproto(NULL, Collection),
    data=list(cost=x, features=features, rij_matrix=rij_matrix(x, features))) 
}

#' @rdname problem
#' @method problem Spatial
#' @export
problem.Spatial <- function(x, features, cost_column = names(x)[1], ...) {
  assertthat::assert_that(inherits(x, c('SpatialPolygonsDataFrame',
    'SpatialLinesDataFrame', 'SpatialPointsDataFrame')))
  cost_column <- match.arg(cost_column, names(x))
  x <- x[is.finite(x[[cost_column]]),]
  assertthat::assert_that(
    isTRUE(all(x[[1]] > 0)),
    isTRUE(all(raster::cellStats(features, 'max', na.rm=TRUE) > 0)),
    raster::nlayers(features) >= 1,
    raster::compareCRS(x@proj4string, features@crs),
    isTRUE(rgeos::gIntersects(methods::as(raster::extent(x), 'SpatialPolygons'),
      methods::as(raster::extent(features), 'SpatialPolygons'))))
  pproto(NULL, ConservationProblem,
    constraints=pproto(NULL, Collection), penalties=pproto(NULL, Collection),
    data=list(cost=x, features=features, cost_column = cost_column,
      rij_matrix=rij_matrix(x[,cost_column], features)))
}

#' @rdname problem
#' @method problem data.frame
#' @export
problem.data.frame <- function(x, feature_data, rij_data, ....) {
  # assert that arguments are valid
  assertthat::assert_that(
    # inputs are data.frames
    inherits(x, 'data.frame'),inherits(feature_data, 'data.frame'),
    inherits(rij_data, 'data.frame'),
    # x$cost
    assertthat::has_name(x, 'cost'), is.numeric(x$cost), all(is.finite(x$cost)),
    # x$id
    assertthat::has_name(x, 'id'), is.numeric(x$id), all(is.finite(x$id)),
    anyDuplicated(x$id)==0,
    # feature_data$id
    assertthat::has_name(feature_data, 'id'), is.numeric(feature_data$id),
    all(is.finite(feature_data$id)), anyDuplicated(feature_data$id)==0,
    # feature_data$name
    assertthat::has_name(feature_data, 'name'), 
    is.character(feature_data$name) || is.factor(feature_data$name),
    all(!is.na(feature_data$name)), anyDuplicated(feature_data$name)==0,
    # rij_data$species
    assertthat::has_name(rij_data, 'species'), is.numeric(rij_data$species), 
    all(is.finite(rij_data$species)), 
    all(rij_data$species %in% feature_data$id),
    # rij_data$pu
    assertthat::has_name(rij_data, 'pu'), is.numeric(rij_data$pu), 
    all(is.finite(rij_data$x)), all(rij_data$pu %in% x$id),
    # rij_data$amount
    assertthat::has_name(rij_data, 'amount'), is.numeric(rij_data$amount), 
    all(is.finite(rij_data$amount)))
  # standardize ids
  rij_data$pu <- match(rij_data$pu, x$id)
  rij_data$species <- match(rij_data$species, feature_data$id)
  # create rij matrix
  rij_mat <- Matrix::sparseMatrix(i=rij_data$species, j=rij_data$pu,
                                  x=rij_data$amount, giveCsparse=TRUE,
                                  index1=TRUE, use.last.ij=FALSE)
  # create new problem object
  p <- pproto(NULL, ConservationProblem,
    constraints=pproto(NULL, Collection), penalties=pproto(NULL, Collection),
    data=list(cost=x, features=feature_data, cost_column = 'cost',
    rij_matrix=rij_mat))
  # return problem
  return(p)
}
