#' @include internal.R 
NULL

#' Connected matrix
#'
#' Create a matrix showing which planning units are spatially connected.
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
#'
#' @param distance \code{numeric} If \code{x} is a 
#'   \code{\link{SpatialPoints-class}} object, the distance that planning units 
#'   have to be within in order to qualify as being connected.
#' 
#' @param ... not used.
#'
#' @details This function returns a \code{\link[Matrix]{dgCMatrix-class}} sparse
#'   matrix. Cells along the off-diagonal indicate if two planning units are 
#'   connected. Cells along the diagonal are zero to reduce memory consumption.
#'
#' @return \code{\link[Matrix]{dsCMatrix-class}} object.
#'
#' @name connected_matrix
#'
#' @rdname connected_matrix
#'
#' @export
connected_matrix <- function(x, ...) UseMethod('connected_matrix')

#' @rdname connected_matrix
#' @method connected_matrix Raster
#' @export
connected_matrix.Raster <- function(x, directions=4L, ...) {
  assertthat::assert_that(inherits(x, 'Raster'))
  assertthat::assert_that(
    isTRUE(raster::nlayers(x)==1))
  # indices of cells with finite values
  include <- raster::Which(is.finite(x), cells=TRUE)
  # find the neighboring indices of these cells
  m <- raster::adjacent(x, include, pairs=TRUE, directions = directions)
  m <- m[(m[,1] %in% include) & (m[,2] %in% include),]
  # coerce to sparse matrix object
  m <- Matrix::forceSymmetric(Matrix::sparseMatrix(i=m[,1], j=m[,2], x=rep(1, nrow(m)),
                                                   dims=rep(raster::ncell(x), 2)))
  # return included planning units
  return(m[include,include])
}

#' @rdname connected_matrix
#' @method connected_matrix SpatialPolygons
#' @export
connected_matrix.SpatialPolygons <- function(x, ...) {
  assertthat::assert_that(inherits(x, 'SpatialPolygons'))
  m <- rcpp_list_to_matrix_indices(rgeos::gIntersects(x, byid=TRUE,
                                                      returnDense=FALSE))
  m <- Matrix::forceSymmetric(Matrix::sparseMatrix(i=m$i, j=m$j, x=m$x))
  # return matrix
  return(m)
}

#' @rdname connected_matrix
#' @method connected_matrix SpatialLines
#' @export
connected_matrix.SpatialLines <- function(x,  ...) {
  assertthat::assert_that(inherits(x, 'SpatialLines'))
  m <- rcpp_list_to_matrix_indices(rgeos::gIntersects(x, byid=TRUE, 
                                                      returnDense=FALSE))
  m <- Matrix::forceSymmetric(Matrix::sparseMatrix(i=m$i, j=m$j, x=m$x))
  # return matrix
  return(m)  
}

#' @rdname connected_matrix
#' @method connected_matrix SpatialPoints
#' @export
connected_matrix.SpatialPoints <- function(x, distance, ...) {
    assertthat::assert_that(inherits(x, 'SpatialPoints'),
                                     assertthat::is.scalar(distance),
                                     isTRUE(distance >= 0))
  m <- rgeos::gWithinDistance(x, dist=distance, byid=TRUE)
  m[m] <- 1
  diag(m) <- 0
  m <- Matrix::Matrix(m,sparse=TRUE)
  m <- Matrix::forceSymmetric(m)
  # return matrix
  return(m)  
}

