#' @include internal.R
NULL

#' Connectivity matrix
#'
#' Create a matrix showing the connectivity between planning units. Connectivity
#' is calculated as the average conductance of two planning units multiplied
#' by the amount of shared boundary between the two planning units. Thus
#' planning units that each have higher a conductance and share a greater
#' boundary are associated with greater connectivity.
#'
#' @param x \code{\link[raster]{Raster-class}} or
#'   \code{\link[sp]{Spatial-class}} object representing planning units. If
#'   \code{x} is a \code{\link[raster]{Raster-class}} object then it must
#'   contain a single band.
#'
#' @param y \code{\link[raster]{Raster-class}} object showing the conductance
#'   of different areas across the study area, or a \code{character} object
#'   denoting a column name in the attribute table of \code{x} that contains
#'   the conductance values. Note that argument to \code{y} can only be a
#'   \code{character} object if the arugment to \code{x} is a
#'   \code{\link[sp]{Spatial-class}} object. Additionally, note that if
#'   argument to \code{x} is a \code{\link{Raster-class}} object then
#'   argument to \code{y} must have the same spatial properties as it
#'   (ie. coordinate system, extent, resolution).
#'
#' @param boundary_data \code{\link[Matrix]{dsCMatrix-class}} object containing
#'   the shared boundary lengths between planning units. This argument defaults
#'   to \code{NULL} so that the boundary data will be automatically calculated.
#'
#' @param included \code{integer} \code{vector} indicating which cells in argument
#'   to \code{x} are planning units. This argument is only used when \code{x}
#'   is a \code{\link[raster]{Raster-class}} object. It defaults to \code{NULL}
#'   so that the cell indices are calculated automatically.
#'
#' @param ... arguments passed to \code{\link{fast_extract}} for extracting
#'   and calculating the conductance for each unit. These arguments
#'   are only used if argument to \code{x} is a \code{link[sp]{Spatial-class}}
#'   object and argument to \code{y} is a \code{\link{Raster-class}}
#'   object.
#'
#' @details This function returns a \code{\link[Matrix]{dsCMatrix-class}}
#'   sparse symmetric matrix. Each row and column represents a planning unit.
#'   Cell values represent the connectivity between two planning units. To
#'   reduce computational burden, if argument to \code{x} is a
#'   \code{\link[raster]{Raster-class}} object then cells in \code{x} that
#'   contain missing (\code{NA}) values are omitted from the returned matrix.
#'   Furthermore, all cells along the diagonal are missing values since
#'   a planing unit does not have any share connectivity with itself.
#'
#' @return \code{\link[Matrix]{dsCMatrix-class}} sparse symmetric matrix object.
#'
#' @name connectivity_matrix
#'
#' @rdname connectivity_matrix
#'
#' @examples
#' ## load data
#' # planning units
#' data(sim_pu_raster, sim_pu_polygons, sim_pu_lines, sim_pu_points)
#' # here we will use the features' habitat suitabilities as conductances
#' data(sim_features)
#'
#' ## create connectivity matrix using raster planning unit data
#' # extract 9 planning units
#' r <- crop(sim_pu_raster, c(0, 0.3, 0, 0.3))
#' # extact conductance data for the 9 planning units
#' cd <- crop(r, sim_features[[1]])
#' # make connectivity matrix
#' cm_raster <- connectivity_matrix(r, cd)
#' # plot data and matrix
#' par(mfrow = c(1,3))
#' plot(r, main = "planning units")
#' plot(cd, main = "conductivity")
#' plot(raster(as.matrix(cm_raster)), main = "connectivity")
#'
#' ## create connectivity matrix using polygon planning unit data
#' # subset 9 polygons
#' ply <- sim_pu_polygons[c(1:2, 10:12, 20:22), ]
#' # make connectivity matrix
#' cm_ply <- connectivity_matrix(ply, sim_features[[1]])
#' # plot data and matrix
#' par(mfrow = c(1,3))
#' plot(ply, main = "planning units")
#' plot(sim_features[[1]], main = "conductivity")
#' plot(raster(as.matrix(cm_ply)), main = "connectivity")
#'
#' @aliases connectivity_matrix,Spatial,character-method connectivity_matrix,Spatial,Raster-method connectivity_matrix,Raster,Raster-method
#'
#' @export
methods::setGeneric(
  "connectivity_matrix",
  signature = methods::signature("x", "y"),
  function(x, y, ...) standardGeneric("connectivity_matrix"))

#' @name connectivity_matrix
#' @usage \S4method{connectivity_matrix}{Spatial,character}(x, y,
#'   boundary_data = NULL, ...)
#' @rdname connectivity_matrix
methods::setMethod(
  "connectivity_matrix",
  signature(x = "Spatial", y = "character"),
  function(x, y, boundary_data = NULL, ...) {
    # validate that arguments are valid
    assertthat::assert_that(inherits(x, "Spatial"),
      assertthat::is.string(y), "data" %in% methods::slotNames(x),
      assertthat::has_name(x@data, y), is.numeric(x@data[[y]]))
    # generate boundary matrix if not provided
    if (is.null(boundary_data))
      boundary_data <- boundary_matrix(x)
    # generate connectivity data for each pair of connected units
    bd <- matrix_to_triplet_dataframe(boundary_data)
    bd <- bd[bd[[1]] != bd[[2]], ]
    bd$x <- bd$x * ( (x[[y]][bd$i] + x[[y]][bd$j]) * 0.5)
    bd <- bd[which(bd$x > 0), ]
    # generate connectivity matrix
    Matrix::sparseMatrix(i = bd$i, j = bd$j, x = bd$x, symmetric = TRUE,
                         dims = rep(length(x), 2))
  })

#' @name connectivity_matrix
#' @usage \S4method{connectivity_matrix}{Spatial,Raster}(x, y,
#'   boundary_data = NULL, ...)
#' @rdname connectivity_matrix
methods::setMethod(
  "connectivity_matrix",
  signature(x = "Spatial", y = "Raster"),
  function(x, y, boundary_data = NULL, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "Spatial"), inherits(y, "Raster"),
      raster::nlayers(y) == 1, raster::compareCRS(x@proj4string, y@crs),
      rgeos::gWithin(methods::as(raster::extent(x), "SpatialPolygons"),
        methods::as(raster::extent(y), "SpatialPolygons")))
    # generate boundary data if missing
    if (is.null(boundary_data))
      boundary_data <- boundary_matrix(x)
    # extract conductance values
    cv <- fast_extract(y, x, ...)
    # generate connectivity data for each pair of connected units
    bd <- matrix_to_triplet_dataframe(boundary_data)
    bd <- bd[bd[[1]] != bd[[2]], ]
    bd$x <- bd$x * ( (cv[bd$i] + cv[bd$j]) * 0.5)
    bd <- bd[which(bd$x > 0), ]
    # connectivity matrix
    Matrix::sparseMatrix(i = bd$i, j = bd$j, x = bd$x, symmetric = TRUE,
                         dims = rep(length(x), 2))
  })

#' @name connectivity_matrix
#' @usage \S4method{connectivity_matrix}{Raster,Raster}(x, y,
#'   boundary_data = NULL, included = NULL, ...)
#' @rdname connectivity_matrix
methods::setMethod(
  "connectivity_matrix",
  signature(x = "Raster", y = "Raster"),
  function(x, y, boundary_data = NULL, included = NULL, ...) {
    # validate that arguments are valid
    assertthat::assert_that(inherits(x, "Raster"), inherits(y, "Raster"),
      raster::nlayers(x) == 1, raster::nlayers(y) == 1,
      raster::compareRaster(x, y, stopiffalse = FALSE, crs = TRUE, res = TRUE,
                            tolerance = 1e-5))
    # extract data from first bands in x and y
    x <- x[[1]]
    y <- y[[1]]
    # get indices for planning units if not provided
    if (is.null(included))
      included <- raster::Which(!is.na(x), cells = TRUE)
    # generate boundary length matrix if not provided
    if (is.null(boundary_data))
      boundary_data <- boundary_matrix(x)
    # generate connectivity data for each pair of connected units
    bd <- matrix_to_triplet_dataframe(boundary_data)
    bd <- bd[bd[[1]] != bd[[2]], ]
    bd$x <- bd$x * ( (y[included[bd$i]] + y[included[bd$j]]) * 0.5)
    bd <- bd[which(bd$x > 0), ]
    # connectivity matrix
    Matrix::sparseMatrix(i = bd$i, j = bd$j, x = bd$x, symmetric = TRUE,
                         dims = rep(length(included), 2))
  })
