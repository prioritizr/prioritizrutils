#' @include internal.R fast_extract.R
NULL

#' Feature by planning unit matrix
#'
#' Generate a feature by planning unit (aka \emph{rij} matrix) using spatial
#' data sets. The \code{rij} contains data on the amount of each feature in
#' each planning unit.
#'
#' @param x \code{\link[raster]{RasterLayer-class}} or
#'   \code{\link[sp]{Spatial-class}} object representing the
#'   planning units
#'
#' @param y \code{\link[raster]{Raster-class}} object representing the
#'   features
#'
#' @param ... additional arguments passed to \code{\link{fast_extract}} if
#'   argument to \code{x} inherits from a \code{\link[sp]{Spatial-class}}
#'   object.
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
#' @aliases rij_matrix,Raster,Raster-method rij_matrix,Spatial,Raster-method
#'
#' @examples
#' # load data
#' data(sim_pu_raster, sim_pu_polygons)
#'
#' # create rij matrix using raster planning units
#' rij_raster <- rij_matrix(sim_pu_raster, sim_features)
#'
#' # create rij matrix using polygon planning units
#' rij_polygons <- rij_matrix(sim_pu_polygons, sim_features)
#'
#' @export
methods::setGeneric("rij_matrix",
                    signature = methods::signature("x", "y"),
                    function(x, y, ...) standardGeneric("rij_matrix"))

#' @name rij_matrix
#' @usage rij_matrix(x, y, ...) # x=Raster, y=Raster
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "Raster", y = "Raster"),
  function(x, y, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, "Raster"), inherits(y, "Raster"),
      isTRUE(raster::nlayers(x) == 1),
      raster::compareRaster(x, y[[1]], res = TRUE, tolerance = 1e-5,
                            stopiffalse = FALSE))
    # data processing
    included <- raster::Which(!is.na(x))
    if (raster::canProcessInMemory(x, n = raster::nlayers(y) + 2)) {
      # if the all the features can be fit into memory then processes
      # them all in memory
      m <- y[included]
      if (!is.matrix(m))
        m <- matrix(m, ncol = 1)
      m[is.na(m)] <- 0
      m <- methods::as(m, "dgCMatrix")
    } else {
      # othewise, process each feature seperately
        m <- plyr::llply(seq_len(raster::nlayers(y)), .parallel = FALSE,
          function(i) {
            m <- matrix(y[included], ncol = 1)
            m[is.na(m)] <- 0
            m <- methods::as(m, "dgCMatrix")
          })
      m <- do.call(rbind, m)
    }
    # return result
    return(Matrix::t(m))
})

#' @name rij_matrix
#' @usage rij_matrix(x, y, ...) # x=Spatial, y=Raster
#' @rdname rij_matrix
methods::setMethod(
  "rij_matrix",
  signature(x = "Spatial", y = "Raster"),
  function(x, y,  ...) {
    m <- fast_extract(x = y, y = x, df = FALSE, sp = FALSE, ...)
    if (raster::nlayers(y) == 1)
      m <- matrix(m, ncol = 1)
    m[is.na(m[])] <- 0
    m <- methods::as(m, "dgCMatrix")
    return(Matrix::t(m))
})
