#' @include internal.R parallel.R
NULL

#' Fast extract
#'
#' Extract data from a \code{\link[raster]{Raster-class}} object from a 
#' \code{\link[sp]{Spatial-class}} object using performance enhancing tricks.
#'
#' @param x \code{\link[raster]{Raster-class}} object.
#'
#' @param y \code{\link[sp]{Spatial-class}} object.
#'
#' @param ... additional arguments passed to \code{\link[raster]{extract}}.
#'
#' @return \code{data.frame}, \code{matrix}, or \code{list} object 
#'   depending on the arguments.
#'
#' @seealso \code{\link[raster]{extract}},
#'   \code{\link[velox]{VeloxRaster_extract}}.
#'
#' @details Spatial analyses will be conducted using the 
#'   \code{\link[velox]{velox}} package if it is installed. Additionally, 
#'   multiple threads can be used to speed up computation using the 
#'   \code{\link{set_number_of_threads}} function.
#'
#' @name fast_extract
#'
#' @exportMethod fast_extract
#'
#' @aliases fast_extract,Raster,SpatialLines-method fast_extract,Raster,SpatialPoints-method fast_extract,Raster,SpatialPolygons-method 
#'
#' @export
methods::setGeneric('fast_extract',
                    signature=methods::signature('x', 'y'),
                    function(x, y, ...) standardGeneric('fast_extract'))

#' @name fast_extract
#' @usage fast_extract(x, y) # Raster, SpatialPolygons
#' @rdname fast_extract
methods::setMethod('fast_extract', signature(x='Raster', y='SpatialPolygons'), 
  function(x, y, fun=mean, velox=requireNamespace('velox'), ...) {
    # assert arguments are valid
    assertthat::assert_that(inherits(x, 'Raster'), inherits(y, 'SpatialPolygons'),
      isTRUE(is.null(fun) || inherits(fun, 'function')),
      assertthat::is.flag(velox), raster::compareCRS(x@crs, y@proj4string),
      rgeos::gIntersects(methods::as(raster::extent(x[[1]]), 'SpatialPolygons'),
        methods::as(raster::extent(y), 'SpatialPolygons')))
    if (velox & !requireNamespace('velox'))
      stop('the velox R package needs to be installed to use velox')
    # data processing
    args <- list(...)
    if (velox & !(isTRUE(args$cellnumbers) || isTRUE(args$sp))) {
      if (inherits(x, 'RasterBrick'))
        x <- raster::stack(x)
      if (is.parallel()) {
        # use the velox with parallel processing
        parallel::clusterExport(.pkgenv$cluster, c('x', 'y', 'fun', 'args',
          'velox_extract'), envir=environment())
        m <- plyr::llply(distribute_load(length(y)), .parallel=TRUE,
          function(i) {
              return(do.call(velox_extract, append(
                list(x=x, y=y[i,], fun=fun), args)))
          })
        parallel::clusterEvalQ(.pkgenv$cluster, {rm('x', 'y', 'fun', 'args')})
        m <- do.call(rbind, m)
      } else {
        # use the velox without parallel processing
        m <- velox_extract(x=x, y=y, fun=fun, ...)
      }
    } else {
      if (is.parallel()) {
        m <- parallelized_extract(x=x, y=y, fun=fun, ...)
      } else {
        m <- raster::extract(x=x, y=y, fun=fun, ...)
      }
    }
    # return result
    return(m)
  }
)

#' @name fast_extract
#' @usage fast_extract(x, y) # Raster, SpatialLines
#' @rdname fast_extract
methods::setMethod('fast_extract', signature(x='Raster', y='SpatialLines'), 
  function(x, y, fun=mean, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, 'Raster'), inherits(y, 'SpatialLines'),
        inherits(fun, 'function'),raster::compareCRS(x@crs, y@proj4string),
        rgeos::gIntersects(methods::as(raster::extent(x[[1]]), 
                                       'SpatialPolygons'), 
                           methods::as(raster::extent(y), 'SpatialPolygons')))
    # data processing
    if (is.parallel()) {
      m <- parallelized_extract(x=x, y=y, fun=fun, ...)
    } else {
      m <- raster::extract(x=x, y=y, fun=fun, ...)
    }
    # return result
    return(m)
  }
)

#' @name fast_extract
#' @usage fast_extract(x, y) # Raster, SpatialPoints
#' @rdname fast_extract
methods::setMethod('fast_extract', signature(x='Raster', y='SpatialPoints'), 
  function(x, y, fun=mean, ...) {
    # assert that arguments are valid
    assertthat::assert_that(inherits(x, 'Raster'), inherits(y, 'SpatialPoints'),
        inherits(fun, 'function'),raster::compareCRS(x@crs, y@proj4string),
        rgeos::gIntersects(methods::as(raster::extent(x[[1]]), 
                                       'SpatialPolygons'),
                           methods::as(raster::extent(y), 'SpatialPolygons')))
    # return result
    return(raster::extract(x, y, fun=fun, ...))
  }
)
