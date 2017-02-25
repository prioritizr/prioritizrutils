#' @include internal.R
NULL

#' Create a new pproto object
#'
#' Construct a new object with \code{pproto}. This object system is inspired
#' from the \code{ggproto} system used in the \emph{ggplot2} package.
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. This is optional: if \code{NULL} (the default),
#'   no class name will be added to the object.
#'
#' @param _inherit ggproto object to inherit from. If \code{NULL}, don't
#'   inherit from any object.
#'
#' @param ... A list of members in the pproto object.
#'
#' @examples
#' Adder <- pproto('Adder',
#'   x = 0,
#'   add = function(self, n) {
#'     self$x <- self$x + n
#'     self$x
#'   }
#' )
#'
#' Adder$add(10)
#' Adder$add(10)
#'
#' Abacus <- pproto('Abacus', Adder,
#'   subtract = function(self, n) {
#'     self$x <- self$x - n
#'     self$x
#'   }
#' )
#' Abacus$add(10)
#' Abacus$subtract(10)
#'
#' @export
pproto <- function(`_class` = NULL, `_inherit` = NULL, ...) {
  assertthat::assert_that(assertthat::is.string(`_class`) || is.null(`_class`),
    inherits(`_inherit`, 'pproto') || is.null(`_inherit`))
  # function to re-instance proto fields
  reprototype_fields <- function(p) {
    n <- p$ls()
    cl <- sapply(n, function(i) inherits(p[[i]], 'pproto'))
    if (isTRUE(any(cl))) {
      for (i in which(cl)) {
        p[[i]] <- p[[i]]$proto()
        reprototype_fields(p[[i]])
      }
    } else {
      return(p)
    }
  }
  # function to copy objects from one proto to another
  assign_fields <- function(p1, p2) {
    if (!inherits(p2, 'pproto')) return(NULL)
    for (i in p2$ls()) {
      if (inherits(p2[[i]], 'proto')) {
        p1[[i]] <- reprototype_fields(p2$proto())
      } else {
        p1[[i]] <- p2[[i]]
      }
    }
    assign_fields(p1, p2$.super)
  }
  # create new proto
  p <- proto::proto(...)
  if (!is.null(`_inherit`)) {
    # assign objects
    assign_fields(p, `_inherit`)
    # assign class
    class(p) <- class(`_inherit`)
  } else {
    # assign pproto class
    class(p) <- c('pproto', class(p))
  }
  # assign new class if specified
  if (!is.null(`_class`))
    class(p) <- c(`_class`, class(p))
  # return value
  p
}

