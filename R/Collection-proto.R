#' @include internal.R pproto.R Constraint-proto.R Penalty-proto.R
NULL

#' @export
methods::setOldClass('Collection')

#' Collection prototype
#'
#' This prototype represents a collection of 
#' \code{\link{ConservationModifier-class}} objects.
#'
#' @section Fields:
#' \describe{
#'   \item{$data}{\code{list} object containing 
#'      \code{\link{ConservationModifier-class}} objects.}
#' }
#'
#' @section Usage:
#' \code{x$print()}
#'
#' \code{x$show()}
#'
#' \code{x$repr()}
#'
#' \code{x$add}
#'
#' \code{x$get_parameter(id)}
#'
#' \code{x$set_parameter(id, value)}
#'
#' \code{x$render_parameter(id)}
#'
#' \code{x$render_all_parameters()}
#'
#' @section Arguments:
#' \describe{
#' \item{id}{\code{id} object.}
#' \item{value}{any object.}
#' }
#'
#' @section Details:
#' \describe{
#' \item{print}{print the object.}
#' \item{show}{show the object.}
#' \item{repr}{\code{character} representation of object.}
#'
#' \item{add}{add \code{\link{ConservationModifier-class}} object.}
#'
#' \item{get_parameter}{retrieve the value of a parameter in the object
#'   using an \code{id} object.}
#' \item{set_parameter}{change the value of a parameter in the object 
#'   to a new object.}
#' \item{render_parameter}{generate a \emph{shiny} widget to modify the
#'   the value of a parameter (specified by argument \code{id}).}
#' \item{render_all_parameters}{generate a \code{\link[shiny]{div}}
#'   containing all the parameters' widgets.}
#' }
#'
#' @seealso \code{\link{Constraint-class}}, \code{\link{Penalties-class}}.
#'
#' @name Collection-class
#'
#' @aliases Collection
NULL

#' @export
Collection <- pproto(
  'Collection',
  data = list(),
  repr = function(self) {
    if (length(self$data)>0)
      return(paste0('<', paste(sapply(self$data, function(z) {z$repr()}),
        collapse='\n'), '>'))
    return('<none>')
  },
  find_parameter = function(id) {
    r <- sapply(self$data,
                function(x) {
                  id %in% sapply(x$parameters, 
                                 function(z) {z$id})})
    s <- sum(r)
    if (s==0) {
      stop('no parameter with matching id found')
    } else if (s > 1) {
      stop('multiple parameters with matching id found')
    }
    which(r)
  },  
  add = function(self, x) {
    assertthat::assert_that(inherits(x, 'ConservationModifier'))
    self$data <- append(self$data, x)
  },
  get_parameter = function(self, id) {
    assertthat::assert_that(inherits(id), 'Id')
    self$data[[self$find_parameter(id)]]$get_parameter(id)
  },
  set_parameter = function(self, id, value) {
    assertthat::assert_that(inherits(id), 'Id')
    self$data[[self$find_parameter(id)]]$set_parameter(id, value)
  },
  render_parameter = function(self, id, value) {
    assertthat::assert_that(inherits(id), 'Id')
    self$data[[self$find_parameter(id)]]$render_parameter(id)
  },
  render_all_parameters = function(self) {
    do.call(shiny::div, 
        append(list(class='Collection'), 
                lapply(self$data,
                       function(x) {x$render_all_parameters()})))
  })
