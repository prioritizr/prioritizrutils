#' @include internal.R ArrayParameter-proto.R ScalarParameter-proto.R Parameters-proto.R
NULL

#' Scalar parameters
#'
#' These functions are used to create parameters that consist of a single
#' number. Parameters have a name, a value, a defined range of acceptable
#' values, a default value, a class, and a \code{\link[shiny]{shiny}} widget for 
#' modifying them. If values are supplied to a parameter that are unacceptable
#' then an error is thrown.
#'
#' @param name \code{character} name of parameter.
#' 
#' @param value \code{integer} or \code{double} value depending on the
#'    parameter.
#'
#' @param lower_limit \code{integer} or \code{double} value representing 
#'   the smallest acceptable value for \code{value}. Defaults to 
#'   the smallest possible number on the system.
#'
#' @param upper_limit \code{integer} or \code{double} value representing 
#'   the largest acceptable value for \code{value}. Defaults to 
#'   the largest possible number on the system.
#'
#' @details Below is a list of parameter generating functions and a brief 
#'   description of each. 
#'
#' \describe{
#'
#'   \item{proportion_parameter}{A parameter that is a \code{double} and bounded
#'     between zero and one.}
#'
#'   \item{integer_parameter}{A parameter that is a \code{integer}.}
#'
#'   \item{numeric_parameter}{A parameter that is a \code{double}.}
#'
#' \item{binary_parameter}{A parameter that is restricted to \code{integer}
#'   values of zero or one.}
#' }
#'
#' @return \code{\link{ScalarParameter-class}} object.
#'
#' @name scalar_parameters
NULL

#' @rdname scalar_parameters
#' @export
proportion_parameter <- function(name, value) {
  assertthat::assert_that(assertthat::is.string(name), is.finite(value),
    assertthat::is.scalar(value), isTRUE(value>=0), isTRUE(value<=1))
  pproto('ProportionParameter', ScalarParameter, id=new_id(), name=name, 
    value=as.double(value), default=as.double(value), class='numeric',
    lower_limit=0.0, upper_limit=1.0, widget='shiny::sliderInput')
}

#' @rdname scalar_parameters
#' @export
binary_parameter <- function(name, value) {
  assertthat::assert_that(assertthat::is.string(name),
    assertthat::is.scalar(value), isTRUE(value==1 | value==0), is.finite(value))
  pproto('BinaryParameter', ScalarParameter, id=new_id(), name=name,
    value=as.integer(value), default=as.integer(value), class='integer',
    lower_limit=0L, upper_limit=1L, widget='shiny::checkboxInput')
}

#' @rdname scalar_parameters
#' @export
integer_parameter <- function(name, value, 
                              lower_limit=as.integer(-.Machine$integer.max),
                              upper_limit=as.integer(.Machine$integer.max)) {
  assertthat::assert_that(assertthat::is.string(name), is.finite(value),
    assertthat::is.scalar(value), isTRUE(round(value)==value))
  pproto('IntegerParameter', ScalarParameter, id=new_id(), name=name,
    value=as.integer(value), default=as.integer(value), class='integer',
    lower_limit=as.integer(lower_limit), upper_limit=as.integer(upper_limit),
    widget='shiny::numericInput')
} 

#' @rdname scalar_parameters
#' @export
numeric_parameter <- function(name, value,
                              lower_limit=.Machine$double.xmin,
                              upper_limit=.Machine$double.xmax) {
  assertthat::assert_that(assertthat::is.string(name),
    assertthat::is.scalar(value), is.finite(value))
  pproto('NumericParameter', ScalarParameter, id=new_id(), name=name,
    value=as.double(value), default=as.double(value), class='numeric',
    lower_limit=as.double(lower_limit), upper_limit=as.double(upper_limit),
    widget='shiny::numericInput')
}

#' Array parameters
#'
#' Create parameters that consist of multiple numbers. If an attempt is made 
#' to create a parameter with conflicting settings then an error will be thrown.
#'
#' @param name \code{character} name of parameter.
#'
#' @param value \code{vector} of values.
#'
#' @param label \code{character} \code{vector} of labels for each value.
#'
#' @param lower_limit \code{vector} of values denoting the minimum acceptable
#'   value for each element in \code{value}. Defaults to the 
#'   smallest possible number on the system.
#'
#' @param upper_limit \code{vector} of values denoting the maximum acceptable
#'   value for each element in \code{value}. Defaults to the 
#'   largest  possible number on the system.
#'   
#' @details Below is a list of parameter generating functions and a brief 
#'   description of each.
#' \describe{
#'
#'   \item{proportion_parameter_array}{a parameter that consists of multiple 
#'    \code{numeric} values that are between zero and one.}
#'
#'   \item{numeric_parameter_array}{a parameter that consists of multiple
#'     \code{numeric} values.}
#'
#'   \item{binary_parameter_array}{a parameter that consists of multiple
#'     \code{integer} values that are either zero or one.}
#'
#' }
#'
#' @return \code{\link{ArrayParameter-class}} object.
#'
#' @name array_parameters
NULL

#' @rdname array_parameters
#' @export
proportion_parameter_array <- function(name, value, label) {
  assertthat::assert_that(assertthat::is.string(name),
    inherits(value, 'numeric'),
    isTRUE(all(value >= 0)), isTRUE(all(value <= 1)), assertthat::noNA(value),
    all(is.finite(value)), inherits(label, 'character'),
    assertthat::noNA(label), length(value) == length(label))
  pproto('ProportionParameterArray', ArrayParameter, id=new_id(),
    name = name, value=as.double(value),
    label=label, class='numeric', default=as.double(value), 
    lower_limit=rep(0.0, length(value)), upper_limit=rep(1.0, length(value)),
    length=length(value), widget='rhandsontable::rHandsontableOutput')
}

#' @rdname array_parameters
#' @export
binary_parameter_array <- function(name, value, label) {
  assertthat::assert_that(assertthat::is.string(name),
    inherits(value, 'numeric'), 
    assertthat::noNA(value), all(is.finite(value)),
    isTRUE(all(value==1 | value==0)),
    inherits(label, 'character'), assertthat::noNA(label),
    length(value) == length(label))
  pproto('BinaryParameterArray', ArrayParameter, id=new_id(),
    name = name, value=as.integer(value),
    label=label, class='integer', lower_limit=rep(0L, length(value)), 
    upper_limit=rep(1L, length(value)),
    default=as.integer(value), length=length(value),
    widget='rhandsontable::rHandsontableOutput')
}

#' @rdname array_parameters
#' @export
numeric_parameter_array <- function(name, value, label,
                                    lower_limit=rep(.Machine$double.xmin,
                                      length(value)),
                                    upper_limit=rep(.Machine$double.xmax,
                                      length(value))) {
  assertthat::assert_that(assertthat::is.string(name),
    inherits(value, 'numeric'),assertthat::noNA(value), all(is.finite(value)),
    inherits(label, 'character'), assertthat::noNA(label),
    length(value) == length(label))
  pproto('NumericParameterArray', ArrayParameter, id=new_id(),
    name = name, value=as.double(value),
    label=label, class='numeric', lower_limit=lower_limit, 
    upper_limit=upper_limit, default=as.double(value), length=length(value),
    widget='rhandsontable::rHandsontableOutput')
}

#' Parameters
#'
#' Create a new collection of \code{Parameter} objects.
#' 
#' @param ... \code{\link{Parameter-class}} objects.
#'
#' @return \code{\link{Parameters-class}} object.
#'
#' @seealso \code{\link{array_parameters}}, \code{\link{scalar_parameters}}.
#'
#' @examples
#'
#' # create two Parameter objects
#' p1 <- binary_parameter('parameter one', 1)
#' print(p1)
#'
#' p2 <- numeric_parameter('parameter two', 5)
#' print(p2)
#'
#' # store Parameter objects in a Parameters object
#' p <- parameters(p1, p2)
#' print(p)
#'
#' @export
parameters <- function(...) {
  args <- list(...)
  assertthat::assert_that(isTRUE(all(sapply(args, inherits, 'Parameter'))))
  p <- pproto(NULL, Parameters)
  for (i in args) p$add(i)
  return(p)
} 
