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
#' @examples
#' # proportion parameter
#' p1 <- proportion_parameter('prop', 0.5) # create new object
#' print(p1) # print it
#' p1$get() # get value
#' p1$id # get id
#' p1$validate(5) # check if 5 is a validate input
#' p1$validate(0.1) # check if 0.1 is a validate input
#' p1$set(0.1) # change value to 0.1
#' print(p1)
#'
#' # binary parameter
#' p2 <- binary_parameter('bin', 0) # create new object
#' print(p2) # print it
#' p2$get() # get value
#' p2$id # get id
#' p2$validate(5) # check if 5 is a validate input
#' p2$validate(1L) # check if 1L is a validate input
#' p2$set(1L) # change value to 1L
#' print(p1) # print it again
#'
#' # integer parameter
#' p3 <- integer_parameter('int', 5L) # create new object
#' print(p3) # print it
#' p3$get() # get value
#' p3$id # get id
#' p3$validate(5.6) # check if 5.6 is a validate input
#' p3$validate(2L) # check if 2L is a validate input
#' p3$set(2L) # change value to 2L
#' print(p3) # print it again
#'
#' # numeric parameter
#' p4 <- numeric_parameter('dbl', -7.6) # create new object
#' print(p4) # print it
#' p4$get() # get value
#' p4$id # get id
#' p4$validate(NA) # check if NA is a validate input
#' p4$validate(8.9) # check if 8.9 is a validate input
#' p4$set(8.9) # change value to 8.9
#' print(p4) # print it again
#'
#' # numeric parameter with lower bounds
#' p5 <- numeric_parameter('bdbl', 6, lower_limit=0) # create new object
#' print(p5) # print it
#' p5$get() # get value
#' p5$id # get id
#' p5$validate(-10) # check if -10 is a validate input
#' p5$validate(90) # check if 90 is a validate input
#' p5$set(90) # change value to 8.9
#' print(p5) # print it again
#'
#' @name scalar_parameters
NULL

#' @rdname scalar_parameters
#' @export
proportion_parameter <- function(name, value) {
  assertthat::assert_that(assertthat::is.string(name), is.finite(value),
    assertthat::is.scalar(value), isTRUE(value >= 0), isTRUE(value <= 1))
  pproto("ProportionParameter", ScalarParameter, id = new_id(), name = name,
         value = as.double(value), default = as.double(value),
         class = "numeric", lower_limit = 0.0, upper_limit = 1.0,
         widget = "shiny::sliderInput")
}

#' @rdname scalar_parameters
#' @export
binary_parameter <- function(name, value) {
  assertthat::assert_that(assertthat::is.string(name),
    assertthat::is.scalar(value), isTRUE(value == 1 | value == 0),
    is.finite(value))
  pproto("BinaryParameter", ScalarParameter, id = new_id(), name = name,
    value = as.integer(value), default = as.integer(value), class = "integer",
    lower_limit = 0L, upper_limit = 1L, widget = "shiny::checkboxInput")
}

#' @rdname scalar_parameters
#' @export
integer_parameter <- function(name, value,
                              lower_limit=as.integer(-.Machine$integer.max),
                              upper_limit=as.integer(.Machine$integer.max)) {
  assertthat::assert_that(assertthat::is.string(name), is.finite(value),
    assertthat::is.scalar(value), isTRUE(round(value) == value))
  pproto("IntegerParameter", ScalarParameter, id = new_id(), name = name,
    value = as.integer(value), default = as.integer(value), class = "integer",
    lower_limit = as.integer(lower_limit),
    upper_limit = as.integer(upper_limit), widget = "shiny::numericInput")
}

#' @rdname scalar_parameters
#' @export
numeric_parameter <- function(name, value,
                              lower_limit=.Machine$double.xmin,
                              upper_limit=.Machine$double.xmax) {
  assertthat::assert_that(assertthat::is.string(name),
    assertthat::is.scalar(value), is.finite(value))
  pproto("NumericParameter", ScalarParameter, id = new_id(), name = name,
    value = as.double(value), default = as.double(value), class = "numeric",
    lower_limit = as.double(lower_limit), upper_limit = as.double(upper_limit),
    widget = "shiny::numericInput")
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
#'   \item{binary_parameter_array}{a parameter that consists of multiple
#'     \code{integer} values that are either zero or one.}
#'
#'   \item{integer_parameter_array}{a parameter that consists of multiple
#'     \code{integer} values.}
#'
#'   \item{numeric_parameter_array}{a parameter that consists of multiple
#'     \code{numeric} values.}
#'
#' }
#'
#' @return \code{\link{ArrayParameter-class}} object.
#'
#' @examples
#' # proportion parameter array
#' p1 <- proportion_parameter_array('prop_array', c(0.1, 0.2, 0.3),
#'                                  letters[1:3])
#' print(p1) # print it
#' p1$get() # get value
#' p1$id # get id
#' invalid <- data.frame(value = 1:3, row.names=letters[1:3]) # invalid values
#' p1$validate(invalid) # check invalid input is invalid
#' valid <- data.frame(value = c(0.4, 0.5, 0.6), row.names=letters[1:3]) # valid
#' p1$validate(valid) # check valid input is valid
#' p1$set(valid) # change value to valid input
#' print(p1)
#'
#' # binary parameter array
#' p2 <- binary_parameter_array('bin_array', c(0L, 1L, 0L), letters[1:3])
#' print(p2) # print it
#' p2$get() # get value
#' p2$id # get id
#' invalid <- data.frame(value = 1:3, row.names=letters[1:3]) # invalid values
#' p2$validate(invalid) # check invalid input is invalid
#' valid <- data.frame(value = c(0L, 0L, 0L), row.names=letters[1:3]) # valid
#' p2$validate(valid) # check valid input is valid
#' p2$set(valid) # change value to valid input
#' print(p2)
#'
#' # integer parameter array
#' p3 <- integer_parameter_array('int_array', c(1:3), letters[1:3])
#' print(p3) # print it
#' p3$get() # get value
#' p3$id # get id
#' invalid <- data.frame(value = rnorm(3), row.names=letters[1:3]) # invalid
#' p3$validate(invalid) # check invalid input is invalid
#' valid <- data.frame(value = 5:7, row.names=letters[1:3]) # valid
#' p3$validate(valid) # check valid input is valid
#' p3$set(valid) # change value to valid input
#' print(p3)
#'
#' # numeric parameter array
#' p4 <- numeric_parameter_array('dbl_array', c(0.1, 4, -5), letters[1:3])
#' print(p4) # print it
#' p4$get() # get value
#' p4$id # get id
#' invalid <- data.frame(value = c(NA, 1, 2), row.names=letters[1:3]) # invalid
#' p4$validate(invalid) # check invalid input is invalid
#' valid <- data.frame(value = c(1, 2, 3), row.names=letters[1:3]) # valid
#' p4$validate(valid) # check valid input is valid
#' p4$set(valid) # change value to valid input
#' print(p4)
#'
#' # numeric parameter array with lower bounds
#' p5 <- numeric_parameter_array('b_dbl_array', c(0.1, 4, -5), letters[1:3],
#'                               lower_limit=c(0, 1, 2))
#' print(p5) # print it
#' p5$get() # get value
#' p5$id# get id
#' invalid <- data.frame(value = c(-1, 5, 5), row.names=letters[1:3]) # invalid
#' p5$validate(invalid) # check invalid input is invalid
#' valid <- data.frame(value = c(0, 1, 2), row.names=letters[1:3]) # valid
#' p5$validate(valid) # check valid input is valid
#' p5$set(valid) # change value to valid input
#' print(p5)
#'
#' @name array_parameters
NULL

#' @rdname array_parameters
#' @export
proportion_parameter_array <- function(name, value, label) {
  assertthat::assert_that(assertthat::is.string(name),
    inherits(value, "numeric") || inherits(value, "integer"),
    isTRUE(all(value >= 0)), isTRUE(all(value <= 1)), assertthat::noNA(value),
    all(is.finite(value)), inherits(label, "character"),
    assertthat::noNA(label), length(value) == length(label))
  pproto("ProportionParameterArray", ArrayParameter, id = new_id(),
    name = name, value = as.double(value),
    label = label, class = "numeric", default = as.double(value),
    lower_limit = rep(0.0, length(value)),
    upper_limit = rep(1.0, length(value)), length = length(value),
    widget = "rhandsontable::rHandsontableOutput")
}

#' @rdname array_parameters
#' @export
binary_parameter_array <- function(name, value, label) {
  assertthat::assert_that(assertthat::is.string(name),
    inherits(value, "numeric") || inherits(value, "integer"),
    assertthat::noNA(value), all(is.finite(value)),
    isTRUE(all(value == 1 | value == 0)),
    inherits(label, "character"), assertthat::noNA(label),
    length(value) == length(label))
  pproto("BinaryParameterArray", ArrayParameter, id = new_id(),
    name = name, value = as.integer(value),
    label = label, class = "integer", lower_limit = rep(0L, length(value)),
    upper_limit = rep(1L, length(value)),
    default = as.integer(value), length = length(value),
    widget = "rhandsontable::rHandsontableOutput")
}

#' @rdname array_parameters
#' @export
integer_parameter_array <- function(name, value, label,
                                  lower_limit=rep(as.integer(
                                    -.Machine$integer.max), length(value)),
                                    upper_limit=rep(as.integer(
                                      .Machine$integer.max), length(value))) {
  assertthat::assert_that(assertthat::is.string(name),
    inherits(value, "numeric") || inherits(value, "integer"),
    assertthat::noNA(value), all(is.finite(value)),
    inherits(label, "character"), assertthat::noNA(label),
    length(value) == length(label))
  pproto("IntegerParameterArray", ArrayParameter, id = new_id(),
    name = name, value = as.integer(value),
    label = label, class = "integer", lower_limit = as.integer(lower_limit),
    upper_limit = as.integer(upper_limit), default = as.integer(value),
    length = length(value), widget = "rhandsontable::rHandsontableOutput")
}

#' @rdname array_parameters
#' @export
numeric_parameter_array <- function(name, value, label,
                                    lower_limit=rep(.Machine$double.xmin,
                                      length(value)),
                                    upper_limit=rep(.Machine$double.xmax,
                                      length(value))) {
  assertthat::assert_that(assertthat::is.string(name),
    inherits(value, "numeric") || inherits(value, "integer"),
    assertthat::noNA(value), all(is.finite(value)),
    inherits(label, "character"), assertthat::noNA(label),
    length(value) == length(label))
  pproto("NumericParameterArray", ArrayParameter, id = new_id(),
    name = name, value = as.double(value),
    label = label, class = "numeric", lower_limit = as.double(lower_limit),
    upper_limit = as.double(upper_limit), default = as.double(value),
    length = length(value), widget = "rhandsontable::rHandsontableOutput")
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
#' p1 <- binary_parameter("parameter one", 1)
#' print(p1)
#'
#' p2 <- numeric_parameter("parameter two", 5)
#' print(p2)
#'
#' # store Parameter objects in a Parameters object
#' p <- parameters(p1, p2)
#' print(p)
#'
#' @export
parameters <- function(...) {
  args <- list(...)
  assertthat::assert_that(isTRUE(all(sapply(args, inherits, "Parameter"))))
  p <- pproto(NULL, Parameters)
  for (i in args) p$add(i)
  return(p)
}
