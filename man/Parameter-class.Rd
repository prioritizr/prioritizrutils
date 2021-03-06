% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Parameter-proto.R
\name{Parameter-class}
\alias{Parameter-class}
\alias{Parameter}
\title{Parameter class}
\description{
This class is used to represent a parameter that has multiple values. Each
value has a different label to differentiate values. \strong{Only experts
should interact directly with this class.}
}
\section{Fields}{

\describe{

\item{$id}{\code{\link{Id}} identifier for parameter.}

\item{$name}{\code{character} name of parameter.}

\item{$value}{\code{numeric} \code{vector} of values.}

\item{$default}{\code{numeric} \code{vector} of default values.}

\item{$class}{\code{character} name of the class that the values inherit
  from (eg. \code{"integer"}.}

\item{$lower_limit}{\code{numeric} \code{vector} specifying the minimum
  permitted value for each element in \code{$value}.}

\item{$upper_limit}{\code{numeric} \code{vector} specifying the maximum
  permitted value for each element in \code{$value}.}

\item{$widget}{\code{function} used to construct a
               \code{\link[shiny]{shiny}} interface for modifying values.}
}
}

\section{Usage}{


\code{x$print()}

\code{x$show()}

\code{x$reset()}
}

\section{Details}{

\describe{
\item{print}{print the object.}
\item{show}{show the object.}
\item{reset}{change the parameter values to be the default values.}
}
}

\seealso{
\code{\link{ScalarParameter-class}}.
}
