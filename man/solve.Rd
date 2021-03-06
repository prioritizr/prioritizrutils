% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solve.R
\name{solve}
\alias{solve}
\alias{solve,OptimizationProblem,Solver-method}
\alias{solve,ConservationProblem,missing-method}
\alias{solve}
\alias{solve}
\title{Solve a problem}
\arguments{
\item{a}{\code{\link{ConservationProblem-class}} or an
\code{\link{OptimizationProblem-class}} object.}

\item{b}{\code{\link{Solver-class}} object. Not used if \code{a} is an
\code{\link{ConservationProblem-class}} object.}

\item{...}{arguments passed to \code{\link{compile}}.}
}
\value{
A \code{\link[raster]{RasterLayer-class}},
  \code{\link[sp]{Spatial-class}}, or a \code{numeric} vector containing the
  solution depending on the argument to \code{a}.
}
\description{
Solve a \code{\link{ConservationProblem-class}} or an
\code{\link{OptimizationProblem-class}}.
}
\details{
If a \code{\link{OptimizationProblem-class}} is supplied, then the
  solution is returned as a \code{logical} showing the status of each
  planning unit. If a \code{\link{ConservationProblem-class}} is supplied,
  then the  solution is returned in the format of the cost data contained
  inside it:
  \itemize{
  \item{\code{\link[raster]{RasterLayer-class}}}{the solution is returned as
     a \code{\link[raster]{RasterLayer-class}} object. Cell values denote
     the decision.}
  \item{\code{\link[sp]{Spatial-class}}}{the solution is returned as a
     \code{\link[sp]{SpatialPolygonsDataFrame}} or
     \code{\link[sp]{SpatialLinesDataFrame}} with a "Solution" column
     containing the decision values.}
  }
}
\examples{
# build minimal conservation problem
p <- problem(sim_pu_raster, sim_features) \%>\%
  add_min_set_objective() \%>\%
  add_relative_targets(0.1) \%>\%
  add_binary_decision()

\donttest{
# solve the problem
s <- solve(p)

# print the solution
print(s)

# plot solution
plot(s, main="solution")
}

}
