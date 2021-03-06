% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_boundary_penalties.R
\name{add_boundary_penalties}
\alias{add_boundary_penalties}
\title{Add boundary penalties}
\usage{
add_boundary_penalties(x, penalty, edge_factor, boundary_data = NULL)
}
\arguments{
\item{x}{\code{\link{ConservationProblem-class}} object.}

\item{penalty}{\code{numeric} penalty for exposed edges. This
is equivalent to the
\href{http://marxan.net/downloads/uq_marxan_web_2/module2.html}{boundary length modifier (BLM)}
parameter in \href{http://marxan.net}{marxan.net}.}

\item{edge_factor}{\code{numeric} proportion to scale edges that do
not have any neighboring planning units. For example, an edge factor
of \code{0.5} is commonly used for planning units along the coast line.}

\item{boundary_data}{\code{matrix}, \code{\link[Matrix]{Matrix}}, or
\code{data.frame} object showing the shared boundary lengths
between planning units. If \code{boundary_data} is a matrix then
each row and column denote a planning unit and cell values
represent their shared boundary. If \code{boundary_data} is
\code{data.frame} then it must have the columns \code{"id1"},
\code{"id2"}, and \code{"boundary"}, where each row shows
the shared boundary between two planning units (as per the standard
Marxan input format). This argument is required
if the planning units in \code{x} are stored in a \code{data.frame}
object. Otherwise, it is optional and the boundary data will be
automatically calculated. Note that the \code{boundary_data} must be
denote symmetric relationships between planning units. If
assymetric relaionships are required, use the
\code{\link{add_connectivity_penalties}} function.}
}
\value{
\code{\link{ConservationProblem-class}} object.
}
\description{
Add penalties to a conservation problem to favor solutions that clump
selected planning units together into contiguous reserves.
}
\examples{
# create basic problem
p1 <- problem(sim_pu_raster, sim_features) \%>\%
  add_min_set_objective() \%>\%
  add_relative_targets(0.2) \%>\%
  add_default_solver()

# create problem with low boundary penalties
p2 <- p1 \%>\% add_boundary_penalties(5, 1)

# create problem with high boundary penalties
# receive half the penalty as inner edges
p3 <- p1 \%>\% add_boundary_penalties(50, 1)

\donttest{
# solve problems
s <- stack(solve(p1), solve(p2), solve(p3))

# plot solutions
plot(s, main = c("basic solution", "small penalties", "high penalties"))
}

}
\seealso{
\code{\link{constraints}}, \code{\link{penalties}}.
}
