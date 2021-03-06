% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marxan_problem.R
\name{marxan_problem}
\alias{marxan_problem}
\alias{marxan_problem.default}
\alias{marxan_problem.data.frame}
\alias{marxan_problem.character}
\title{Marxan conservation problem}
\usage{
marxan_problem(x, ...)

\method{marxan_problem}{default}(x, features, targets,
  targets_type = c("relative", "absolute"), locked_in = waiver(),
  locked_out = waiver(), penalty = 0, edge_factor = 0.5, ...)

\method{marxan_problem}{data.frame}(x, spec, puvspr, bound = NULL, blm = 0,
  asymmetric_connectivity = FALSE, ...)

\method{marxan_problem}{character}(x, ...)
}
\arguments{
\item{x}{\code{character} file path for Marxan input file (typically
called \code{"input.dat"}), a \code{data.frame} object containing
planning unit data following conventions (ie. 'pu.dat'), or a
\code{\link[sp]{Spatial-class}} or \code{\link[raster]{Raster-class}}
object containing planning unit data.}

\item{...}{not used}

\item{features}{\code{\link[raster]{Raster-class}} object containing
data on the distribution of features across the study area.}

\item{targets}{\code{numeric} amount of each feature to be represented
in the solution.}

\item{targets_type}{\code{character} name indicating if the \code{targets}
are expressed as \code{"relative"} (eg. \code{0.2} meaning that 20 \% of a
feature needs to be conserved), or \code{"absolute"} (eg. \code{200}
meaning that 200 units of a feature need to be conserved) amounts.}

\item{locked_in}{Object that determines which planning units that should be
locked in. See details for more information.}

\item{locked_out}{Object that determines which planning units that should be
locked out. See details for more information.}

\item{penalty}{\code{numeric} penalty for exposed edges. This
is equivalent to the
\href{http://marxan.net/downloads/uq_marxan_web_2/module2.html}{boundary length modifier (BLM)}
parameter in \href{http://marxan.net}{marxan.net}.}

\item{edge_factor}{\code{numeric} proportion to scale edges that do
not have any neighboring planning units. For example, an edge factor
of \code{0.5} is commonly used for planning units along the coast line.}

\item{spec}{\code{data.frame} containing information on the features.
 The argument to \code{spec} must follow the conventions used by
 Marxan. Each row corresponds to a different feature. It must also contain
the following columns:
 \describe{
   \item{\code{"id"}}{\code{integer} unique identifier for each feature
     These identifiers are used in the argument to \code{rij_data}.}
   \item{\code{"name"}}{\code{character} name for each feature.}
   \item{\code{"prop"}}{\code{numeric} relative target for each feature
      (optional).}'
   \item{\code{"amount"}}{\code{numeric} absolute target for each
       feature (optional).}
 }}

\item{puvspr}{\code{data.frame} containing information on the amount of
each feature in each planning unit. Similar to \code{spec}, the
argument to \code{puvspr} must follow the conventions used by
Marxan. It must contain the following columns:
\describe{
  \item{\code{"pu"}}{\code{integer} planning unit identifier.}
  \item{\code{"species"}}{\code{integer} feature identifier.}
  \item{\code{"amount"}}{\code{numeric} amount of the feature in the
    planning unit.}
}}

\item{bound}{puvspr \code{data.frame} containing information on the
planning units' boundaries or the connectivity between planning units.
Similar to \code{spec}, the argument to \code{bound} must follow the
conventions used by Marxan. It must contain the following columns:
\describe{
  \item{\code{"id1"}}{\code{integer} planning unit identifier.}
  \item{\code{"id2"}}{\code{integer} planning unit identifier.}
  \item{\code{"boundary"}}{\code{numeric} length of shared boundary
    between planning units, or the strength of the connectivity between
    the two planning units.}
}}

\item{blm}{\code{numeric} boundary length modifier. This argument only
has an effect when argument to \code{x} is a \code{data.frame}. Defaults
to zero.}

\item{asymmetric_connectivity}{\code{logical} does the argument
to \code{bound} denote asymmetric connectivity between planning units?
This argument only has an effect when argument to \code{bound} is a
\code{data.frame}. Defaults to \code{FALSE}.}
}
\value{
\code{\link{ConservationProblem-class}} object.
}
\description{
Create a Marxan conservation problem. Although users are encouraged to
build and tailor conservation planning problems to suit their own needs,
sometimes it is easier to use a more familiar "canned" approach. This
function provides a convenient wrapper for generating
and solving Marxan-style conservation problems. If users already have
their conservation planning data in the Marxan input format, this function
can also be used to read Marxan data files and solve the Marxan-style
problems using exact algorithm solvers.
}
\examples{

# create Marxan problem using spatial data
data(sim_pu_raster, sim_features)
p1 <- marxan_problem(sim_pu_raster, features=sim_features, targets=0.2,
                     targets_type="relative", penalty=1,
                     edge_factor=0.5)

\donttest{
# solve problem
s1 <- solve(p1)

# show solution
plot(s1)
}

# create marxan problem using marxan input files
input_file <- system.file("extdata/input.dat", package="prioritizrutils")
p2 <- marxan_problem(input_file)

\donttest{
# solve problem
s2 <- solve(p2)

# count number of selected planning units in solution
print(sum(s2))
}

}
