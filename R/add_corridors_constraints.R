#' @include internal.R Constraint-proto.R
NULL

#' Add corridors constraint
#'
#' It is important to maintain connectivity between reserves. However,
#' some areas are more difficult for species to traverse then other areas.
#' As a consequence, even though reserves may be connected, species may
#' not be able to move between reserves if the areas connecting them
#' are barriers to dispersal.
#'
#' This function adds constraints to ensure that corridors connect
#' reserves and that individuals from all species can utilise the
#' corridors. Friction \code{\link[raster]{Raster-class}} objects area
#' used to show each dificult areas are to traverse.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @param targets \code{numeric} targets specifying the minimum flow
#'  permitted throughout the solution. If \code{targets} is a single
#'  number then it is used for each feature. Otherwise, if \code{targets} is
#'  a \code{vector} then it specifies the target for each feature.
#'
#' @param friction \code{\link[raster]{RasterStack-class}} object denoting
#'   friction surfaces for each feature. Cells with zero values offer no cost
#'   to dispersal and individuals can move through these with ease. Cells with
#'   higher values are associated with a cost for traversal. Cells with
#'   \code{NA} values are complete barriers to dispersal and individuals
#'   cannot move through these at all.
#'
#' @return \code{\link{ConservationProblem-class}} object.
#'
#' @seealso \code{\link{constraints}}, \code{\link{penalties}}.
#'
#' @examples
#'
#' \dontrun{
#'
#' # create basic problem
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_binary_decision()
#'
#' # create problem with added corridor constraints
#' p2 <- p1 %>% add_corridors_constraints(0.2, sim_friction_raster)
#'
#' # solve problems
#' s <- stack(solve(p1), solve(p2))
#'
#' # plot solutions
#' plot(s, main=c("basic solution", "solution with corridors"))
#'
#' }
#' @export
add_corridors_constraints <- function(x, targets, friction) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, "RasterStack"),
    is.numeric(targets), isTRUE(all(is.finite(targets))),
    isTRUE(all(targets > 0)), length(targets) == raster::nlayers(x),
    isTRUE(all(raster::cellStats(x, "min") > 0)),
    raster::compareRaster(x$features[[1]], friction[[1]], crs = TRUE,
                          res = TRUE, tolerance = 1e-5, stopiffalse = FALSE))
  # create parameters
  p <- parameters(binary_parameter("Apply constraint?", 1),
    proportion_parameter_array("Friction targets", targets, x$feature_names()))
  # create new constraint object
  x$add_constraint(pproto(
    "CorridorsConstraint",
    Constraint,
    name = "Corridors Constraint",
    parameters = parameters(p),
    data = list(friction = friction),
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      stop("TODO: implement apply method for add_corridors_constraints")
    }))
}
