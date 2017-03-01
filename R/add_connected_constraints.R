#' @include internal.R Constraint-proto.R
NULL

#' Add connected constraints
#'
#' Add constraints to a conservation problem to ensure that all selected
#' planning units are spatially connected to each other.
#'
#' @param x \code{\link{ConservationProblem-class}} object.
#'
#' @return \code{\link{ConservationProblem-class}} object with the constraint
#'   added to it.
#'
#' @seealso \code{\link{constraints}} for all the available constraints.
#'
#' @examples
#' \dontrun{
#' # create basic problem
#' p1 <- problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.2) %>%
#'   add_default_solver(time_limit=5)
#'
#' # create problem with added connected constraints
#' p2 <- p1 %>% add_connected_constraints()
#'
#' # solve problems
#' s <- stack(solve(p1), solve(p2))
#'
#' # plot solutions
#' plot(s, main=c("basic solution", "connected solution"))
#' }
#' @export
add_connected_constraints <- function(x) {
  # assert argumnt is valid
  assertthat::assert_that(inherits(x, "ConservationProblem"))
  # check that at least two planning units are touching each other
  stop("TODO: validity checks")
  # add the constraint
  x$add_constraint(pproto(
    "ConnectedConstraint",
    Constraint,
    name = "Connected constraint",
    parameters = parameters(binary_parameter("apply constraint?", 1L)),
    calculate = function(self, x) {
      assertthat::assert_that(inherits(x, "ConservationProblem"))
      if (is.Waiver(x$get_data("connected_matrix"))) {
        # create matrix
        m <- connected_matrix(x$data$cost)
        # manually coerce boundary matrix to "dgCMatrix" class so that
        # elements in the lower diagonal are not filled in
        class(m) <- "dgCMatrix"
        # store data
        x$set_data("connected_matrix", m)
      }
      invisible(TRUE)
    },
    apply = function(self, x, y) {
      assertthat::assert_that(inherits(x, "OptimizationProblem"),
        inherits(y, "ConservationProblem"))
      if (self$parameters$get("Apply constraint?") == 1)
        rcpp_apply_connected_constraints(x$ptr, y$get_data("connected_matrix"))
      invisible(TRUE)
    }))
}
