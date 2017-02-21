#include "prioritizrutils.h"
#include "optimization_problem.h"
 
// [[Rcpp::export]]
bool rcpp_apply_minimum_set_objective(SEXP x, Rcpp::NumericVector targets,
                                      Rcpp::NumericVector costs) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_obj.push_back(costs[i]);
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_sense.push_back(">=");
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_rhs.push_back(targets[i]);
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_col_ids.push_back("pu");
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_row_ids.push_back("spp_target");
  ptr->_modelsense="min";
  return true;
}
 
