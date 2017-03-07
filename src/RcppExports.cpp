// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// rcpp_new_optimization_problem
SEXP rcpp_new_optimization_problem(std::size_t nrow, std::size_t ncol, std::size_t ncell);
RcppExport SEXP prioritizrutils_rcpp_new_optimization_problem(SEXP nrowSEXP, SEXP ncolSEXP, SEXP ncellSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::size_t >::type nrow(nrowSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type ncol(ncolSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type ncell(ncellSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_new_optimization_problem(nrow, ncol, ncell));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_predefined_optimization_problem
SEXP rcpp_predefined_optimization_problem(Rcpp::List l);
RcppExport SEXP prioritizrutils_rcpp_predefined_optimization_problem(SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_predefined_optimization_problem(l));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_ncol
std::size_t rcpp_get_optimization_problem_ncol(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_ncol(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_ncol(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_nrow
std::size_t rcpp_get_optimization_problem_nrow(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_nrow(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_nrow(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_ncell
std::size_t rcpp_get_optimization_problem_ncell(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_ncell(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_ncell(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_A
Rcpp::List rcpp_get_optimization_problem_A(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_A(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_A(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_modelsense
std::string rcpp_get_optimization_problem_modelsense(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_modelsense(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_modelsense(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_number_of_planning_units
std::size_t rcpp_get_optimization_problem_number_of_planning_units(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_number_of_planning_units(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_number_of_planning_units(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_number_of_features
std::size_t rcpp_get_optimization_problem_number_of_features(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_number_of_features(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_number_of_features(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_vtype
std::vector<std::string> rcpp_get_optimization_problem_vtype(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_vtype(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_vtype(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_obj
std::vector<double> rcpp_get_optimization_problem_obj(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_obj(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_obj(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_rhs
std::vector<double> rcpp_get_optimization_problem_rhs(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_rhs(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_rhs(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_sense
std::vector<std::string> rcpp_get_optimization_problem_sense(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_sense(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_sense(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_lb
std::vector<double> rcpp_get_optimization_problem_lb(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_lb(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_lb(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_ub
std::vector<double> rcpp_get_optimization_problem_ub(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_ub(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_ub(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_col_ids
std::vector<std::string> rcpp_get_optimization_problem_col_ids(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_col_ids(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_col_ids(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_row_ids
std::vector<std::string> rcpp_get_optimization_problem_row_ids(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_row_ids(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_row_ids(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_optimization_problem_compressed_formulation
bool rcpp_get_optimization_problem_compressed_formulation(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_get_optimization_problem_compressed_formulation(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_optimization_problem_compressed_formulation(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_add_rij_data
bool rcpp_add_rij_data(SEXP x, arma::sp_mat rij, bool compressed_formulation);
RcppExport SEXP prioritizrutils_rcpp_add_rij_data(SEXP xSEXP, SEXP rijSEXP, SEXP compressed_formulationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type rij(rijSEXP);
    Rcpp::traits::input_parameter< bool >::type compressed_formulation(compressed_formulationSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_add_rij_data(x, rij, compressed_formulation));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_asymmetric_boundary_constraints
bool rcpp_apply_asymmetric_boundary_constraints(SEXP x, arma::sp_mat boundary_matrix, double penalty, double edge_factor);
RcppExport SEXP prioritizrutils_rcpp_apply_asymmetric_boundary_constraints(SEXP xSEXP, SEXP boundary_matrixSEXP, SEXP penaltySEXP, SEXP edge_factorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type boundary_matrix(boundary_matrixSEXP);
    Rcpp::traits::input_parameter< double >::type penalty(penaltySEXP);
    Rcpp::traits::input_parameter< double >::type edge_factor(edge_factorSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_asymmetric_boundary_constraints(x, boundary_matrix, penalty, edge_factor));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_connected_constraints
bool rcpp_apply_connected_constraints(SEXP x, arma::sp_mat connected_matrix);
RcppExport SEXP prioritizrutils_rcpp_apply_connected_constraints(SEXP xSEXP, SEXP connected_matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type connected_matrix(connected_matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_connected_constraints(x, connected_matrix));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_corridor_constraints
bool rcpp_apply_corridor_constraints(SEXP x, Rcpp::List connected_matrix_list, Rcpp::NumericVector threshold);
RcppExport SEXP prioritizrutils_rcpp_apply_corridor_constraints(SEXP xSEXP, SEXP connected_matrix_listSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type connected_matrix_list(connected_matrix_listSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_corridor_constraints(x, connected_matrix_list, threshold));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_binary_decision
bool rcpp_apply_binary_decision(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_apply_binary_decision(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_binary_decision(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_proportion_decision
bool rcpp_apply_proportion_decision(SEXP x);
RcppExport SEXP prioritizrutils_rcpp_apply_proportion_decision(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_proportion_decision(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_semicontinuous_decision
bool rcpp_apply_semicontinuous_decision(SEXP x, double upper);
RcppExport SEXP prioritizrutils_rcpp_apply_semicontinuous_decision(SEXP xSEXP, SEXP upperSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type upper(upperSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_semicontinuous_decision(x, upper));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_feature_weights
bool rcpp_apply_feature_weights(SEXP x, Rcpp::NumericVector weights);
RcppExport SEXP prioritizrutils_rcpp_apply_feature_weights(SEXP xSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_feature_weights(x, weights));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_locked_in_constraints
bool rcpp_apply_locked_in_constraints(SEXP x, Rcpp::IntegerVector indices);
RcppExport SEXP prioritizrutils_rcpp_apply_locked_in_constraints(SEXP xSEXP, SEXP indicesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type indices(indicesSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_locked_in_constraints(x, indices));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_locked_out_constraints
bool rcpp_apply_locked_out_constraints(SEXP x, Rcpp::IntegerVector indices);
RcppExport SEXP prioritizrutils_rcpp_apply_locked_out_constraints(SEXP xSEXP, SEXP indicesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type indices(indicesSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_locked_out_constraints(x, indices));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_max_cover_objective
bool rcpp_apply_max_cover_objective(SEXP x, Rcpp::NumericVector abundances, Rcpp::NumericVector costs, double budget);
RcppExport SEXP prioritizrutils_rcpp_apply_max_cover_objective(SEXP xSEXP, SEXP abundancesSEXP, SEXP costsSEXP, SEXP budgetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type abundances(abundancesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type costs(costsSEXP);
    Rcpp::traits::input_parameter< double >::type budget(budgetSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_max_cover_objective(x, abundances, costs, budget));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_max_features_objective
bool rcpp_apply_max_features_objective(SEXP x, Rcpp::NumericVector targets, Rcpp::NumericVector costs, double budget);
RcppExport SEXP prioritizrutils_rcpp_apply_max_features_objective(SEXP xSEXP, SEXP targetsSEXP, SEXP costsSEXP, SEXP budgetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type targets(targetsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type costs(costsSEXP);
    Rcpp::traits::input_parameter< double >::type budget(budgetSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_max_features_objective(x, targets, costs, budget));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_max_phylo_objective
bool rcpp_apply_max_phylo_objective(SEXP x, Rcpp::NumericVector targets, Rcpp::NumericVector costs, double budget, arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths);
RcppExport SEXP prioritizrutils_rcpp_apply_max_phylo_objective(SEXP xSEXP, SEXP targetsSEXP, SEXP costsSEXP, SEXP budgetSEXP, SEXP branch_matrixSEXP, SEXP branch_lengthsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type targets(targetsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type costs(costsSEXP);
    Rcpp::traits::input_parameter< double >::type budget(budgetSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type branch_matrix(branch_matrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type branch_lengths(branch_lengthsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_max_phylo_objective(x, targets, costs, budget, branch_matrix, branch_lengths));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_min_set_objective
bool rcpp_apply_min_set_objective(SEXP x, Rcpp::NumericVector targets, Rcpp::NumericVector costs);
RcppExport SEXP prioritizrutils_rcpp_apply_min_set_objective(SEXP xSEXP, SEXP targetsSEXP, SEXP costsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type targets(targetsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type costs(costsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_min_set_objective(x, targets, costs));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_neighbor_constraints
bool rcpp_apply_neighbor_constraints(SEXP x, arma::sp_mat connected_matrix, int k);
RcppExport SEXP prioritizrutils_rcpp_apply_neighbor_constraints(SEXP xSEXP, SEXP connected_matrixSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type connected_matrix(connected_matrixSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_neighbor_constraints(x, connected_matrix, k));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_apply_symmetric_boundary_constraints
bool rcpp_apply_symmetric_boundary_constraints(SEXP x, arma::sp_mat boundary_matrix, double penalty, double edge_factor);
RcppExport SEXP prioritizrutils_rcpp_apply_symmetric_boundary_constraints(SEXP xSEXP, SEXP boundary_matrixSEXP, SEXP penaltySEXP, SEXP edge_factorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type boundary_matrix(boundary_matrixSEXP);
    Rcpp::traits::input_parameter< double >::type penalty(penaltySEXP);
    Rcpp::traits::input_parameter< double >::type edge_factor(edge_factorSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_apply_symmetric_boundary_constraints(x, boundary_matrix, penalty, edge_factor));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_boundary_data
Rcpp::List rcpp_boundary_data(Rcpp::DataFrame data, double tolerance);
RcppExport SEXP prioritizrutils_rcpp_boundary_data(SEXP dataSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_boundary_data(data, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_branch_matrix
arma::sp_mat rcpp_branch_matrix(Rcpp::List x);
RcppExport SEXP prioritizrutils_rcpp_branch_matrix(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_branch_matrix(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_list_to_matrix_indices
Rcpp::List rcpp_list_to_matrix_indices(Rcpp::List x, std::size_t n_preallocate);
RcppExport SEXP prioritizrutils_rcpp_list_to_matrix_indices(SEXP xSEXP, SEXP n_preallocateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n_preallocate(n_preallocateSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_list_to_matrix_indices(x, n_preallocate));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sp_to_polyset
Rcpp::DataFrame rcpp_sp_to_polyset(Rcpp::List x, std::string slot, std::size_t n_preallocate);
RcppExport SEXP prioritizrutils_rcpp_sp_to_polyset(SEXP xSEXP, SEXP slotSEXP, SEXP n_preallocateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type slot(slotSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n_preallocate(n_preallocateSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sp_to_polyset(x, slot, n_preallocate));
    return rcpp_result_gen;
END_RCPP
}
