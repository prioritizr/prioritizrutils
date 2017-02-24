#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_asymmetric_boundary_constraints(SEXP x, 
                                    arma::sp_mat boundary_matrix, 
                                    double penalty, 
                                    double edge_factor) {
  
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();  
  
  // apply penalty and edge scaling
  boundary_matrix *= penalty;
  boundary_matrix.diag() *= edge_factor;
  
  /// extract data from the boundary matrix
  // data to represent connectivities
  std::vector<std::size_t> pu_i;
  pu_i.reserve(boundary_matrix.n_nonzero - ptr->_number_of_planning_units);
  std::vector<std::size_t> pu_j;
  pu_j.reserve(boundary_matrix.n_nonzero - ptr->_number_of_planning_units);
  std::vector<double> pu_b;
  pu_b.reserve(boundary_matrix.n_nonzero - ptr->_number_of_planning_units);
  // data to represent fixed costs
  std::vector<std::size_t> pu_fi;
  pu_i.reserve(ptr->_number_of_planning_units);
  std::vector<double> pu_fb;
  pu_b.reserve(ptr->_number_of_planning_units);
  std::size_t curr_i, curr_j;
  double curr_value;
  for (arma::sp_mat::const_iterator it=boundary_matrix.begin(); 
       it!=boundary_matrix.end();  
       ++it) {
    // get row and column indices for cell
    curr_i = it.row();
    curr_j = it.col();
    curr_value = *it;
    if (curr_i != curr_j) {
      // extract planning unit indices and shared boundaries from the matrix
      pu_i.push_back(curr_i);
      pu_j.push_back(curr_j);
      pu_b.push_back(curr_value);
    } else {
      // extract fixed planning unit costs
      pu_fi.push_back(curr_i);
      pu_fb.push_back(curr_value);
    }
  }
  
  // if the objective is to minimize the costs, then boundary penalties are
  // positive
  if (ptr->_modelsense == "min") {
    // add fixed costs to planning units
    for (std::size_t i=0; i<pu_fi.size(); ++i)
      ptr->_obj[pu_fi[i]] -= pu_fb[i];
    // add connections betweeen planning units to obj
    for (auto i=pu_b.cbegin(); i!=pu_b.cend(); ++i)
      ptr->_obj.push_back((*i));
  }
  
  // if the objective is to maximize the costs, then boundary penalties are
  // negative
  if (ptr->_modelsense == "max") {
    // add fixed connectivity costs from planning units
    for (std::size_t i=0; i<pu_fi.size(); ++i)
      ptr->_obj[pu_fi[i]] += pu_fb[i];
    // add exposed boundaries to obj
    for (auto i=pu_b.cbegin(); i!=pu_b.cend(); ++i)
      ptr->_obj.push_back((*i) * -1.0);
  }
  
  // add lb for new decision variables
  for (auto i=pu_i.cbegin(); i!=pu_i.cend(); ++i)
    ptr->_lb.push_back(0.0);
  // add ub for new decision variables
  for (auto i=pu_i.cbegin(); i!=pu_i.cend(); ++i)
    ptr->_ub.push_back(1.0);
  // add vtype for new decision variables
  for (auto i=pu_i.cbegin(); i!=pu_i.cend(); ++i)
    ptr->_vtype.push_back(ptr->_vtype[0]);
  // add col ids for new decision variables
  for (auto i=pu_i.cbegin(); i!=pu_i.cend(); ++i)
    ptr->_col_ids.push_back("b");
  
  // add new constraints to 
  std::size_t A_row = (A_original_nrow-1);
  for (std::size_t i=0; i<(pu_i.size()); ++i) {
    // increment row
    ++A_row;
    
    // constraint
    // pu_ij <= pu_i
    // pu_ij - pu_i <= 0
    // ie. if pu_ij cannot be 1 is pu_i is 0
    ptr->_A_i.push_back(A_row); 
    ptr->_A_i.push_back(A_row); 
    ptr->_A_j.push_back(A_original_ncol + i); 
    ptr->_A_j.push_back(pu_i[i]);
    ptr->_A_x.push_back(1.0);    
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("b1");

    // increment row
    ++A_row;

    // constraint
    // pu_ij >= pu_i - pu_j
    // pu_ij - pu_i + pu_j >= 0
    // ie. if pu_i is 1 then pu_i_j must be 1
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(pu_i[i]);
    ptr->_A_j.push_back(pu_j[i]);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_A_x.push_back(1.0);
    ptr->_sense.push_back(">=");
    ptr->_rhs.push_back(0);
    ptr->_row_ids.push_back("b2");    
    
    // increment row
    ++A_row;

    // constraint
    // pu_ij <= 2 - pu_i - pu_j
    // pu_ij + pu_i + pu_j <= 2
    // ie. if pu_i and pu _j are both 1 than pu_ij = 0
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(pu_i[i]);
    ptr->_A_j.push_back(pu_j[i]);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(2.0);
    ptr->_row_ids.push_back("b3");
    
  }
  
  // return result
  return true;
}
