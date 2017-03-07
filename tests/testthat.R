# load packages
library(testthat)
library(prioritizrutils)

# load solver packages
require(gurobi)
require(lpsymphony)
require(Rsymphony)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# run tests
test_check("prioritizrutils")
