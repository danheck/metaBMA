library("testthat")
library("metaBMA")
library("rstan")

stopifnot(require("rstan"))

test_check("metaBMA")

# check timing without skip_on_cran() for CRAN submissions:
# Sys.setenv (NOT_CRAN=FALSE)
# test_dir("tests/testthat")
