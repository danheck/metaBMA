library(metaBMA)
library(testthat)

test_that("meta_ordered() gives correct results", {

  # unconstrained prior on d:
  expect_error(meta_ordered(y, SE, data=dat, d = prior("norm", c(0,1))))


})
