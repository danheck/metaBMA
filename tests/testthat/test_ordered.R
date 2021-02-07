library(testthat)
library(metaBMA)


test_that("meta_ordered() gives correct results", {
  set.seed(12352)
  SE <- runif(20, .3, 1.3)
  dat <- data.frame(y = rnorm(20, 0.4, SE), SE = SE, study = 1:20)
  head(dat)

  # unconstrained prior on d:
  expect_error(meta_ordered(y, SE, data = dat, d = prior("norm", c(0, 1))))

  # mo <- meta_ordered("y", "SE", data=dat, iter = 1000)
  # expect_type(mo$BF, "double")
  # expect_equal(rownames(mo$BF), c("null", "fixed", "ordered", "random"))
  # expect_equal(rownames(mo$estimates), c("fixed", "ordered", "random"))
})
