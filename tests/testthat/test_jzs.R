library(metBMA)
library(testthat)

set.seed(123452)
SE <- runif(20, .3,1.3)
d <- data.frame(yyy = rnorm(20), SE = SE, study = 1:20)
d$xx <- rnorm(20, d$yyy, 1)
d$cat <- rep(c("a", "b"), 10)


test_that("JZS continuous predictors", {

  fe_xx <- meta_fixed(yyy ~ xx, SE, study, data = d, logml= "s", summ = "s", iter = 6000)
  re_xx <- meta_random(yyy ~ xx, SE, study, data = d, logml= "s", summ = "s", iter = 6000)
  plot_posterior(fe_xx)
  plot_posterior(re_xx)
  expect_silent(bm <- bma(list(fe_xx = fe_xx, re_xx = re_xx), summ="i"))
  expect_silent(plot_posterior(bm))
})

test_that("JZS discrete predictors", {

  fe_xx <- meta_fixed(yyy ~ cat, SE, study, data = d, logml= "s", summ = "s", iter = 6000)
  re_xx <- meta_random(yyy ~ cat, SE, study, data = d, logml= "s", summ = "s", iter = 6000)
  expect_silent(bm <- bma(list(fe_xx, re_xx)))
})

test_that("JZS continuous + discrete predictors", {

  fe_both <- meta_fixed(yyy ~ cat + xx, SE, study, data = d, logml= "s",
                        summ = "s", iter = 6000)
})
